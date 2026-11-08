# =============================================================================
# TAF (Terminal Aerodrome Forecast) raw-text parser
# -----------------------------------------------------------------------------
# Purpose
#   Read raw TAF bulletin text files for a date range, split them into
#   individual TAF reports, parse each report's header (station, issue time,
#   validity period), break the forecast body into time segments
#   (BASE / FM / BECMG / TEMPO / PROBxx), extract METAR-style weather fields
#   from each segment, and give every segment a UTC start and end time.
#
# Input
#   Text files stored as <root>/YYYY/MM/DD/*.txt. Each file holds many TAFs,
#   separated by blank lines. One TAF ("block") looks like this:
#
#     2025/11/30 00:00                                         <- block timestamp line
#     TAF SVMI 302200Z 0100/0200 09004KT 9999 SCT016 ...       <- header line + base forecast
#           TEMPO 0100/0204 09003KT 9000 -DZ SCT013 BKN040     <- change group (continuation line)
#          FM010500 00000KT 9999 SCT013 SCT060                 <- change group (continuation line)
#
# Output
#   taf_segs: a long table with ONE ROW PER FORECAST SEGMENT
#   (several rows per TAF).
#
# Time codes used in TAFs (all UTC, with no month or year):
#   DDHHMMZ    issue time, e.g. 302200Z = day 30, 22:00Z
#   DDHH/DDHH  validity period, e.g. 0100/0200 = day 01 00Z to day 02 00Z
#              (hour "24" is allowed, e.g. 0124 = end of day 01)
#   FMDDHHMM   "from" time, e.g. FM010500 = from day 01, 05:00Z
# The month and year are taken from the block timestamp line (see Step 3).
# =============================================================================
### Code provided by: Jasmine Siyu Wu; Edited & annotated by: Sicheng Guo


install.packages("lubridate") # Run if not already installed
library(lubridate)
library(data.table) # required for the first step to parse the raw data
library(stringr) # required for the first step to parse the raw data

# ---- Settings ---------------------------------------------------------------
# root:           top folder holding the YYYY/MM/DD sub-folders of raw TAF files
# keep_stations:  ICAO station codes of interest (used for the optional subset in Step 3)
# start/end_utc:  date range to read; every calendar day in this range is loaded
# taf_parsed_dir: output CSV path (relative to R's working directory)

root <- "D:/r_projects/research/TAF_Parse"  # change to your path
keep_stations <- c("KDFW", "KDAL") # optional
start_utc <- ymd_hms("2025-12-01 00:00:00", tz = "UTC") # Need to install lubridate library
end_utc   <- ymd_hms("2025-12-01 23:59:59", tz = "UTC")
taf_parsed_dir <- "Output_TAF_DiversionEvent/taf_20251201_parsed.csv"


# =============================================================================
# 0) Create a list of TAF files to read
# -----------------------------------------------------------------------------
# Build one folder path per day in [start_utc, end_utc], following the
# <root>/YYYY/MM/DD layout. Skip days whose folder does not exist, then list
# every .txt file inside the remaining folders (sub-folders included).
# Note: a file for 2025-12-01 00Z can contain TAFs issued late on 2025-11-30;
# their block timestamps therefore show 2025/11/30.
# =============================================================================

days_needed <- seq.Date(as.Date(start_utc), as.Date(end_utc), by = "day")
days_needed

day_dirs <- file.path(
  root,
  format(days_needed, "%Y"),
  format(days_needed, "%m"),
  format(days_needed, "%d")
) #D:/r_projects/research/TAF_Parse/2025/12/01


# Keep only dirs that exist
day_dirs <- day_dirs[dir.exists(day_dirs)]

taf_files <- unlist(lapply(day_dirs, function(d) {
  list.files(d, pattern = "\\.txt$", 
             recursive = TRUE, 
             full.names = TRUE)
}), use.names = FALSE)


length(taf_files)
head(taf_files, 5)


# =============================================================================
# 1) Split each file into TAF "blocks" (records)
# -----------------------------------------------------------------------------
# A blank line marks the end of one TAF. The function:
#   1. reads all lines, replaces tabs with spaces, trims trailing spaces;
#   2. flags blank lines;
#   3. gives each line a block_id with cumsum(is_blank): the counter goes up
#      by 1 at every blank line, so all lines between two blank lines share
#      the same id;
#   4. drops the blank lines and joins each block's lines into one string,
#      KEEPING the "\n" line breaks.
# The line breaks matter: Step 2 uses them to tell the timestamp line, the
# header line and the change-group lines apart.
# Result: one row per TAF with columns source_file, block_id, raw_block.
# block_id is unique only within a file, so (source_file, block_id) is the key.
# =============================================================================

read_taf_blocks <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  x <- str_replace_all(x, "\t", " ")
  x <- str_trim(x, side = "right")
  
  # Identify blank lines
  is_blank <- str_detect(x, "^\\s*$")
  
  # Block id increments after a blank line
  block_id <- cumsum(is_blank) + 1L
  
  # Keep nonblank lines with block_id
  dt <- data.table(
    source_file = file,
    block_id = block_id[!is_blank],
    line = x[!is_blank]
  )
  
  # Collapse lines within each block into a single string (preserve line breaks)
  blocks <- dt[, .(raw_block = paste(line, collapse = "\n")), by = .(source_file, block_id)]
  blocks[]
}

# Run the splitter on every file and stack the results into one table.
taf_blocks <- rbindlist(lapply(taf_files, read_taf_blocks), use.names = TRUE, fill = TRUE)

# Do NOT flatten raw_block here. str_squish() turns "\n" into a space, which
# merges the timestamp and header lines; Step 2 then deletes the whole TAF
# together with the timestamp line and returns NA for every field.

# taf_blocks[, raw_block := str_squish(str_replace_all(raw_block, "\n", " \n "))] ## comment out this line or step 2 will return nulls

taf_blocks[1, raw_block]
## "2025/11/30 00:00 TAF SVMI 302200Z 0100/0200 09004KT 9999 SCT016 TX34/0118Z TN23/0108Z TEMPO 0100/0204 09003KT 9000 -DZ SCT013 BKN040 FM010500 00000KT 9999 SCT013 SCT060"



# =============================================================================
# 2) Parse each TAF block into header fields + segment lines
# -----------------------------------------------------------------------------
# Input:  one raw_block string (multi-line).
# Output: a named list that becomes one row of taf_parsed:
#   block_ts_chr   timestamp line text, e.g. "2025/11/30 00:00"
#   station        4-letter ICAO code, e.g. "KDFW"
#   issue_z        issue time code DDHHMMZ, e.g. "302200Z"
#   valid_from     validity start DDHH, e.g. "0100"
#   valid_to       validity end   DDHH, e.g. "0200"
#   is_amd         TRUE if the header line reads "TAF AMD ..." (amended forecast)
#   header_rest    rest of the header line after the validity period
#                  = the BASE (prevailing) forecast conditions
#   segments_text  all lines after the header line (change groups), "\n"-separated
#   taf_text       the whole TAF (without the timestamp line) on one line
# =============================================================================
parse_taf_block <- function(raw_block) {
  # Split the block back into its lines and clean them
  lines <- readLines(textConnection(raw_block), warn = FALSE)
  lines <- gsub("\t", " ", lines)
  lines <- trimws(lines, which = "right")
  
  # drop empty lines
  lines <- lines[!grepl("^\\s*$", lines)]
  
  # optional block timestamp line like "2024/11/04 00:00"
  # If the first line starts with YYYY/MM/DD HH:MM, keep just the date-time
  # part (anything after it, e.g. "Correction" or "Ammendment", is dropped)
  # and remove that line, so the TAF itself starts at lines[1].
  
  block_ts_chr <- NA_character_
  if (length(lines) > 0 && grepl("^\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}\\b", lines[1])) {
    block_ts_chr <- sub("^\\s*(\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}).*$", "\\1", lines[1])
    lines <- lines[-1]
  }
  
  # Find the header line: the first line that starts with the word "TAF".
  # If there is none, return a row with NA header fields but keep the text
  # (taf_text) so these blocks can be inspected later.
  
  # Note: about 3,100 of 10,744 TAFs (~29%) end up with NA station,
  #   issue_z, valid_from and valid_to. They either have no line starting
  #   with "TAF" (caught here) or fail the header regex below. Layouts seen:
  #     ~1,700  "TAF" alone on its line; the rest is on the NEXT line:
  #               TAF
  #                     AMD KGJT 302231Z 3023/0118 31005KT ...
  #       ~500  repeated keyword:        "TAF TAF SBGR 302200Z ..."
  #       ~155  correction:              "TAF COR SCBA 302208Z ..."
  #               (also "TAF COR TAF COR SCBA ...")
  #   a few 100 no "TAF" keyword at all: "KRME 302237Z 3023/0118 ..."
  #                                      "FBMN 302200Z 0100/0124 ..."
  #          5  multi-part bulletin:     "PART 1 OF 2 TAF ..."
  #   Many US stations (K***) use the first and last layouts, so KDFW/KDAL
  #   TAFs can be among the NA rows.
  
  hdr_idx <- which(grepl("^\\s*TAF\\b", lines))[1]
  if (is.na(hdr_idx)) {
    return(list(
      block_ts_chr = block_ts_chr,
      station = NA_character_, issue_z = NA_character_,
      valid_from = NA_character_, valid_to = NA_character_,
      is_amd = FALSE,
      header_rest = NA_character_,
      segments_text = NA_character_,
      taf_text = paste(lines, collapse = " ")
    ))
  }
  
  header_line <- lines[hdr_idx]
  
  # Parse header: TAF (AMD) STN DDHHMMZ DDHH/DDHH [rest...]
  # Capture groups returned by str_match (column 1 is the full match):
  #   m[,2]  "AMD " if present, otherwise NA  -> is_amd
  #   m[,3]  station code (4 letters/digits)  -> station
  #   m[,4]  issue time DDHHMMZ               -> issue_z
  #   m[,5]  validity DDHH/DDHH               -> split into valid_from / valid_to
  #   m[,6]  everything after the validity    -> header_rest (BASE conditions)
  # If the line does not fit this pattern, all groups are NA.
  
  m <- str_match(header_line,
                 "^\\s*TAF\\s+(AMD\\s+)?([A-Z0-9]{4})\\s+(\\d{6}Z)\\s+(\\d{4}/\\d{4})\\s*(.*)$")
  is_amd <- !is.na(m[,2])
  station <- m[,3]
  issue_z <- m[,4]
  valid <- m[,5]
  header_rest <- m[,6]
  
  # "0100/0200" -> valid_from = "0100" (chars 1-4), valid_to = "0200" (chars 6-9)
  valid_from <- ifelse(!is.na(valid), substr(valid, 1, 4), NA_character_)
  valid_to   <- ifelse(!is.na(valid), substr(valid, 6, 9), NA_character_)
  
  # Everything AFTER the header line are segments (BECMG/TEMPO/FM/PROB etc.)
  # Each continuation line normally holds one change group; they are kept
  # as one "\n"-separated string and split into rows in Step 4.
  seg_lines <- if (hdr_idx < length(lines)) lines[(hdr_idx + 1):length(lines)] else character(0)
  
  segments_text <- if (length(seg_lines) == 0) NA_character_ else paste(seg_lines, collapse = "\n")
  
  list(
    block_ts_chr = block_ts_chr,
    station = station,
    issue_z = issue_z,
    valid_from = valid_from,
    valid_to = valid_to,
    is_amd = is_amd,
    header_rest = header_rest,
    segments_text = segments_text,
    taf_text = paste(lines, collapse = " ")
  )
}

# Apply the parser to every block. Grouping by (source_file, block_id) runs
# the function once per TAF; as.list() turns the returned list into columns.
# Result: taf_parsed, one row per TAF.
taf_parsed <- taf_blocks[, as.list(parse_taf_block(raw_block)), by = .(source_file, block_id)]

taf_parsed[, block_ts_utc := ymd_hm(block_ts_chr, tz = "UTC")]

# =============================================================================
# 3) Subset to relevant stations (DFW/DAL) and convert TAF time codes to UTC
# =============================================================================
## KEEP the code structure. Here we don't make subset for data dictionary purpose.

#taf_sub <- taf_parsed[station %in% keep_stations]
taf_sub <- taf_parsed

# Quick checks: number of TAFs per station, and the first parsed row
taf_sub[, .N, by = station][order(station)]
taf_sub[1, .(station, block_ts_chr, issue_z, valid_from, valid_to, header_rest, segments_text)]


# Add: definition of functions
# -----------------------------------------------------------------------------
# taf_ddhhmm_to_utc(): turn day / hour / minute numbers into full UTC date-times.
# TAF codes give only the day of month, so the month must be inferred.
# Method: build three candidate date-times using the reference month
# (month of ref_utc) minus 1, the same month, and plus 1, then keep the
# candidate closest to ref_utc. This handles month and year changes, e.g.
# reference 2025-11-30 with day "01" -> 2025-12-01 (not 2025-11-01).
#   - ISOdatetime() returns NA for impossible dates (e.g. day 31 in November),
#     so such candidates are never picked.
#   - Hours and minutes are added as seconds, so hour 24 rolls over to
#     00:00 of the next day (e.g. "0124" -> day 02 00:00).
#   - If no candidate is valid (or ref_utc is NA), the result is NA.
# All arguments can be vectors; the function works on whole columns at once.
# -----------------------------------------------------------------------------

taf_ddhhmm_to_utc <- function(dd, hh, mi, ref_utc) {
  n <- length(dd)
  ref_month <- floor_date(ref_utc, "month")
  cand <- matrix(NA_real_, nrow = n, ncol = 3)
  for (j in 1:3) {                                  # previous, same, next month
    m0   <- ref_month %m+% months(j - 2)
    base <- ISOdatetime(year(m0), month(m0), dd, 0, 0, 0, tz = "UTC")  # NA if day invalid
    cand[, j] <- as.numeric(base) + hh * 3600 + mi * 60               # handles hour 24
  }
  # Distance (seconds) from each candidate to the reference time;
  # invalid candidates get Inf so they lose.
  d <- abs(cand - as.numeric(ref_utc))
  d[is.na(d)] <- Inf
  # For each row, pick the column (month) with the smallest distance
  best <- max.col(-d, ties.method = "first")
  out  <- cand[cbind(seq_len(n), best)]
  out[!is.finite(d[cbind(seq_len(n), best)])] <- NA
  as.POSIXct(out, origin = "1970-01-01", tz = "UTC")
}

# Issue time code DDHHMMZ -> UTC date-time.
# "302200Z" -> 2025-11-30 22:00 UTC
issuez_to_utc <- function(issue_z, ref_utc) {
  taf_ddhhmm_to_utc(as.integer(substr(issue_z, 1, 2)),
                    as.integer(substr(issue_z, 3, 4)),
                    as.integer(substr(issue_z, 5, 6)), ref_utc)
}

# Validity / change-group code DDHH -> UTC date-time (minutes = 0).
# "0100" -> 2025-12-01 00:00 UTC ; "0124" -> 2025-12-02 00:00 UTC
ddhh_to_utc <- function(ddhh, ref_utc) {
  taf_ddhhmm_to_utc(as.integer(substr(ddhh, 1, 2)),
                    as.integer(substr(ddhh, 3, 4)), 0L, ref_utc)
}

# Convert the header time codes of every TAF to UTC, using the block
# timestamp as the reference:
#   issue_time_utc       when the forecast was issued
#   taf_valid_start_utc  start of the forecast period
#   taf_valid_end_utc    end of the forecast period
taf_sub[, issue_time_utc      := issuez_to_utc(issue_z, block_ts_utc)]
taf_sub[, taf_valid_start_utc := ddhh_to_utc(valid_from, block_ts_utc)]
taf_sub[, taf_valid_end_utc   := ddhh_to_utc(valid_to,   block_ts_utc)]


# taf_sub[, issue_time_utc :=
#           as.POSIXct(
#             mapply(issuez_to_utc, issue_z, block_ts_utc),
#             origin = "1970-01-01", tz = "UTC"
#           )
# ]
# 
# taf_sub[, taf_valid_start_utc :=
#           as.POSIXct(
#             mapply(ddhh_to_utc, valid_from, block_ts_utc),
#             origin = "1970-01-01", tz = "UTC"
#           )
# ]
# 
# taf_sub[, taf_valid_end_utc :=
#           as.POSIXct(
#             mapply(ddhh_to_utc, valid_to, block_ts_utc),
#             origin = "1970-01-01", tz = "UTC"
#           )
# ]


# handle cross-midnight validity
# Safety net: if the end is not after the start, push the end one day later.
# With the closest-month logic above this should rarely or never trigger.
taf_sub[
  !is.na(taf_valid_end_utc) & !is.na(taf_valid_start_utc) &
    taf_valid_end_utc <= taf_valid_start_utc,
  taf_valid_end_utc := taf_valid_end_utc + days(1)
]

# # valid_to is DDHH, and it can be next day; compute and if <= start, add 1 day
# taf_sub[, taf_valid_end_utc := ddhh_to_utc(valid_to, block_ts_utc)]
# taf_sub[!is.na(taf_valid_end_utc) & !is.na(taf_valid_start_utc) & taf_valid_end_utc <= taf_valid_start_utc,
#         taf_valid_end_utc := taf_valid_end_utc + days(1)]


# =============================================================================
# 4) Build a segments table that includes BASE + continuation lines
# -----------------------------------------------------------------------------
# Reshape from one row per TAF to one row per forecast segment:
#   - BASE segment  = conditions on the header line (header_rest), seg_order 0
#   - other segments = each continuation line, seg_order 1, 2, 3 ... in the
#     order they appear in the TAF
# Each row carries the TAF identifiers (source_file, block_id, station, ...)
# so segments can be linked back to their TAF.
# =============================================================================
# 1. BASE segment: one row per TAF issuance

taf_base <- taf_sub[, .(
  source_file, block_id, station, block_ts_utc,
  issue_z, valid_from, valid_to, is_amd,
  seg_order = 0L,
  seg_type  = "BASE",
  seg_line  = header_rest
)]

# 2. Other segments: one row per continuation line
# str_split() breaks segments_text at "\n"; unlist() + by= creates one row
# per line while repeating the TAF identifier columns.
taf_cont <- taf_sub[
  !is.na(segments_text) & segments_text != "",
  .(seg_line = unlist(str_split(segments_text, "\\n"))),
  by = .(source_file, block_id, station, block_ts_utc, issue_z, valid_from, valid_to, is_amd)
]

# Remove leading/extra spaces (continuation lines are indented) and empty lines
taf_cont[, seg_line := str_squish(seg_line)]
taf_cont <- taf_cont[seg_line != "" & !is.na(seg_line)]

# preserve within-TAF line order
taf_cont[, seg_order := seq_len(.N), by = .(source_file, block_id)]

# Combine
taf_segs <- rbindlist(list(taf_base, taf_cont), use.names = TRUE, fill = TRUE)
setorder(taf_segs, source_file, block_id, seg_order)


# =============================================================================
# 5) Classify each segment and extract its timing codes
# -----------------------------------------------------------------------------
# Change-group types (from the first word of the line):
#   FMddhhmm  FROM: conditions change completely at this time and last until
#             the next FM group (or the end of the TAF)
#   BECMG     BECOMING: gradual change during DDHH/DDHH; the new conditions
#             then continue afterwards
#   TEMPO     TEMPORARY: short fluctuations within DDHH/DDHH; the main
#             conditions continue outside them
#   PROBxx    probability (30 or 40 %) of the conditions
#   BASE      header conditions, or any line that does not start with one of
#             the keywords above (e.g. "AMD KGJT ..." in amended TAFs, or
#             "AMDS AFT ..." remarks), which are also labelled BASE here
# =============================================================================
# 5) Parse the segment lines into a long “TAF segments” table
# seg_type for FM should be just "FM" (not "FM041700")
taf_segs[, seg_type := fifelse(str_detect(seg_line, "^FM\\d{6}\\b"), "FM",
                               str_extract(seg_line, "^(BECMG|TEMPO|PROB\\d{2})"))]
taf_segs[is.na(seg_type), seg_type := "BASE"]

# BECMG/TEMPO window (DDHH/DDHH)
# The lookbehind (?<=...) finds the DDHH/DDHH that directly follows the
# BECMG/TEMPO keyword, e.g. "TEMPO 0100/0204 ..." -> "0100/0204".
taf_segs[, seg_valid := fifelse(
  seg_type %in% c("BECMG","TEMPO"),
  str_extract(seg_line, "(?<=^(?:BECMG|TEMPO)\\s)\\d{4}/\\d{4}"),
  NA_character_
)]
# Split the window into start (seg_from) and end (seg_to) DDHH codes
taf_segs[, `:=`(
  seg_from = ifelse(!is.na(seg_valid), str_sub(seg_valid, 1, 4), NA_character_),
  seg_to   = ifelse(!is.na(seg_valid), str_sub(seg_valid, 6, 9), NA_character_)
)]

# FM start time (DDHHMM) from FMDDHHMM
# e.g. "FM010500 ..." -> fm_from = "010500"
taf_segs[, fm_from := fifelse(
  seg_type == "FM",
  str_extract(seg_line, "(?<=^FM)\\d{6}"),
  NA_character_
)]

# For FM, set seg_from = DDHH (hour-level) and keep minutes separately
taf_segs[seg_type == "FM", `:=`(
  seg_from = str_sub(fm_from, 1, 4),      # DDHH
  seg_to   = NA_character_,
  fm_min   = as.integer(str_sub(fm_from, 5, 6))  # MM
)]

# seg_body = the weather part of the segment, without the type keyword and
# time code. Weather fields in Step 6 are read from seg_body so that time
# codes (e.g. "0100/0204") are not mistaken for weather values.
taf_segs[, seg_body := seg_line]

# Remove the segment header tokens
taf_segs[seg_type %in% c("BECMG","TEMPO"),
         seg_body := str_remove(seg_body, "^(BECMG|TEMPO)\\s+\\d{4}/\\d{4}\\s+")]
taf_segs[seg_type == "FM",
         seg_body := str_remove(seg_body, "^FM\\d{6}\\s+")]
taf_segs[str_detect(seg_type, "^PROB"),
         seg_body := str_remove(seg_body, "^PROB\\d{2}\\s+")]
taf_segs[seg_type == "BASE",
         seg_body := seg_body]  # no-op


# =============================================================================
# 6) Minimal "same fields as METAR" parsing for each TAF segment
# -----------------------------------------------------------------------------
# Extract wind, visibility, clouds/ceiling, thunderstorm/rain flags and flight
# category from seg_body, using the same codes as METAR reports. Each field
# describes the forecast conditions of that segment only. A field is NA when
# the segment does not mention it (e.g. a TEMPO group that changes only the
# visibility has no wind).
# =============================================================================
# Wind token: dddffKT or dddffGggKT or VRB
#   ddd = direction in degrees (or VRB = variable), ff = speed in knots,
#   Ggg = gust speed in knots. e.g. "27015G25KT" = 270 deg, 15 kt, gusts 25 kt

taf_segs[, fcst_wind_token := str_extract(seg_body, "\\b(?:VRB|\\d{3})\\d{2,3}(?:G\\d{2,3})?KT\\b")]
# Direction: first 3 characters; NA when variable (VRB)
taf_segs[, fcst_wind_dir := fifelse(!is.na(fcst_wind_token) & str_starts(fcst_wind_token, "VRB"),
                                    NA_integer_, as.integer(substr(fcst_wind_token, 1, 3)))]
# Speed: the 2-3 digits right after the direction
taf_segs[, fcst_wind_spd_kt := as.integer(str_extract(fcst_wind_token, "(?<=^(?:VRB|\\d{3}))\\d{2,3}"))]
# Gust: digits after "G" (NA if no gust)
taf_segs[, fcst_wind_gust_kt := as.integer(str_remove(str_extract(fcst_wind_token, "G\\d{2,3}"), "G"))]

# Visibility: meters "9999" or "####" or US "P6SM"/"6SM"
taf_segs[, fcst_vis_m := as.integer(str_extract(seg_body, "\\b\\d{4}\\b"))]  # 9999 or 5000 etc (coarse)
taf_segs[, fcst_vis_sm := as.numeric(str_remove(str_extract(seg_body, "\\bP?\\d+SM\\b"), "SM"))]
taf_segs[str_detect(seg_body, "\\bP6SM\\b"), fcst_vis_sm := 6]  # treat P6SM as 6+ (cap at 6)

# Clouds: FEW/SCT/BKN/OVC/VV### plus CB/TCU
taf_segs[, cloud_tokens := str_extract_all(seg_body, "\\b(?:FEW|SCT|BKN|OVC|VV)\\d{3}(?:CB|TCU)?\\b")]

# Ceiling = min(BKN/OVC/VV)
get_ceiling_ft <- function(tokens) {
  if (length(tokens) == 0) return(NA_integer_)
  types <- substr(tokens, 1, 3)
  h <- as.integer(substr(tokens, 4, 6)) * 100L
  cand <- h[types %in% c("BKN","OVC","VV")]
  if (length(cand) == 0) NA_integer_ else min(cand, na.rm = TRUE)
}
taf_segs[, fcst_ceiling_ft := vapply(cloud_tokens, get_ceiling_ft, integer(1))]

# Convective flags (basic)
taf_segs[, fcst_vcts := str_detect(seg_body, "\\bVCTS\\b")]
taf_segs[, fcst_ts   := str_detect(seg_body, "\\bTS\\b|\\bTSRA\\b|\\bVCTS\\b")]
taf_segs[, fcst_tsra := str_detect(seg_body, "\\bTSRA\\b")]
taf_segs[, fcst_ra   := str_detect(seg_body, "\\bRA\\b|\\bSHRA\\b")]
taf_segs[, fcst_shra := str_detect(seg_body, "\\bSHRA\\b")]

# Segment flight category (if you want it at segment level)
taf_segs[, fcst_flight_cat := fifelse(
  (!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 500) | (!is.na(fcst_vis_sm) & fcst_vis_sm < 1), "LIFR",
  fifelse((!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 1000) | (!is.na(fcst_vis_sm) & fcst_vis_sm < 3), "IFR",
          fifelse((!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 3000) | (!is.na(fcst_vis_sm) & fcst_vis_sm <= 5), "MVFR", "VFR")
  )
)]

# =============================================================================
# 7) Derive seg_start_utc / seg_end_utc for every segment (BASE, TEMPO, BECMG, FM)
# -----------------------------------------------------------------------------
# Rules used:
#   BASE         start = TAF validity start
#   TEMPO/BECMG  start/end = their own DDHH/DDHH window
#   FM           start = FMddhhmm time
#   Any segment still without an end (BASE, FM, PROB) ends where the next
#   segment of the same TAF starts, or at the TAF validity end if it is last.
# =============================================================================
# Join issuance timing info onto segments
# (adds issue_time_utc, taf_valid_start_utc, taf_valid_end_utc to every
# segment row of the same TAF)

taf_segs <- merge(
  taf_segs,
  taf_sub[, .(source_file, block_id, station, issue_time_utc, 
              taf_valid_start_utc, taf_valid_end_utc)],
  by = c("source_file", "block_id", "station"),
  all.x = TRUE
)

# Initialize
taf_segs[, `:=`(seg_start_utc = as.POSIXct(NA), 
                seg_end_utc = as.POSIXct(NA))]

# BASE starts at TAF valid start
taf_segs[seg_type == "BASE", seg_start_utc := taf_valid_start_utc]

# TEMPO/BECMG seg_start_utc from seg_from (DDHH)
# Converted one TAF at a time (by = source_file, block_id), using that TAF's
# validity start as the reference for the month.
taf_segs[
  seg_type %in% c("TEMPO","BECMG") & !is.na(seg_from) & seg_from != "",
  seg_start_utc := as.POSIXct(
    vapply(seg_from, function(x) as.numeric(ddhh_to_utc(x, taf_valid_start_utc[1])), numeric(1)),
    origin = "1970-01-01", tz = "UTC"
  ),
  by = .(source_file, block_id)
]

# TEMPO/BECMG seg_end_utc from seg_to (DDHH)
taf_segs[
  seg_type %in% c("TEMPO","BECMG") & !is.na(seg_to) & seg_to != "",
  seg_end_utc := as.POSIXct(
    vapply(seg_to, function(x) as.numeric(ddhh_to_utc(x, taf_valid_start_utc[1])), numeric(1)),
    origin = "1970-01-01", tz = "UTC"
  ),
  by = .(source_file, block_id)
]


# If TEMPO/BECMG end <= start, add 1 day (cross-midnight)
# Safety net, same idea as for the TAF validity in Step 3.
taf_segs[seg_type %in% c("TEMPO","BECMG") &
           !is.na(seg_start_utc) & !is.na(seg_end_utc) & seg_end_utc <= seg_start_utc,
         seg_end_utc := seg_end_utc + days(1)]


# FM start uses fm_from (DDHHMM)
taf_segs[
  seg_type == "FM" & !is.na(fm_from) & fm_from != "",
  seg_start_utc := as.POSIXct(
    vapply(fm_from, ddhhmm_to_utc, as.POSIXct(NA), anchor_time_utc = taf_valid_start_utc[1]),
    tz = "UTC", origin = "1970-01-01"
  ),
  by = .(source_file, block_id)
]

# Make sure the new time columns display in UTC (not the computer's local time zone)
attr(taf_segs$seg_start_utc, "tzone") <- "UTC"
attr(taf_segs$seg_end_utc, "tzone") <- "UTC"


# Order within issuance
# Sort each TAF's segments by start time, then find the start time of the
# next segment (shift "lead" = value from the following row, same TAF only).
setorder(taf_segs, source_file, block_id, seg_start_utc, seg_order)
taf_segs[, next_start := shift(seg_start_utc, type = "lead"), 
         by = .(source_file, block_id)]
attr(taf_segs$next_start, "tzone") <- "UTC"


# Fill missing ends
# 1st: end = start of the next segment; 2nd: if it is the last segment,
# end = TAF validity end.
# Simplification: "next segment" includes TEMPO/BECMG groups, so a BASE or
# FM segment is cut off when the first TEMPO/BECMG starts, even though in
# TAF meaning the main conditions continue around a TEMPO period.

taf_segs[is.na(seg_end_utc), seg_end_utc := next_start]
taf_segs[is.na(seg_end_utc), seg_end_utc := taf_valid_end_utc]

# # Clip to TAF validity window 
# taf_segs[!is.na(seg_start_utc) & seg_start_utc < taf_valid_start_utc, 
#          seg_start_utc := taf_valid_start_utc]
# taf_segs[!is.na(seg_end_utc) & seg_end_utc > taf_valid_end_utc, 
#          seg_end_utc := taf_valid_end_utc]

# Drop degenerate segments
# taf_segs <- taf_segs[!is.na(seg_start_utc) & !is.na(seg_end_utc) & seg_end_utc >= seg_start_utc]


# Save parsed file
fwrite(taf_segs, taf_parsed_dir)
