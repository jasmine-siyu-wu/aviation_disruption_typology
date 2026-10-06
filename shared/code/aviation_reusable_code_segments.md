# Reusable Aviation Code Segments

These excerpts are organized from the supplied project files. They preserve the procedural workflow rather than converting everything into functions.

## A. METAR / TAF reading and parsing (R)

### A1. METAR: read files for one event/window
Source: `Diversion_Weather_NewDates.rmd` by Jasmine Wu, chunk “event 2 metar data reading”.

Reads raw METAR files for a specified airport and date range, extracts the relevant observations, and combines them into a single dataset for one event or analysis window.

```r
# UPDATE these commands before running this chunk
root_dir <- "../../Data/Weather/METAR_SDS_2024/2024"
event_start <- ymd_hms("2024-11-04 00:00:00", tz = "UTC")
event_end   <- ymd_hms("2024-11-06 00:00:00", tz = "UTC")
stations_keep <- c("KDFW", "KDAL", "DFW", "DAL")
out_file <- "Output_METAR_DiversionEvent/metar_DFW_DAL_20241104_20241105.csv"


# Create a list of files to read
all_files <- list.files(
  path = root_dir,
  pattern = "^MTRR_NOAA_\\d{8}_\\d{6}_3600\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

parse_file_time <- function(fp) {
  fn <- basename(fp)
  m <- str_match(fn, "^MTRR_NOAA_(\\d{8})_(\\d{6})_3600\\.csv$")
  if (any(is.na(m))) return(NA_real_)
  ymd_hms(paste0(m[2], " ", m[3]), tz = "UTC")
}

file_times <- as.POSIXct(vapply(all_files, parse_file_time, as.POSIXct(NA)), origin = "1970-01-01", tz = "UTC")
files_in_window <- all_files[file_times >= event_start & file_times < event_end]

if (length(files_in_window) == 0) stop("No files found in the event window.")

kept <- vector("list", length(files_in_window))
k <- 0L

for (fp in files_in_window) {
  dt <- fread(fp, showProgress = FALSE, na.strings = c("*NULL*", "NULL", ""))
  dt <- dt[MtrStnId %in% stations_keep]
  if (nrow(dt) > 0) {
    dt[, source_file := basename(fp)]
    k <- k + 1L
    kept[[k]] <- dt
  }
}

metar_sub <- rbindlist(kept[seq_len(k)], fill = TRUE)


# final time filter + order
# metar_sub <- metar_sub[ MtrRecDateTime >= event_start & MtrRecDateTime < event_end]
setorder(metar_sub, MtrStnId, MtrRecDateTime)

fwrite(metar_sub, out_file)
cat("Wrote:", out_file, 
    "\nRows:", nrow(metar_sub), 
    "\nFiles read:", length(files_in_window), 
    "\nFiles with matches:", k, "\n")
```

### A2. METAR: parse consolidated observations
Source: `Diversion_Weather_NewDates.rmd` by Jasmine Wu, chunk “event 2 metar data parsing”.

Parses the consolidated raw METAR reports into structured weather variables and timestamps that can be used in subsequent weather and disruption analyses.

```r
# UPDATE these commands before running this chunk
# --- load the consolidated metar file ---
dt <- fread("Output_METAR_DiversionEvent/metar_DFW_DAL_20241104_20241105.csv", 
            na.strings = c("*NULL*", "NULL", ""))
metar_parsed_dir <- "Output_METAR_DiversionEvent/metar_DFW_DAL_20241104_20241105_parsed.csv"



# 0) cleanning & preparation
# Make sure time column is UTC (adjust if needed)
dt[, rec_time_utc := parse_date_time(MtrRecDateTime,
                                    orders = c("ymd HMS", "ymd HM"),
                                    tz = "UTC")]

# Clean metarBody text
dt[, metar := str_squish(metarBody)]
# dt must have: a unique key per METAR row (create one if needed) + metar text
dt[, metar_id := .I]              # unique id per record


# 1) station + obs time token
# METAR format usually contains: "KDFW 040345Z ..."
dt[, obs_time_token := str_extract(metar, "\\b\\d{6}Z\\b")]

# Extract day + hhmm from token like 040345Z
dt[, obs_day := as.integer(substr(obs_time_token, 1, 2))]
dt[, obs_hh  := as.integer(substr(obs_time_token, 3, 4))]
dt[, obs_mm  := as.integer(substr(obs_time_token, 5, 6))]

# Build obs datetime using rec_time_utc's year-month as anchor
# (Works for the Nov 4–5 window. For month-crossing, we’d add a rollover rule.)
dt[, obs_time_utc := as.POSIXct(
  sprintf("%04d-%02d-%02d %02d:%02d:00",
          year(rec_time_utc), month(rec_time_utc), obs_day, obs_hh, obs_mm),
  tz = "UTC"
)]


# 2) wind: dddssGggKT or VRBssKT
dt[, wind_token := str_extract(metar, "\\b(?:VRB|\\d{3})\\d{2,3}(?:G\\d{2,3})?KT\\b")]
# Direction
dt[, wind_dir := fifelse(!is.na(wind_token) & str_starts(wind_token, "VRB"),
                         NA_integer_,
                         as.integer(substr(wind_token, 1, 3)))]
# Speed: remove leading direction (ddd or VRB), then take digits before optional G
dt[, wind_spd_kt := as.integer(str_extract(wind_token, "(?<=^(?:VRB|\\d{3}))\\d{2,3}"))]
# Gust
dt[, wind_gust_kt := as.integer(str_remove(str_extract(wind_token, "G\\d{2,3}"), "G"))]
# --- wind variability token: dddVddd (often appears right after wind group) ---
dt[, wind_var_token := str_extract(metar, "\\b\\d{3}V\\d{3}\\b")]
dt[, wind_var_from_deg := as.integer(substr(wind_var_token, 1, 3))]
dt[, wind_var_to_deg   := as.integer(substr(wind_var_token, 5, 7))]
# Optional: a simple flag
dt[, wind_is_variable_dir := !is.na(wind_var_token) | (!is.na(wind_token) & str_starts(wind_token, "VRB"))]


# 3) visibility: handle "10SM", "1/2SM", "2 1/2SM"
# Common patterns: "10SM", "1/2SM", "2SM", "2 1/2SM"
dt[, vis_token := str_extract(metar, "\\b(\\d+\\s\\d/\\d|\\d/\\d|\\d+)SM\\b")]

vis_to_num <- function(x) {
  if (is.na(x)) return(NA_real_)
  x <- str_remove(x, "SM")
  x <- str_trim(x)

  # "2 1/2" => 2.5
  if (str_detect(x, "^\\d+\\s\\d/\\d$")) {
    parts <- str_split(x, "\\s", simplify = TRUE)
    whole <- as.numeric(parts[1])
    frac  <- parts[2]
    num <- as.numeric(str_split(frac, "/", simplify = TRUE)[1])
    den <- as.numeric(str_split(frac, "/", simplify = TRUE)[2])
    return(whole + num/den)
  }

  # "1/2" => 0.5
  if (str_detect(x, "^\\d/\\d$")) {
    num <- as.numeric(str_split(x, "/", simplify = TRUE)[1])
    den <- as.numeric(str_split(x, "/", simplify = TRUE)[2])
    return(num/den)
  }

  # "10" => 10
  if (str_detect(x, "^\\d+$")) return(as.numeric(x))

  NA_real_
}

dt[, vis_sm := vapply(vis_token, vis_to_num, numeric(1))]


# 4) cloud layers + ceiling
# Capture cloud groups, including optional CB/TCU suffix:
# FEW020, SCT045, BKN018, OVC100, VV002, plus BKN025CB, SCT030TCU, etc.
dt[, cloud_tokens := str_extract_all(
  metar,
  "\\b(?:FEW|SCT|BKN|OVC|VV)\\d{3}(?:CB|TCU)?\\b"
)]

##### Long table: one row per layer, preserve the order in the METAR
cloud_long <- dt[, .(layer_token = unlist(cloud_tokens)), by = .(metar_id)]

# If some METARs have no cloud tokens, cloud_long will lack those metar_id; that's fine.
# Layer sequence index (order as it appeared)
cloud_long[, layer_idx := seq_len(.N), by = metar_id]

# Parse layer fields
cloud_long[, layer_type := substr(layer_token, 1, 3)]                 # FEW/SCT/BKN/OVC/VV
cloud_long[, layer_hundreds_ft := as.integer(substr(layer_token, 4, 6))]
cloud_long[, layer_ft := layer_hundreds_ft * 100]

# CB/TCU flags
cloud_long[, layer_cb  := str_ends(layer_token, "CB")]
cloud_long[, layer_tcu := str_ends(layer_token, "TCU")]

#### Compute ceiling correctly from the long table
# Ceiling table per METAR
ceiling_dt <- cloud_long[layer_type %in% c("BKN", "OVC", "VV"),
                         .(ceiling_ft = min(layer_ft, na.rm = TRUE)),
                         by = metar_id]
lowest_cloud_dt <- cloud_long[, .(lowest_cloud_ft = min(layer_ft, na.rm = TRUE)), 
                              by = metar_id]

# Merge ceiling back to main dt
dt <- merge(dt, ceiling_dt, by = "metar_id", all.x = TRUE)
dt <- merge(dt, lowest_cloud_dt, by = "metar_id", all.x = TRUE)

#### Keep a compact “layers string” summary per METAR (for debugging)
layers_summary <- cloud_long[, .(
  layers_all = paste(layer_token, collapse = " "),
  layers_ceiling_candidates = paste(layer_token[layer_type %in% c("BKN","OVC","VV")], collapse = " ")
), by = metar_id]

dt <- merge(dt, layers_summary, by = "metar_id", all.x = TRUE)



# 5) Runway visual range (RVR)
# Extract all RVR tokens (can be multiple per METAR)
# Examples:
# R13L/1600FT
# R13L/1600VP6000FT
# R13L/M0600V1000FT
# R04/1200FT/U
dt[, rvr_tokens := str_extract_all(metar, "\\bR\\d{2}[RLC]?/[^\\s]+\\b")]

# Long table: one row per METAR x RVR token
rvr_long <- dt[, .(rvr_token = unlist(rvr_tokens)), by = .(metar_id)]
rvr_long[, rvr_idx := seq_len(.N), by = metar_id]
# Parse runway (e.g., 13L)
rvr_long[, runway := str_extract(rvr_token, "(?<=^R)\\d{2}[RLC]?")]
# Remove leading "Rxx?/"; keep the value part
rvr_long[, rvr_value := str_remove(rvr_token, "^R\\d{2}[RLC]?/")]
# Remove trailing FT and optional trend suffix like /U /D /N
# Some formats: "...FT", "...FT/U", "...FTD" in other feeds; we handle common ones.
rvr_long[, rvr_value := str_replace(rvr_value, "FT(?:/[UDN])?$", "")]
rvr_long[, rvr_trend := str_extract(rvr_token, "(?<=FT/)[:UDN]$")]
# If the above doesn't match due to quirks, safe default:
rvr_long[is.na(rvr_trend), rvr_trend := str_extract(rvr_token, "(?<=FT/)\\w$")]
# Split variable range by V if present
rvr_long[, rvr_variable := str_detect(rvr_value, "V")]
# low and high parts
rvr_long[, `:=`(
  rvr_low_str  = ifelse(rvr_variable, str_split_fixed(rvr_value, "V", 2)[,1], rvr_value),
  rvr_high_str = ifelse(rvr_variable, str_split_fixed(rvr_value, "V", 2)[,2], NA_character_)
)]
# Qualifiers: P (greater than), M (less than)
rvr_long[, rvr_low_qual  := str_extract(rvr_low_str, "^[PM]")]
rvr_long[, rvr_high_qual := str_extract(rvr_high_str, "^[PM]")]
# Numeric values
rvr_long[, rvr_low_ft  := as.integer(str_remove(rvr_low_str, "^[PM]"))]
rvr_long[, rvr_high_ft := as.integer(str_remove(rvr_high_str, "^[PM]"))]
# For non-variable RVR, set high = low (optional convenience)
rvr_long[is.na(rvr_high_ft), rvr_high_ft := rvr_low_ft]
# --- Merge a summary back to dt (optional) ---
# Example: worst (minimum) RVR reported that hour
rvr_summary <- rvr_long[, .(
  rvr_min_ft = suppressWarnings(min(rvr_low_ft, na.rm = TRUE)),
  rvr_any = TRUE
), by = metar_id]

dt <- merge(dt, rvr_summary, by = "metar_id", all.x = TRUE)
dt[is.na(rvr_any), `:=`(rvr_any = FALSE, rvr_min_ft = NA_integer_)]



# 6a) weather flags (basic)
dt[, wx_ts   := str_detect(metar, "\\bTS\\b|\\bTSRA\\b|\\bTSSN\\b")]
dt[, wx_tsra := str_detect(metar, "\\bTSRA\\b")]
dt[, wx_vcts := str_detect(metar, "\\bVCTS\\b")]
dt[, wx_ra   := str_detect(metar, "\\bRA\\b|\\b-RA\\b|\\b\\+RA\\b")]
dt[, wx_shra := str_detect(metar, "\\bSHRA\\b|\\b-SHRA\\b|\\b\\+SHRA\\b")]

dt[, convective_any := wx_ts | wx_vcts]
dt[, convective_onfield := wx_ts]  # TS/TSRA indicates at-field thunder


# 6b) Weather tokens
# Intensity / proximity qualifiers
INTENSITY <- c("-", "+")    # no sign = moderate
PROXIMITY <- c("VC")        # vicinity
# Descriptors
DESCR <- c("MI","PR","BC","DR","BL","SH","TS","FZ")
# Phenomena: precipitation
PRECIP <- c("DZ","RA","SN","SG","IC","PL","GR","GS","UP")
# Obscuration
OBSC <- c("BR","FG","FU","VA","DU","SA","HZ","PY")
# Other (non-precip, non-obsc)
OTHER <- c("PO","SQ","FC","SS","DS")

# Build a regex that matches plausible weather tokens.
# This intentionally excludes cloud groups (FEW020), wind groups (18012KT), etc.
wx_regex <- paste0(
  "\\b",                          # token boundary
  "[+-]?",                        # optional intensity
  "(?:VC)?",                      # optional vicinity
  "(?:", paste(DESCR, collapse="|"), "){0,2}",  # 0-2 descriptors
  "(?:", paste(c(PRECIP, OBSC, OTHER), collapse="|"), "){1,3}", # 1-3 phenomena
  "\\b"
)

dt[, wx_tokens := str_extract_all(metar, wx_regex)]


# 7) Temperature
# Extract temp/dew token: e.g., 21/20 or M02/M05
# Usually appears near the end of the body before altimeter.
dt[, tempdew_token := str_extract(metar, "\\bM?\\d{2}/M?\\d{2}\\b")]
# Extract temp/dew sub-tokens
dt[, temp_token := str_extract(tempdew_token, "^(M?\\d{2})")]
dt[, dew_token  := str_extract(tempdew_token, "(?<=/)(M?\\d{2})")]

decode_temp_vec <- function(x) {
  out <- rep(NA_real_, length(x))
  idx <- !is.na(x)
  out[idx] <- ifelse(
    str_starts(x[idx], "M"),
    -as.numeric(str_remove(x[idx], "^M")),
     as.numeric(x[idx])
  )
  out
}

dt[, temp_c := decode_temp_vec(temp_token)]
dt[, dew_c  := decode_temp_vec(dew_token)]


# 8) Altimeter

# Altimeter token: A#### (inches of mercury * 100)
dt[, alt_token := str_extract(metar, "\\bA\\d{4}\\b")]
dt[, alt_inhg := ifelse(is.na(alt_token), NA_real_,
                        as.integer(str_remove(alt_token, "^A")) / 100)]


colnames(dt)
metar_parsed <- dt
# Keep a tidy output
# core_cols <- c(
#   "metar_id", "MtrStnId", "obs_time_utc", "rec_time_utc",
#   "wind_dir", "wind_spd_kt", "wind_gust_kt",
#   "wind_var_from_deg", "wind_var_to_deg", "wind_is_variable_dir",
#   "vis_sm", "rvr_any", "rvr_min_ft",
#   "ceiling_ft",
#   "temp_c", "dew_c", "alt_inhg",
#   "wx_ra", "wx_shra", "wx_vcts", "wx_ts", "wx_tsra",
#   "convective_any", "convective_onfield"
# )
# metar_core <- dt[, ..core_cols]


# Save parsed file
fwrite(metar_parsed, metar_parsed_dir)
```

### A3. TAF: read and parse one event/window
Source: `Diversion_Weather_NewDates.rmd`  by Jasmine Wu, chunk “event 2 taf data reading parsing”.

Reads raw TAF files for a specified airport and event window and parses individual forecast blocks, including forecast issuance and validity information, into a structured dataset.

```r
# Update these commands before running this chunk
root <- "../../Data/Weather/taf_Sherlock_2024"  # change to your path
keep_stations <- c("KDFW", "KDAL")
start_utc <- ymd_hms("2024-11-04 00:00:00", tz = "UTC")
end_utc   <- ymd_hms("2024-11-05 23:59:59", tz = "UTC")
taf_parsed_dir <- "Output_TAF_DiversionEvent/taf_DFW_DAL_20241104_20241105_parsed.csv"


# 0) Create a list of TAF files to read
days_needed <- seq.Date(as.Date(start_utc), as.Date(end_utc), by = "day")
days_needed

day_dirs <- file.path(
  root,
  format(days_needed, "%Y"),
  format(days_needed, "%m"),
  format(days_needed, "%d")
)

# Keep only dirs that exist
day_dirs <- day_dirs[dir.exists(day_dirs)]

taf_files <- unlist(lapply(day_dirs, function(d) {
  list.files(d, pattern = "\\.txt$", 
             recursive = TRUE, 
             full.names = TRUE)
}), use.names = FALSE)

length(taf_files)
head(taf_files, 5)


# 1) Split each file into TAF “blocks” (records)
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

taf_blocks <- rbindlist(lapply(taf_files, read_taf_blocks), use.names = TRUE, fill = TRUE)
# taf_blocks[, raw_block := str_squish(str_replace_all(raw_block, "\n", " \n "))]
taf_blocks[1, raw_block]
# [1] "2024/11/04 00:00 TAF SCAR 042200Z 0500/0524 21004KT 9999 OVC033 TN18/0510Z TX21/0518Z BECMG 0504/0506 OVC020 BECMG 0517/0519 21014KT FEW033 BECMG 0522/0524 20004KT OVC025"


# 2) Parse each TAF block into header fields + segment lines
parse_taf_block <- function(raw_block) {
  lines <- readLines(textConnection(raw_block), warn = FALSE)
  lines <- gsub("\t", " ", lines)
  lines <- trimws(lines, which = "right")

  # drop empty lines
  lines <- lines[!grepl("^\\s*$", lines)]

  # optional block timestamp line like "2024/11/04 00:00"
  block_ts_chr <- NA_character_
  if (length(lines) > 0 && grepl("^\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}\\b", lines[1])) {
    block_ts_chr <- sub("^\\s*(\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}).*$", "\\1", lines[1])
    lines <- lines[-1]
  }

  # find the header line starting with TAF
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
  m <- str_match(header_line,
                 "^\\s*TAF\\s+(AMD\\s+)?([A-Z0-9]{4})\\s+(\\d{6}Z)\\s+(\\d{4}/\\d{4})\\s*(.*)$")
  is_amd <- !is.na(m[,2])
  station <- m[,3]
  issue_z <- m[,4]
  valid <- m[,5]
  header_rest <- m[,6]

  valid_from <- ifelse(!is.na(valid), substr(valid, 1, 4), NA_character_)
  valid_to   <- ifelse(!is.na(valid), substr(valid, 6, 9), NA_character_)

  # Everything AFTER the header line are segments (BECMG/TEMPO/FM/PROB etc.)
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

taf_parsed <- taf_blocks[, as.list(parse_taf_block(raw_block)), by = .(source_file, block_id)]

taf_parsed[, block_ts_utc := ymd_hm(block_ts_chr, tz = "UTC")]


# 3) Subset to relevant stations (DFW/DAL)
taf_sub <- taf_parsed[station %in% keep_stations]

taf_sub[, .N, by = station][order(station)]
taf_sub[1, .(station, block_ts_chr, issue_z, valid_from, valid_to, header_rest, segments_text)]

taf_sub[, issue_time_utc :=
  as.POSIXct(
    mapply(issuez_to_utc, issue_z, block_ts_utc),
    origin = "1970-01-01", tz = "UTC"
  )
]

taf_sub[, taf_valid_start_utc :=
  as.POSIXct(
    mapply(ddhh_to_utc, valid_from, block_ts_utc),
    origin = "1970-01-01", tz = "UTC"
  )
]

taf_sub[, taf_valid_end_utc :=
  as.POSIXct(
    mapply(ddhh_to_utc, valid_to, block_ts_utc),
    origin = "1970-01-01", tz = "UTC"
  )
]

# handle cross-midnight validity
taf_sub[
  !is.na(taf_valid_end_utc) & !is.na(taf_valid_start_utc) &
    taf_valid_end_utc <= taf_valid_start_utc,
  taf_valid_end_utc := taf_valid_end_utc + days(1)
]

# # valid_to is DDHH, and it can be next day; compute and if <= start, add 1 day
# taf_sub[, taf_valid_end_utc := ddhh_to_utc(valid_to, block_ts_utc)]
# taf_sub[!is.na(taf_valid_end_utc) & !is.na(taf_valid_start_utc) & taf_valid_end_utc <= taf_valid_start_utc,
#         taf_valid_end_utc := taf_valid_end_utc + days(1)]


# 4) Build a segments table that includes BASE + continuation lines
# 1. BASE segment: one row per TAF issuance
taf_base <- taf_sub[, .(
  source_file, block_id, station, block_ts_utc,
  issue_z, valid_from, valid_to, is_amd,
  seg_order = 0L,
  seg_type  = "BASE",
  seg_line  = header_rest
)]

# 2. Other segments: one row per continuation line
taf_cont <- taf_sub[
  !is.na(segments_text) & segments_text != "",
  .(seg_line = unlist(str_split(segments_text, "\\n"))),
  by = .(source_file, block_id, station, block_ts_utc, issue_z, valid_from, valid_to, is_amd)
]

taf_cont[, seg_line := str_squish(seg_line)]
taf_cont <- taf_cont[seg_line != "" & !is.na(seg_line)]

# preserve within-TAF line order
taf_cont[, seg_order := seq_len(.N), by = .(source_file, block_id)]

# Combine
taf_segs <- rbindlist(list(taf_base, taf_cont), use.names = TRUE, fill = TRUE)
setorder(taf_segs, source_file, block_id, seg_order)


# 5) Parse the segment lines into a long “TAF segments” table
# seg_type for FM should be just "FM" (not "FM041700")
taf_segs[, seg_type := fifelse(str_detect(seg_line, "^FM\\d{6}\\b"), "FM",
                        str_extract(seg_line, "^(BECMG|TEMPO|PROB\\d{2})"))]
taf_segs[is.na(seg_type), seg_type := "BASE"]

# BECMG/TEMPO window (DDHH/DDHH)
taf_segs[, seg_valid := fifelse(
  seg_type %in% c("BECMG","TEMPO"),
  str_extract(seg_line, "(?<=^(?:BECMG|TEMPO)\\s)\\d{4}/\\d{4}"),
  NA_character_
)]
taf_segs[, `:=`(
  seg_from = ifelse(!is.na(seg_valid), str_sub(seg_valid, 1, 4), NA_character_),
  seg_to   = ifelse(!is.na(seg_valid), str_sub(seg_valid, 6, 9), NA_character_)
)]

# FM start time (DDHHMM) from FMDDHHMM
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


# 6) Minimal “same fields as METAR” parsing for each TAF segment
# Wind token: dddffKT or dddffGggKT or VRB
taf_segs[, fcst_wind_token := str_extract(seg_body, "\\b(?:VRB|\\d{3})\\d{2,3}(?:G\\d{2,3})?KT\\b")]
taf_segs[, fcst_wind_dir := fifelse(!is.na(fcst_wind_token) & str_starts(fcst_wind_token, "VRB"),
                                    NA_integer_, as.integer(substr(fcst_wind_token, 1, 3)))]
taf_segs[, fcst_wind_spd_kt := as.integer(str_extract(fcst_wind_token, "(?<=^(?:VRB|\\d{3}))\\d{2,3}"))]
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


# 7) Derive seg_start_utc / seg_end_utc for every segment (BASE, TEMPO, BECMG, FM)
# Join issuance timing info onto segments
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

attr(taf_segs$seg_start_utc, "tzone") <- "UTC"
attr(taf_segs$seg_end_utc, "tzone") <- "UTC"


# Order within issuance
setorder(taf_segs, source_file, block_id, seg_start_utc, seg_order)
taf_segs[, next_start := shift(seg_start_utc, type = "lead"), 
         by = .(source_file, block_id)]
attr(taf_segs$next_start, "tzone") <- "UTC"


# Fill missing ends
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
```

### A4. METAR: batch/event-registry workflow
Source: `Diversion_Weather_NewDates.rmd`by Jasmine Wu, chunk “metar functions”.

Extends the single-event METAR workflow to multiple events defined in an event registry. It identifies and processes the relevant METAR files for each airport and analysis window and combines the results across events.

```r
## 1) METAR: read + subset (function)
metar_read_event <- function(event_row, metar_base_dir) {
  stopifnot(nrow(event_row) == 1)

  # derive year-specific root from event start
  yr <- year(event_row$start_utc_dt)
  print(yr)
  metar_year_dir <- file.path(metar_base_dir, as.character(yr))
  print(metar_year_dir)
  if (!dir.exists(metar_year_dir)) {
    stop("METAR year directory not found: ", metar_year_dir,
         "\nCheck metar_base_dir or whether you have that year.")
  }

  start_utc <- event_row$start_utc_dt - 
    hours(as.integer(event_row$metar_buf_hr %||% 12))
  end_utc   <- event_row$end_utc_dt   + 
    hours(as.integer(event_row$metar_buf_hr %||% 12))
  stations_keep <- unlist(event_row$stations_keep)

  files_in_window <- metar_hourly_files_in_window(metar_year_dir, start_utc, end_utc)
  if (length(files_in_window) == 0) {
    stop("No METAR hourly files found for window: ", start_utc, " to ", end_utc,
         "\nYear dir: ", metar_year_dir)
  }

  kept <- vector("list", length(files_in_window))
  k <- 0L

  for (fp in files_in_window) {
    dt <- fread(fp, showProgress = FALSE, na.strings = c("*NULL*", "NULL", ""))
    dt <- dt[MtrStnId %in% stations_keep]
    if (nrow(dt) > 0) {
      dt[, source_file := basename(fp)]
      dt[, source_path := fp]
      k <- k + 1L
      kept[[k]] <- dt
    }
  }

  metar_sub <- rbindlist(kept[seq_len(k)], fill = TRUE)
  setorder(metar_sub, MtrStnId, MtrRecDateTime)

  list(
    data = metar_sub,
    files_found = length(files_in_window),
    files_with_matches = k,
    window_start_utc = start_utc,
    window_end_utc = end_utc,
    stations_keep = stations_keep,
    metar_year_dir = metar_year_dir
  )
}




## 2) METAR: parse (function)
metar_parse <- function(dt) {
  # 0) cleaning & preparation
  dt <- copy(dt)

  dt[, rec_time_utc := parse_date_time(
    MtrRecDateTime, orders = c("ymd HMS", "ymd HM"), tz = "UTC"
  )]

  dt[, metar := str_squish(metarBody)]
  dt[, metar_id := .I]

  # 1) obs time token
  dt[, obs_time_token := str_extract(metar, "\\b\\d{6}Z\\b")]
  dt[, obs_day := as.integer(substr(obs_time_token, 1, 2))]
  dt[, obs_hh  := as.integer(substr(obs_time_token, 3, 4))]
  dt[, obs_mm  := as.integer(substr(obs_time_token, 5, 6))]

  dt[, obs_time_utc := as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:%02d:00",
            year(rec_time_utc), month(rec_time_utc), obs_day, obs_hh, obs_mm),
    tz = "UTC"
  )]

  # 2) wind + variability
  dt[, wind_token := str_extract(metar, "\\b(?:VRB|\\d{3})\\d{2,3}(?:G\\d{2,3})?KT\\b")]
  dt[, wind_dir := fifelse(!is.na(wind_token) & str_starts(wind_token, "VRB"),
                           NA_integer_, as.integer(substr(wind_token, 1, 3)))]
  dt[, wind_spd_kt := as.integer(str_extract(wind_token, "(?<=^(?:VRB|\\d{3}))\\d{2,3}"))]
  dt[, wind_gust_kt := as.integer(str_remove(str_extract(wind_token, "G\\d{2,3}"), "G"))]

  dt[, wind_var_token := str_extract(metar, "\\b\\d{3}V\\d{3}\\b")]
  dt[, wind_var_from_deg := as.integer(substr(wind_var_token, 1, 3))]
  dt[, wind_var_to_deg   := as.integer(substr(wind_var_token, 5, 7))]
  dt[, wind_is_variable_dir := !is.na(wind_var_token) | (!is.na(wind_token) & str_starts(wind_token, "VRB"))]

  # 3) visibility (SM)
  dt[, vis_token := str_extract(metar, "\\b(\\d+\\s\\d/\\d|\\d/\\d|\\d+)SM\\b")]

  vis_to_num <- function(x) {
    if (is.na(x)) return(NA_real_)
    x <- str_remove(x, "SM") %>% str_trim()

    if (str_detect(x, "^\\d+\\s\\d/\\d$")) {
      parts <- str_split(x, "\\s", simplify = TRUE)
      whole <- as.numeric(parts[1])
      frac  <- parts[2]
      num <- as.numeric(str_split(frac, "/", simplify = TRUE)[1])
      den <- as.numeric(str_split(frac, "/", simplify = TRUE)[2])
      return(whole + num/den)
    }
    if (str_detect(x, "^\\d/\\d$")) {
      num <- as.numeric(str_split(x, "/", simplify = TRUE)[1])
      den <- as.numeric(str_split(x, "/", simplify = TRUE)[2])
      return(num/den)
    }
    if (str_detect(x, "^\\d+$")) return(as.numeric(x))
    NA_real_
  }
  dt[, vis_sm := vapply(vis_token, vis_to_num, numeric(1))]

  # 4) clouds + ceiling (long table per METAR)
  dt[, cloud_tokens := str_extract_all(
    metar, "\\b(?:FEW|SCT|BKN|OVC|VV)\\d{3}(?:CB|TCU)?\\b"
  )]

  cloud_long <- dt[, .(layer_token = unlist(cloud_tokens)), by = .(metar_id)]
  cloud_long[, layer_idx := seq_len(.N), by = metar_id]
  cloud_long[, layer_type := substr(layer_token, 1, 3)]
  cloud_long[, layer_hundreds_ft := as.integer(substr(layer_token, 4, 6))]
  cloud_long[, layer_ft := layer_hundreds_ft * 100L]
  cloud_long[, layer_cb  := str_ends(layer_token, "CB")]
  cloud_long[, layer_tcu := str_ends(layer_token, "TCU")]

  ceiling_dt <- cloud_long[layer_type %in% c("BKN","OVC","VV"),
                           .(ceiling_ft = min(layer_ft, na.rm = TRUE)),
                           by = metar_id]
  lowest_cloud_dt <- cloud_long[, .(lowest_cloud_ft = min(layer_ft, na.rm = TRUE)), by = metar_id]

  dt <- merge(dt, ceiling_dt, by="metar_id", all.x=TRUE)
  dt <- merge(dt, lowest_cloud_dt, by="metar_id", all.x=TRUE)

  layers_summary <- cloud_long[, .(
    layers_all = paste(layer_token, collapse=" "),
    layers_ceiling_candidates = paste(layer_token[layer_type %in% c("BKN","OVC","VV")], collapse=" ")
  ), by = metar_id]
  dt <- merge(dt, layers_summary, by="metar_id", all.x=TRUE)

  # 5) RVR
  dt[, rvr_tokens := str_extract_all(metar, "\\bR\\d{2}[RLC]?/[^\\s]+\\b")]
  rvr_long <- dt[, .(rvr_token = unlist(rvr_tokens)), by = .(metar_id)]
  rvr_long[, runway := str_extract(rvr_token, "(?<=^R)\\d{2}[RLC]?")]
  rvr_long[, rvr_value := str_remove(rvr_token, "^R\\d{2}[RLC]?/")]
  rvr_long[, rvr_value := str_replace(rvr_value, "FT(?:/[UDN])?$", "")]
  rvr_long[, rvr_variable := str_detect(rvr_value, "V")]
  rvr_long[, `:=`(
    rvr_low_str  = ifelse(rvr_variable, str_split_fixed(rvr_value, "V", 2)[,1], rvr_value),
    rvr_high_str = ifelse(rvr_variable, str_split_fixed(rvr_value, "V", 2)[,2], NA_character_)
  )]
  rvr_long[, rvr_low_ft  := as.integer(str_remove(rvr_low_str, "^[PM]"))]
  rvr_long[, rvr_high_ft := as.integer(str_remove(rvr_high_str, "^[PM]"))]
  rvr_long[is.na(rvr_high_ft), rvr_high_ft := rvr_low_ft]

  rvr_summary <- rvr_long[, .(
    rvr_min_ft = suppressWarnings(min(rvr_low_ft, na.rm = TRUE)),
    rvr_any = TRUE
  ), by = metar_id]
  dt <- merge(dt, rvr_summary, by="metar_id", all.x=TRUE)
  dt[is.na(rvr_any), `:=`(rvr_any = FALSE, rvr_min_ft = NA_integer_)]

  # 6) wx flags (basic)
  dt[, wx_ts   := str_detect(metar, "\\bTS\\b|\\bTSRA\\b|\\bTSSN\\b")]
  dt[, wx_tsra := str_detect(metar, "\\bTSRA\\b")]
  dt[, wx_vcts := str_detect(metar, "\\bVCTS\\b")]
  dt[, wx_ra   := str_detect(metar, "\\bRA\\b|\\b-RA\\b|\\b\\+RA\\b")]
  dt[, wx_shra := str_detect(metar, "\\bSHRA\\b|\\b-SHRA\\b|\\b\\+SHRA\\b")]
  dt[, convective_any := wx_ts | wx_vcts]
  dt[, convective_onfield := wx_ts]

  # 7) temp/dew
  dt[, tempdew_token := str_extract(metar, "\\bM?\\d{2}/M?\\d{2}\\b")]
  dt[, temp_token := str_extract(tempdew_token, "^(M?\\d{2})")]
  dt[, dew_token  := str_extract(tempdew_token, "(?<=/)(M?\\d{2})")]

  decode_temp_vec <- function(x) {
    out <- rep(NA_real_, length(x))
    idx <- !is.na(x)
    out[idx] <- ifelse(
      str_starts(x[idx], "M"),
      -as.numeric(str_remove(x[idx], "^M")),
       as.numeric(x[idx])
    )
    out
  }
  dt[, temp_c := decode_temp_vec(temp_token)]
  dt[, dew_c  := decode_temp_vec(dew_token)]

  # 8) altimeter
  dt[, alt_token := str_extract(metar, "\\bA\\d{4}\\b")]
  dt[, alt_inhg := ifelse(is.na(alt_token), NA_real_,
                          as.integer(str_remove(alt_token, "^A")) / 100)]

  dt[]
}

write_metar_outputs <- function(event_row, metar_sub, metar_parsed, out_root) {
  event_dir <- file.path(out_root, event_row$event_id)
  dir.create(event_dir, recursive = TRUE, showWarnings = FALSE)

  fwrite(metar_sub, file.path(event_dir, "metar_sub.csv"))
  saveRDS(metar_parsed, file.path(event_dir, "metar_parsed.rds"))
  fwrite(metar_parsed, file.path(event_dir, "metar_parsed.csv"))
  
  print(paste0("METAR outputs written in ", event_dir,  ": \nmetar_sub.csv \nmetar_parsed.rds \nmetar_parsed.csv"))

  invisible(event_dir)
}



## 3) METAR visualization helpers
# plot_metar_ceiling <- function(metar_parsed, station="KDFW") {
#   dt <- copy(metar_parsed)
#   dt[, obs_time_utc := ymd_hms(obs_time_utc, tz="UTC")]
#   dt <- dt[MtrStnId == station][order(obs_time_utc)]
# 
#   dt[, flight_cat := fifelse(
#     (!is.na(ceiling_ft) & ceiling_ft < 500) | (!is.na(vis_sm) & vis_sm < 1), "LIFR",
#     fifelse((!is.na(ceiling_ft) & ceiling_ft < 1000) | (!is.na(vis_sm) & vis_sm < 3), "IFR",
#       fifelse((!is.na(ceiling_ft) & ceiling_ft < 3000) | (!is.na(vis_sm) & vis_sm <= 5), "MVFR", "VFR")
#     )
#   )]
#   dt[, flight_cat := factor(flight_cat, levels=c("LIFR","IFR","MVFR","VFR"))]
# 
#   bands <- data.table(
#     cat = factor(c("LIFR","IFR","MVFR","VFR"), levels=c("LIFR","IFR","MVFR","VFR")),
#     ymin = c(0, 500, 1000, 3000),
#     ymax = c(500, 1000, 3000, 12000)
#   )
# 
#   flight_cat_colors <- c("VFR"="#2ECC71","MVFR"="#3498DB","IFR"="#E74C3C","LIFR"="#9B59B6")
# 
#   ggplot(dt, aes(x=obs_time_utc)) +
#     geom_rect(data=bands, aes(xmin=-Inf, xmax=Inf, ymin=ymin, ymax=ymax, fill=cat),
#               inherit.aes=FALSE, alpha=0.12) +
#     scale_fill_manual(values=flight_cat_colors, name="Flight Category") +
#     geom_line(aes(y=ceiling_ft), na.rm=TRUE) +
#     geom_point(aes(y=ceiling_ft, shape=convective_any), size=2, na.rm=TRUE) +
#     scale_shape_discrete(name="Convective") +
#     scale_y_continuous(name="Ceiling (ft AGL)", limits=c(0,12000), breaks=c(0,500,1000,3000,5000,8000,12000),
#                        labels=scales::comma) +
#     scale_x_datetime(name="Time (UTC)", date_breaks="3 hours", date_labels="%b %d\n%H:%M") +
#     labs(title=paste0(station, " METAR: Ceiling over time"),
#          subtitle="Points are METAR observations; shape indicates convective conditions") +
#     theme_minimal(base_size=12) +
#     theme(panel.grid.minor=element_blank())
# }


## 4) Example: run METAR for Event 2 (DFW) using functions
# metar_root <- "../../Data/Weather/METAR_SDS_2024/2024"
# ev <- events[event_id=="E02_DFW_20241104_20241105"][1]
# 
# metar_res <- metar_read_event(
#   root_dir = metar_root,
#   event_start = ev$start_utc,
#   event_end   = ev$end_utc,
#   stations_keep = unlist(ev$stations_keep)
# )
# 
# metar_parsed <- metar_parse(metar_res$data)
# 
# dir.create("Output/Event_E02", recursive=TRUE, showWarnings=FALSE)
# saveRDS(metar_parsed, "Output/Event_E02/metar_parsed.rds")
# fwrite(metar_res$data, "Output/Event_E02/metar_sub.csv")
# 
# plot_metar_ceiling(metar_parsed, station="KDFW")

# One-event runner (METAR only)
run_metar_one_event <- function(event_row, metar_base_dir, out_root,
                                overwrite=FALSE) {
  stopifnot(nrow(event_row) == 1)

  event_dir <- file.path(out_root, event_row$event_id)
  print(event_dir)
  rds_path  <- file.path(event_dir, "metar_parsed.rds")
  print(rds_path)

  if (!overwrite && file.exists(rds_path)) {
    return(list(event_id = event_row$event_id, status="cached", path=rds_path))
  }

  res <- metar_read_event(event_row, metar_base_dir)
  metar_sub <- res$data

  # our existing parser function
  metar_parsed <- metar_parse(metar_sub)

  write_metar_outputs(event_row, metar_sub, metar_parsed, out_root)

  list(event_id = event_row$event_id, status="ok",
       rows=nrow(metar_sub),
       files_found=res$files_found, files_with_matches=res$files_with_matches,
       metar_year_dir=res$metar_year_dir)
}

## example test run
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

metar_E02_event_row <- events[event_id == "E02_DFW_20241104_20241105", .(event_id, airport, stations_keep, start_utc_dt, end_utc_dt)]
metar_E02_event_row

metar_E02_out_root <- "Output_METAR_DiversionEvent_NewDates"
metar_base_dir <- "../../Data/Weather/METAR_SDS"

run_metar_one_event(metar_E02_event_row, metar_base_dir, out_root = metar_E02_out_root, overwrite = TRUE)
  
  

## 5) Batch METAR across all events
run_metar_all_events <- function(events_dt, metar_base_dir,
                                 out_root="Output/Events",
                                 overwrite=FALSE) {
  qc <- vector("list", nrow(events_dt))

  for (i in seq_len(nrow(events_dt))) {
    ev <- events_dt[i]
    message("METAR: ", ev$event_id, " (", ev$airport, ")")

    qc[[i]] <- tryCatch(
      run_metar_one_event(ev, metar_base_dir, out_root, overwrite),
      error = function(e) list(event_id=ev$event_id, status="error", msg=conditionMessage(e))
    )
  }

  rbindlist(qc, fill=TRUE)
}


##############################
# EXECUTING CODE
#############################
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")
metar_base_dir <- "../../Data/Weather/METAR_SDS"
metar_out_root <- "Output_METAR_DiversionEvent_NewDates"

# run METAR for all events
metar_qc <- run_metar_all_events(events, metar_base_dir,
                                 out_root = metar_out_root, 
                                 overwrite = TRUE)
metar_qc
```

### A5. TAF: batch/event-registry workflow
Source: `Diversion_Weather_NewDates.rmd` by Jasmine Wu, chunk “taf functions”.

Extends the TAF workflow to multiple events defined in an event registry. It reads and parses the relevant forecasts for each airport and analysis window and produces a consolidated event-level TAF dataset.

```r
## 1) List TAF text files for an event window
taf_list_files_event <- function(event_row, taf_base_dir) {
  stopifnot(nrow(event_row) == 1)

  if (!dir.exists(taf_base_dir)) stop("TAF base directory not found: ", taf_base_dir)

  # buffer
  if (!("taf_buf_hr" %in% names(event_row))) event_row[, taf_buf_hr := 0L]
  buf <- as.integer(event_row$taf_buf_hr %||% 0)

  start_utc <- event_row$start_utc_dt - hours(buf)
  end_utc <- event_row$end_utc_dt + hours(buf)

  days_needed <- seq.Date(as.Date(start_utc), as.Date(end_utc), by="day")

  day_dirs <- file.path(
    taf_base_dir,
    format(days_needed, "%Y"),
    format(days_needed, "%m"),
    format(days_needed, "%d")
  )

  day_dirs <- day_dirs[dir.exists(day_dirs)]
  if (length(day_dirs) == 0) return(character())

  taf_files <- unlist(lapply(day_dirs, function(d) {
    list.files(d, pattern="\\.txt$", recursive=TRUE, full.names=TRUE)
  }), use.names=FALSE)

  taf_files
}


## 2) Read blocks from one file 
taf_read_blocks_file <- function(file) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  x <- str_replace_all(x, "\t", " ")
  x <- str_trim(x, side = "right")

  is_blank <- str_detect(x, "^\\s*$")
  block_id <- cumsum(is_blank) + 1L

  dt <- data.table(
    source_file = file,
    block_id = block_id[!is_blank],
    line = x[!is_blank]
  )

  dt[, .(raw_block = paste(line, collapse="\n")), by=.(source_file, block_id)]
}


## 3) Parse one block into header fields + continuation text
# taf_parse_block <- function(raw_block) {
#   lines <- readLines(textConnection(raw_block), warn = FALSE)
#   lines <- gsub("\t", " ", lines)
#   lines <- trimws(lines, which = "right")
#   lines <- lines[!grepl("^\\s*$", lines)]
#   if (length(lines) == 0) {
#     return(list(block_ts_chr=NA_character_, 
#                 station=NA_character_, issue_z=NA_character_,
#                 valid_from=NA_character_, 
#                 valid_to=NA_character_, is_amd=FALSE,
#                 header_rest=NA_character_, 
#                 segments_text=NA_character_, taf_text=NA_character_))
#   }
# 
#   # optional block timestamp line like "2024/11/04 00:00"
#   block_ts_chr <- NA_character_
#   if (grepl("^\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}\\b", lines[1])) {
#     block_ts_chr <- sub("^\\s*(\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}).*$", "\\1", lines[1])
#     lines <- lines[-1]
#   }
# 
#   hdr_idx <- which(grepl("^\\s*TAF\\b", lines))[1]
#   if (is.na(hdr_idx)) {
#     return(list(block_ts_chr=block_ts_chr, 
#                 station=NA_character_, issue_z=NA_character_,
#                 valid_from=NA_character_, valid_to=NA_character_, is_amd=FALSE,
#                 header_rest=NA_character_, segments_text=NA_character_,
#                 taf_text=paste(lines, collapse=" ")))
#   }
# 
#   header_line <- lines[hdr_idx]
# 
#   m <- str_match(header_line,
#                  "^\\s*TAF\\s+(AMD\\s+)?([A-Z0-9]{4})\\s+(\\d{6}Z)\\s+(\\d{4}/\\d{4})\\s*(.*)$")
#   is_amd <- !is.na(m[,2])
#   station <- m[,3]
#   issue_z <- m[,4]
#   valid <- m[,5]
#   header_rest <- m[,6]
# 
#   valid_from <- ifelse(!is.na(valid), substr(valid, 1, 4), NA_character_)
#   valid_to   <- ifelse(!is.na(valid), substr(valid, 6, 9), NA_character_)
# 
#   seg_lines <- if (hdr_idx < length(lines)) lines[(hdr_idx+1):length(lines)] else character(0)
#   segments_text <- if (length(seg_lines)==0) NA_character_ else paste(seg_lines, collapse="\n")
# 
#   list(
#     block_ts_chr = block_ts_chr,
#     station = station,
#     issue_z = issue_z,
#     valid_from = valid_from,
#     valid_to = valid_to,
#     is_amd = is_amd,
#     header_rest = header_rest,
#     segments_text = segments_text,
#     taf_text = paste(lines, collapse=" ")
#   )
# }
taf_parse_block <- function(raw_block) {
  lines <- readLines(textConnection(raw_block), warn = FALSE)
  lines <- gsub("\t", " ", lines)
  lines <- trimws(lines, which = "right")
  lines <- lines[!grepl("^\\s*$", lines)]

  if (length(lines) == 0) {
    return(list(
      block_ts_chr = NA_character_,
      station = NA_character_,
      issue_z = NA_character_,
      valid_from = NA_character_,
      valid_to = NA_character_,
      is_amd = FALSE,
      header_rest = NA_character_,
      segments_text = NA_character_,
      taf_text = NA_character_
    ))
  }

  # timestamp line, possibly with "Amendment"/"Ammendment"
  block_ts_chr <- NA_character_
  block_ts_line <- NA_character_

  if (grepl("^\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}\\b", lines[1])) {
    block_ts_line <- lines[1]
    block_ts_chr <- sub(
      "^\\s*(\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}).*$",
      "\\1",
      lines[1]
    )
    lines <- lines[-1]
  }

  is_amd_from_ts <- !is.na(block_ts_line) &&
    grepl("amend|ammend", block_ts_line, ignore.case = TRUE)

  # Normalize spacing but keep original line boundaries for continuation lines
  lines_clean <- str_squish(lines)

  # Drop standalone TAF line if present
  lines_clean <- lines_clean[lines_clean != "TAF"]

  if (length(lines_clean) == 0) {
    return(list(
      block_ts_chr = block_ts_chr,
      station = NA_character_,
      issue_z = NA_character_,
      valid_from = NA_character_,
      valid_to = NA_character_,
      is_amd = is_amd_from_ts,
      header_rest = NA_character_,
      segments_text = NA_character_,
      taf_text = NA_character_
    ))
  }

  # Find header line flexibly:
  # 1. TAF AMD KDFW ...
  # 2. AMD KDFW ...
  # 3. KDFW ...
  hdr_idx <- which(grepl(
    "^(TAF\\s+)?(AMD\\s+|COR\\s+)?[A-Z0-9]{4}\\s+\\d{6}Z\\s+\\d{4}/\\d{4}\\b",
    lines_clean
  ))[1]

  if (is.na(hdr_idx)) {
    return(list(
      block_ts_chr = block_ts_chr,
      station = NA_character_,
      issue_z = NA_character_,
      valid_from = NA_character_,
      valid_to = NA_character_,
      is_amd = is_amd_from_ts,
      header_rest = NA_character_,
      segments_text = NA_character_,
      taf_text = paste(lines_clean, collapse = " ")
    ))
  }

  header_line <- lines_clean[hdr_idx]

  m <- str_match(
    header_line,
    "^(?:TAF\\s+)?(?:(AMD|COR)\\s+)?([A-Z0-9]{4})\\s+(\\d{6}Z)\\s+(\\d{4}/\\d{4})\\s*(.*)$"
  )

  amend_token <- m[, 2]
  station <- m[, 3]
  issue_z <- m[, 4]
  valid <- m[, 5]
  header_rest <- m[, 6]

  is_amd <- is_amd_from_ts | (!is.na(amend_token) & amend_token == "AMD")

  valid_from <- ifelse(!is.na(valid), substr(valid, 1, 4), NA_character_)
  valid_to   <- ifelse(!is.na(valid), substr(valid, 6, 9), NA_character_)

  seg_lines <- if (hdr_idx < length(lines_clean)) {
    lines_clean[(hdr_idx + 1):length(lines_clean)]
  } else {
    character(0)
  }

  segments_text <- if (length(seg_lines) == 0) {
    NA_character_
  } else {
    paste(seg_lines, collapse = "\n")
  }

  list(
    block_ts_chr = block_ts_chr,
    station = station,
    issue_z = issue_z,
    valid_from = valid_from,
    valid_to = valid_to,
    is_amd = is_amd,
    header_rest = header_rest,
    segments_text = segments_text,
    taf_text = paste(lines_clean, collapse = " ")
  )
}

## 4) Build segments table: BASE + each continuation line
taf_build_segments <- function(taf_sub) {
  # BASE segment
  taf_base <- taf_sub[, .(
    source_file, block_id, station, block_ts_utc,
    issue_z, valid_from, valid_to, is_amd,
    seg_order = 0L,
    seg_type  = "BASE",
    seg_line  = header_rest
  )]

  # Continuation lines
  taf_cont <- taf_sub[
    !is.na(segments_text) & segments_text != "",
    .(seg_line = unlist(str_split(segments_text, "\\n"))),
    by = .(source_file, block_id, station, block_ts_utc, issue_z, valid_from, valid_to, is_amd)
  ]
  taf_cont[, seg_line := str_squish(seg_line)]
  taf_cont <- taf_cont[seg_line != "" & !is.na(seg_line)]
  taf_cont[, seg_order := seq_len(.N), by=.(source_file, block_id)]

  taf_segs <- rbindlist(list(taf_base, taf_cont), use.names=TRUE, fill=TRUE)
  setorder(taf_segs, source_file, block_id, seg_order)

  # segment type
  taf_segs[, seg_type := fifelse(str_detect(seg_line, "^FM\\d{6}\\b"), "FM",
                          str_extract(seg_line, "^(BECMG|TEMPO|PROB\\d{2})"))]
  taf_segs[is.na(seg_type), seg_type := "BASE"]

  # extract windows
  taf_segs[, seg_valid := fifelse(
    seg_type %in% c("BECMG","TEMPO"),
    str_extract(seg_line, "(?<=^(?:BECMG|TEMPO)\\s)\\d{4}/\\d{4}"),
    NA_character_
  )]
  taf_segs[, `:=`(
    seg_from = ifelse(!is.na(seg_valid), str_sub(seg_valid, 1, 4), NA_character_),
    seg_to   = ifelse(!is.na(seg_valid), str_sub(seg_valid, 6, 9), NA_character_)
  )]

  # FM start DDHHMM
  taf_segs[, fm_from := fifelse(seg_type=="FM", str_extract(seg_line, "(?<=^FM)\\d{6}"), NA_character_)]

  # remove header tokens -> seg_body
  taf_segs[, seg_body := seg_line]
  taf_segs[seg_type %in% c("BECMG","TEMPO"),
           seg_body := str_remove(seg_body, "^(BECMG|TEMPO)\\s+\\d{4}/\\d{4}\\s+")]
  taf_segs[seg_type=="FM",
           seg_body := str_remove(seg_body, "^FM\\d{6}\\s+")]
  taf_segs[str_detect(seg_type, "^PROB"),
           seg_body := str_remove(seg_body, "^PROB\\d{2}\\s+")]

  taf_segs[]
}


## 5) Parse “METAR-like” fields from each segment
taf_parse_segment_fields <- function(taf_segs) {
  taf_segs <- copy(taf_segs)

  # Wind
  taf_segs[, fcst_wind_token := str_extract(seg_body, "\\b(?:VRB|\\d{3})\\d{2,3}(?:G\\d{2,3})?KT\\b")]
  taf_segs[, fcst_wind_dir := fifelse(!is.na(fcst_wind_token) & str_starts(fcst_wind_token, "VRB"),
                                      NA_integer_, as.integer(substr(fcst_wind_token, 1, 3)))]
  taf_segs[, fcst_wind_spd_kt := as.integer(str_extract(fcst_wind_token, "(?<=^(?:VRB|\\d{3}))\\d{2,3}"))]
  taf_segs[, fcst_wind_gust_kt := as.integer(str_remove(str_extract(fcst_wind_token, "G\\d{2,3}"), "G"))]

  # Vis (SM) common in US TAF: P6SM, 6SM
  taf_segs[, fcst_vis_sm := as.numeric(str_remove(str_extract(seg_body, "\\bP?\\d+SM\\b"), "SM"))]
  taf_segs[str_detect(seg_body, "\\bP6SM\\b"), fcst_vis_sm := 6]

  # Clouds
  taf_segs[, cloud_tokens := str_extract_all(seg_body, "\\b(?:FEW|SCT|BKN|OVC|VV)\\d{3}(?:CB|TCU)?\\b")]

  get_ceiling_ft <- function(tokens) {
    if (length(tokens)==0) return(NA_integer_)
    types <- substr(tokens, 1, 3)
    h <- as.integer(substr(tokens, 4, 6)) * 100L
    cand <- h[types %in% c("BKN","OVC","VV")]
    if (length(cand)==0) NA_integer_ else min(cand, na.rm=TRUE)
  }
  taf_segs[, fcst_ceiling_ft := vapply(cloud_tokens, get_ceiling_ft, integer(1))]

  # Convective flags
  taf_segs[, fcst_vcts := str_detect(seg_body, "\\bVCTS\\b")]
  taf_segs[, fcst_vcsh := str_detect(seg_body, "\\bVCSH\\b")]
  taf_segs[, fcst_ts   := str_detect(seg_body, "\\bTS\\b|\\bTSRA\\b|\\bVCTS\\b")]
  taf_segs[, fcst_tsra := str_detect(seg_body, "\\bTSRA\\b")]
  taf_segs[, fcst_ra   := str_detect(seg_body, "\\bRA\\b|\\bSHRA\\b")]
  taf_segs[, fcst_shra := str_detect(seg_body, "\\bSHRA\\b")]

  # segment flight category
  taf_segs[, fcst_flight_cat := fifelse(
    (!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 500) | (!is.na(fcst_vis_sm) & fcst_vis_sm < 1), "LIFR",
    fifelse((!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 1000) | (!is.na(fcst_vis_sm) & fcst_vis_sm < 3), "IFR",
      fifelse((!is.na(fcst_ceiling_ft) & fcst_ceiling_ft < 3000) | (!is.na(fcst_vis_sm) & fcst_vis_sm <= 5), "MVFR", "VFR")
    )
  )]

  taf_segs[]
}


## 6) Add issuance + segment datetimes (the critical step)
taf_add_datetimes <- function(taf_segs, taf_sub) {
  taf_sub2 <- copy(taf_sub)

  # derive block timestamp (UTC)
  taf_sub2[, block_ts_utc := ymd_hm(block_ts_chr, tz="UTC")]

  # issue time
  taf_sub2[, issue_time_utc := issuez_to_utc(issue_z, block_ts_utc)]

  # TAF valid window
  taf_sub2[, taf_valid_start_utc := ddhh_to_utc(valid_from, block_ts_utc)]
  taf_sub2[, taf_valid_end_utc   := ddhh_to_utc(valid_to,   block_ts_utc)]
  taf_sub2[!is.na(taf_valid_end_utc) & !is.na(taf_valid_start_utc) & taf_valid_end_utc <= taf_valid_start_utc,
           taf_valid_end_utc := taf_valid_end_utc + days(1)]

  # join issuance info onto segments
  segs <- merge(
    copy(taf_segs),
    taf_sub2[, .(source_file, block_id, station, issue_time_utc, taf_valid_start_utc, taf_valid_end_utc)],
    by=c("source_file","block_id","station"),
    all.x=TRUE
  )

  segs[, `:=`(seg_start_utc=as.POSIXct(NA, tz="UTC"), seg_end_utc=as.POSIXct(NA, tz="UTC"))]

  # BASE
  segs[seg_type=="BASE", seg_start_utc := taf_valid_start_utc]

  # TEMPO/BECMG
  segs[seg_type %in% c("TEMPO","BECMG") & !is.na(seg_from),
       seg_start_utc := ddhh_to_utc(seg_from, taf_valid_start_utc)]
  segs[seg_type %in% c("TEMPO","BECMG") & !is.na(seg_to),
       seg_end_utc := ddhh_to_utc(seg_to, taf_valid_start_utc)]
  segs[seg_type %in% c("TEMPO","BECMG") & !is.na(seg_start_utc) & !is.na(seg_end_utc) & seg_end_utc <= seg_start_utc,
       seg_end_utc := seg_end_utc + days(1)]

  # FM
  segs[seg_type=="FM" & !is.na(fm_from),
       seg_start_utc := ddhhmm_to_utc(fm_from, taf_valid_start_utc)]

  # fill ends from next start or taf_valid_end
  setorder(segs, source_file, block_id, seg_start_utc, seg_order)
  segs[, next_start := shift(seg_start_utc, type="lead"), by=.(source_file, block_id)]
  segs[is.na(seg_end_utc), seg_end_utc := next_start]
  segs[is.na(seg_end_utc), seg_end_utc := taf_valid_end_utc]

  segs[]
}


## 7) Full event-level TAF runner (read → parse → segments → datetimes → write)
taf_process_one_event <- function(event_row, taf_base_dir, out_root="Output/Events", overwrite=FALSE) {
  stopifnot(nrow(event_row)==1)

  event_dir <- file.path(out_root, event_row$event_id)
  dir.create(event_dir, recursive=TRUE, showWarnings=FALSE)
  rds_path <- file.path(event_dir, "taf_segs_parsed.rds")

  if (!overwrite && file.exists(rds_path)) {
    return(list(event_id=event_row$event_id, status="cached", path=rds_path))
  }

  files <- taf_list_files_event(event_row, taf_base_dir)
  if (length(files)==0) stop("No TAF files found for event days under: ", taf_base_dir)

  # read blocks
  blocks <- rbindlist(lapply(files, taf_read_blocks_file), use.names=TRUE, fill=TRUE)

  # parse blocks
  taf_parsed <- blocks[, as.list(taf_parse_block(raw_block)), by=.(source_file, block_id)]
  taf_parsed[, block_ts_utc := ymd_hm(block_ts_chr, tz="UTC")]

  # subset stations
  keep_stations <- unlist(event_row$stations_keep)
  taf_sub <- taf_parsed[station %in% keep_stations]
  if (nrow(taf_sub)==0) stop("No TAF blocks after station filter: ", paste(keep_stations, collapse=", "))

  # segments + fields + datetimes
  segs <- taf_build_segments(taf_sub)
  segs <- taf_parse_segment_fields(segs)
  segs <- taf_add_datetimes(segs, taf_sub)

  # write
  fwrite(segs, file.path(event_dir, "taf_segs_parsed.csv"))
  saveRDS(segs, file.path(event_dir, "taf_segs_parsed.rds"))

  list(event_id=event_row$event_id, status="ok", rows=nrow(segs), files=length(files))
}

## example test run [e2]
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

taf_E02_event_row <- events[event_id == "E02_DFW_20241104_20241105", .(event_id, airport, stations_keep, start_utc_dt, end_utc_dt)]
taf_E02_event_row

taf_E02_out_root <- "Output_TAF_DiversionEvent_NewDates"
taf_base_dir <- "../../Data/Weather/taf_Sherlock/"

taf_process_one_event(taf_E02_event_row, taf_base_dir,
                      out_root=taf_E02_out_root,
                      overwrite=TRUE) 

## example test run [e3]
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

taf_E03_event_row <- events[event_id == "E03_LGA_20220317_20220318", .(event_id, airport, stations_keep, start_utc_dt, end_utc_dt)]
taf_E03_event_row

taf_E03_out_root <- "Output_TAF_DiversionEvent_NewDates"
taf_base_dir <- "../../Data/Weather/taf_Sherlock/"

taf_process_one_event(taf_E03_event_row, taf_base_dir,
                      out_root=taf_E03_out_root,
                      overwrite=TRUE) 


## example test run [e4]
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

taf_E04_event_row <- events[event_id == "E04_SAN_20241219_20241221", .(event_id, airport, stations_keep, start_utc_dt, end_utc_dt)]
taf_E04_event_row

taf_E04_out_root <- "Output_TAF_DiversionEvent_NewDates"
taf_base_dir <- "../../Data/Weather/taf_Sherlock/"

taf_process_one_event(taf_E04_event_row, taf_base_dir,
                      out_root=taf_E04_out_root,
                      overwrite=TRUE) 

## example test run [e20]
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

taf_E20_event_row <- events[event_id == "E20_DFW_20220601_20220602", .(event_id, airport, stations_keep, start_utc_dt, end_utc_dt)]
taf_E20_event_row

taf_E20_out_root <- "Output_TAF_DiversionEvent_NewDates"
taf_base_dir <- "../../Data/Weather/taf_Sherlock/"

taf_process_one_event(taf_E20_event_row, taf_base_dir,
                      out_root=taf_E20_out_root,
                      overwrite=TRUE) 


## 8) Batch run across all events
taf_process_all_events <- function(events_dt, taf_base_dir,
                                   out_root="Output/Events",
                                   overwrite=FALSE) {
  qc <- vector("list", nrow(events_dt))

  for (i in seq_len(nrow(events_dt))) {
    ev <- events_dt[i]
    message("TAF: ", ev$event_id, " (", ev$airport, ")")

    qc[[i]] <- tryCatch(
      taf_process_one_event(ev, taf_base_dir, out_root, overwrite),
      error=function(e) list(event_id=ev$event_id, status="error", msg=conditionMessage(e))
    )
  }
  rbindlist(qc, fill=TRUE)
}


## 10) Run TAF batch
events <- read_event_registry("top20_diversion_events_registry_20260403.csv")

taf_base_dir <- "../../Data/Weather/taf_Sherlock"
taf_out_root <- "Output_TAF_DiversionEvent_NewDates/"

taf_qc <- taf_process_all_events(events, taf_base_dir, 
                                 out_root=taf_out_root, 
                                 overwrite=TRUE)
taf_qc
```



## B. Airport timezone table and airport-specific conversion (Python)

### B1. Generate airport → timezone mapping from coordinates

Source: `timezone_conversion.ipynb` by Jasmine Wu.

Uses airport latitude and longitude coordinates to identify the corresponding IANA timezone and create an airport-to-timezone reference table.


```python
# Initialize timezone finder
tf = TimezoneFinder()

# 1. Build airport → timezone table from coordinates
def build_airport_timezone_table(airport_ref):
    """
    Build airport timezone table using lat/lon.

    Parameters
    ----------
    airport_ref : pd.DataFrame
        Must contain columns:
        - iata (FAA/IATA code)
        - lat
        - lon

    Returns
    -------
    pd.DataFrame
        Airport + timezone mapping
    """

    airport_tz = (
        airport_ref[['iata', 'lat', 'lon']]
        .drop_duplicates()
        .copy()
    )

    airport_tz['Timezone'] = airport_tz.apply(
        lambda row: tf.timezone_at(
            lng=row['lon'],
            lat=row['lat']
        ),
        axis=1
    )

    return airport_tz
```

```python
airports_loc = pd.read_csv("./input_data/Airport_LOC.csv")
airports_ref = pd.read_csv("./input_data/airports_data.csv")

airport_tz = build_airport_timezone_table(airports_ref)


```

### B2. Manual timezone fixes

Source: `timezone_conversion.ipynb` by Jasmine Wu.

Adds or corrects timezone mappings for airports that cannot be reliably assigned using the coordinate-based procedure.

```python
# 2. Add manual fixes
manual_tz = pd.DataFrame({
    "iata": [
        "SJU", "BQN", "PSE",
        "STT", "STX",
        "GUM",
        "YAK",
        "OGD",
        "XWA",
        "USA",
        "BIH",
        "SWO"
    ],
    "Timezone": [
        "America/Puerto_Rico",
        "America/Puerto_Rico",
        "America/Puerto_Rico",
        "America/St_Thomas",
        "America/St_Thomas",
        "Pacific/Guam",
        "America/Anchorage",
        "America/Denver",
        "America/Chicago",
        "America/New_York",
        "America/Los_Angeles",
        "America/Chicago"
    ]
})
```

### B3. Combine generated and manual timezone mappings

Source: `timezone_conversion.ipynb` by Jasmine Wu.

Combines the automatically generated mappings and manual corrections into the complete `airport_tz_full` reference table used for subsequent timezone conversions.

```python
def combine_timezone_tables(airport_tz, manual_tz):
    """
    Combine auto-generated and manual timezone mappings.
    Manual values overwrite missing ones.
    """

    airport_tz_full = pd.concat(
        [airport_tz, manual_tz],
        ignore_index=True
    )

    # Keep first non-null timezone per airport
    airport_tz_full = (
        airport_tz_full
        .sort_values(by='Timezone', na_position='last')
        .drop_duplicates(subset='iata', keep='first')
    )

    return airport_tz_full
```

```python

airport_tz_full = combine_timezone_tables(
    airport_tz,
    manual_tz
)

airport_tz_full.to_csv("./output_data/airport_tz_full.csv", index=False)

```

### B4. Convert local datetimes using the timezone corresponding to each airport

Source: `timezone_conversion.ipynb` by Jasmine Wu.

Converts local timestamps to UTC on a row-by-row basis using the timezone associated with each observation's airport, including appropriate daylight-saving-time offsets.


```python
# Step 3: Convert ActualDep to UTC
def convert_local_to_utc(
    data,
    airport_tz_table,
    airport_col="Origin", # this column can be departure, diverted to, or destination airports
    local_time_col="ActualDep", # change the local time column as needed to departure, diverted to, or destination local times
    local_output_col=None,
    utc_output_col=None,
    elapsed_col=None,
    est_arrival_output_col="EstArrival_UTC"
):
    df = data.copy()

    df = df.merge(
        airport_tz_table[["iata", "Timezone"]],
        left_on=airport_col,
        right_on="iata",
        how="left"
    )

    df[local_time_col] = pd.to_datetime(df[local_time_col])

    if local_output_col is None:
        local_output_col = f"{local_time_col}_local"

    if utc_output_col is None:
        utc_output_col = f"{local_time_col}_UTC"

    def attach_timezone(row):
        if pd.isna(row[local_time_col]) or pd.isna(row["Timezone"]):
            return pd.NaT

        return row[local_time_col].replace(
            tzinfo=ZoneInfo(row["Timezone"])
        )

    df[local_output_col] = df.apply(attach_timezone, axis=1)

    df[utc_output_col] = df[local_output_col].apply(
        lambda x: x.astimezone(ZoneInfo("UTC")) if pd.notna(x) else pd.NaT
    )

    if elapsed_col is not None:
        df[est_arrival_output_col] = (
            df[utc_output_col] +
            pd.to_timedelta(df[elapsed_col], unit="m")
        )

    return df
```

### B5. Usage examples: origin and first diversion airport

Source: `timezone_conversion.ipynb` by Jasmine Wu.

Demonstrates airport-specific timezone conversion for flight timestamps, including actual departure time at the origin airport and landing time at the first diversion airport.

```python
diverted_flights_utc = convert_local_to_utc(
    data=diverted_flights,
    airport_tz_table=airport_tz_full,
    airport_col="Origin", # this column can be departure, diverted to, or destination airports
    local_time_col="ActualDepTime" # change the local time column as needed to departure, diverted to, or destination local times
)

diverted_flights_utc = convert_local_to_utc(
    data=diverted_flights_utc,
    airport_tz_table=airport_tz_full,
    airport_col="Div1Airport", # this column can be departure, diverted to, or destination airports
    local_time_col="Div1ActualLanding" # change the local time column as needed to departure, diverted to, or destination local times
)
```

## C. Generate diversion dataset from Marketing Carrier On-Time Performance files (Python)

### C1. Find and combine monthly source files

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Identifies monthly Marketing Carrier On-Time Performance files, checks their column structures, reads them, and combines them into a single flight-level dataset covering the study period.

```python
# check that all files have the same column names
columns_set = []
for file in file_list:
    df = pd.read_csv(file, nrows=0)  
    columns_set.append(set(df.columns))

all_same_columns = all(columns == columns_set[0] for columns in columns_set)
print("All files have the same columns:", all_same_columns)

# read and concatenate one file at a time to avoid holding all in memory simultaneously
if all_same_columns:
    chunks = []
    for f in file_list:
        chunks.append(pd.read_csv(f))
    flights = pd.concat(chunks, ignore_index=True)
    del chunks  # free memory immediately after concat
    print("Combined DataFrame shape:", flights.shape)
else:
    print("Warning: Not all files have the same columns. Check differences before combining.")
```

### C2. Remove duplicate records and identify diverted flights

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Removes duplicate BTS flight records and identifies diverted flights based on the relationship between the scheduled destination and first diversion airport.

```python
# drop rows marked as duplicates
flights = flights.drop(flights[flights['Duplicate'] == 'Y'].index)

# filter diverted flights only
# must have a diversion 1 airport that is different from the original destination
diverted_flights = flights[
    (
        flights['Div1Airport'].notna() & 
        (flights['Div1Airport'] != flights['Dest'])
    )
].copy()

#drop columns that do not have any data
empty_columns = diverted_flights.columns[diverted_flights.isna().all()]
diverted_flights = diverted_flights.dropna(axis=1, how='all')
```

### C3. Save the reduced diversion dataset

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Removes fields that contain no information for the diverted-flight subset and saves a smaller intermediate dataset for more efficient downstream processing.

```python
# save diverted flights to a new CSV because this is the main data we will be working with and the original files were huge 
diverted_flights.to_csv('diverted_flights.csv', index=False)
```

### C4. Construct actual departure datetime with midnight rollover handling

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Combines the flight date and actual departure time to construct a complete departure datetime, with explicit handling of flights whose departure occurs after midnight relative to the scheduled flight date.

```python
# flight date is original date of flight, not calculating for delays
# calculate actual departure date and time

# convert to datetime for time calculations
diverted_flights['FlightDate'] = pd.to_datetime(diverted_flights['FlightDate'], errors='coerce')

# drop missing DepTime (cancelled flight)
diverted_flights['DepTime'] = diverted_flights['DepTime'].dropna()

# ActualDep: FlightDate + actual departure time (DepTime already reflects any delay)
diverted_flights['ActualDep'] = (
    diverted_flights['FlightDate'] +
    pd.to_timedelta(diverted_flights['DepTime'] // 100, unit='h') +
    pd.to_timedelta(diverted_flights['DepTime'] % 100, unit='m')
)

# scheduled departure for rollover reference
diverted_flights['sched_dep'] = (
    diverted_flights['FlightDate'] +
    pd.to_timedelta(diverted_flights['CRSDepTime'] // 100, unit='h') +
    pd.to_timedelta(diverted_flights['CRSDepTime'] % 100, unit='m')
)

# if ActualDep is a full day behind scheduled, it crossed midnight forward — add one day
diverted_flights.loc[
    (diverted_flights['ActualDep'] - diverted_flights['sched_dep']).dt.total_seconds() / 60 < -700,
    'ActualDep'
] += pd.Timedelta(days=1)

# if ActualDep is a full day ahead of scheduled, subtract one day
diverted_flights.loc[
    (diverted_flights['ActualDep'] - diverted_flights['sched_dep']).dt.total_seconds() / 60 > 700,
    'ActualDep'
] -= pd.Timedelta(days=1)

diverted_flights.drop(columns=['sched_dep'], inplace=True)

diverted_flights['ActualDepDate'] = diverted_flights['ActualDep'].dt.date
diverted_flights['ActualDepTime'] = diverted_flights['ActualDep'].dt.time
```

### C5. Construct scheduled arrival, estimated arrival, and first-diversion landing datetime

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Constructs the key arrival-side timestamps used in the diversion analysis, including scheduled arrival, estimated actual arrival at the original destination, and actual landing time at the first diversion airport, while accounting for date rollover.

```python
# 1. Scheduled Arrival
diverted_flights['ScheduledArrival'] = (
    diverted_flights['FlightDate'] +
    pd.to_timedelta(diverted_flights['CRSArrTime'] // 100, unit='h') +
    pd.to_timedelta(diverted_flights['CRSArrTime'] % 100, unit='m')
)
diverted_flights.loc[
    (diverted_flights['ScheduledArrival'] - diverted_flights['ActualDep']).dt.total_seconds() / 60 < -700,
    'ScheduledArrival'
] += pd.Timedelta(days=1)

# 2. Estimated Actual Arrival
diverted_flights['CRSElapsedTime'] = diverted_flights['CRSElapsedTime'].fillna(0)
diverted_flights['EstimatedActualArrival'] = (
    diverted_flights['ActualDep'] +
    pd.to_timedelta(diverted_flights['CRSElapsedTime'], unit='m')
)

# 3. Div1 Actual Landing
diverted_flights['Div1ActualLanding'] = (
    diverted_flights['ActualDep'] +
    pd.to_timedelta(
        (diverted_flights['Div1WheelsOn'] // 100 * 60 + diverted_flights['Div1WheelsOn'] % 100) -
        (diverted_flights['ActualDep'].dt.hour * 60 + diverted_flights['ActualDep'].dt.minute),
        unit='m'
    )
)
diverted_flights.loc[
    (diverted_flights['Div1ActualLanding'] - diverted_flights['ActualDep']).dt.total_seconds() < 0,
    'Div1ActualLanding'
] += pd.Timedelta(days=1)
```

### C6. Optional QC: compare reconstructed departure time against `DepDelay`

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Checks the reconstructed actual departure datetime against the departure delay reported in the source data to identify potential datetime-construction errors or unusual records.

```python
# just checking DepDelay should equal the difference between ActualDep and scheduled departure
check = (
    diverted_flights['ActualDep'] - 
    (diverted_flights['FlightDate'] + 
     pd.to_timedelta(diverted_flights['CRSDepTime'] // 100, unit='h') + 
     pd.to_timedelta(diverted_flights['CRSDepTime'] % 100, unit='m'))
).dt.total_seconds() / 60

diff = check - diverted_flights['DepDelay']
print("Difference distribution:")
print(diff.describe())
print("\nValue counts of rounded differences:")
print(diff.round().value_counts().head(20))
# the times are relatively similar, but enough to matter for date changes
```

### C7. Save analysis-ready diversion data

Source: `flightDiversion.ipynb` by Jun Luu and `flightDiversion_wu.ipynb` by Jasmine Wu.

Saves the processed diverted-flight dataset with the constructed timestamps and relevant flight and diversion attributes for subsequent event identification and analysis.

```python
# save diverted flights with new variables to a new CSV 
diverted_flights.to_csv('diverted_flights_analysis.csv', index=False)
```
