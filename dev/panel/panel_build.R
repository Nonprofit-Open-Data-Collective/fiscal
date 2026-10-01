# Build the 2019-2024 fiscal panel used by the hypothesis tests in
# dev/health-index/ (see the ratio-correlation memo for H1-H5).
#
# The stable population is every organization that filed in all five years
# 2019-2023. Tax year 2024 is included where present but is NOT required for
# membership: that file is still filling in (fiscal-year returns for FY2025
# arrive through 2026), so requiring it would drop large and June-year-end
# filers disproportionately.
#
# Steps
#   1. Header (P00) files for each year, downloaded to the data root.
#   2. One filing per organization-year (panel990::panel_deduplicate).
#   3. Intersection tests -> the EIN2 sampling frame, saved for replication.
#   4. A sample frame (sfw) carrying that EIN2 subset plus the dedup rule.
#   5. panelize() with the DuckDB backend, BMF fields appended.
#   6. Panel classification and imputation of single-year gaps.
#
# Everything is written to <data root>/PANEL, outside the repo. Run with:
#   Rscript dev/panel/panel_build.R

suppressMessages({
  library(data.table)
  library(bit64)
  library(panel990)
})

# ---- configuration ---------------------------------------------------------

YEARS_STABLE  <- 2019:2023          # membership requires a filing in each
YEAR_OPTIONAL <- 2024               # included when present
YEARS_ALL     <- c(YEARS_STABLE, YEAR_OPTIONAL)
TABLES        <- c("P00", "P08", "P09", "P10")   # header, revenue, expenses, balance sheet

data_root <- Sys.getenv("FISCAL_DATA_DIR", unset = file.path(path.expand("~"), "Documents", "fiscal-data"))
efile_dir  <- file.path(data_root, "efile")
panel_dir  <- file.path(data_root, "PANEL")
dir.create(efile_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)

HEADER_COLS <- c("EIN2", "TAX_YEAR", "RETURN_TYPE", "RETURN_GROUP_X", "RETURN_PARTIAL_X",
                 "RETURN_AMENDED_X", "RETURN_TIME_STAMP", "TAX_PERIOD_END_DATE",
                 "F9_00_GRO_RCPT")

msg <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- 1. header files -------------------------------------------------------

options(timeout = max(3600, getOption("timeout")))   # these files are ~300 MB

# Size on the server, so a truncated download is not mistaken for a good file.
remote_size <- function(url) {
  h <- tryCatch(curlGetHeaders(url), error = function(e) character())
  n <- grep("^content-length:", h, ignore.case = TRUE, value = TRUE)
  if (!length(n)) return(NA_real_)
  as.numeric(sub("^[^:]+:\\s*", "", trimws(tail(n, 1))))
}

header_path <- function(year) {
  f <- file.path(efile_dir, sprintf("F9-P00-T00-HEADER-%d.CSV", year))
  url <- paste0(data_source()$root, sprintf("F9-P00-T00-HEADER-%d.CSV", year))
  want <- remote_size(url)
  if (file.exists(f) && !is.na(want) && file.size(f) != want) {
    msg("%d: local header is %s of %s bytes; re-downloading", year,
        format(file.size(f), big.mark = ","), format(want, big.mark = ","))
    unlink(f)
  }
  if (!file.exists(f)) {
    msg("downloading %s", url)
    utils::download.file(url, f, mode = "wb", quiet = TRUE)
    if (!is.na(want) && file.size(f) != want)
      stop(sprintf("download incomplete for %d: %s of %s bytes", year,
                   format(file.size(f), big.mark = ","), format(want, big.mark = ",")))
  }
  f
}

# ---- 2. one filing per organization-year -----------------------------------

year_filings <- function(year) {
  d <- fread(header_path(year), select = HEADER_COLS, showProgress = FALSE)
  d <- d[TAX_YEAR == year]
  before <- nrow(d)
  d <- as.data.table(panel_deduplicate(
    as.data.frame(d), id = "EIN2", year = "TAX_YEAR", group = "RETURN_GROUP_X",
    partial = "RETURN_PARTIAL_X", amended = "RETURN_AMENDED_X",
    timestamp = "RETURN_TIME_STAMP", verbose = FALSE))
  msg("%d: %s filings -> %s organizations after dedup", year,
      format(before, big.mark = ","), format(nrow(d), big.mark = ","))
  d[, .(EIN2 = as.character(EIN2), TAX_YEAR, form = RETURN_TYPE,
        gross_receipts = as.numeric(F9_00_GRO_RCPT),
        period_end = TAX_PERIOD_END_DATE)]
}

filings <- lapply(YEARS_ALL, year_filings)
names(filings) <- as.character(YEARS_ALL)
eins <- lapply(filings, function(d) unique(d$EIN2))

# ---- 3. intersection tests -------------------------------------------------

# Cumulative intersection: how many organizations survive each added year.
cumulative <- Reduce(intersect, eins[as.character(YEARS_STABLE)], accumulate = TRUE)
intersection_report <- data.table(
  through_year = YEARS_STABLE,
  organizations_that_year = vapply(eins[as.character(YEARS_STABLE)], length, 0L),
  in_every_year_so_far = vapply(cumulative, length, 0L))
intersection_report[, share_of_2019 := in_every_year_so_far / in_every_year_so_far[1]]
print(intersection_report)

stable_eins <- cumulative[[length(cumulative)]]
frame_index <- data.table(EIN2 = sort(stable_eins))
frame_index[, in_2024 := EIN2 %in% eins[["2024"]]]

# Diagnostics carried alongside the frame: 2019 form type and size.
frame_index <- merge(frame_index, filings[["2019"]][, .(EIN2, form_2019 = form,
                                                        gross_receipts_2019 = gross_receipts,
                                                        period_end_2019 = period_end)],
                     by = "EIN2", all.x = TRUE)
frame_index[, size_2019 := cut(gross_receipts_2019, c(-Inf, 1e5, 1e6, 1e7, Inf),
                               labels = c("under $100k", "$100k-$1m", "$1m-$10m", "$10m+"))]

msg("stable population (filed 2019-2023): %s organizations; %s also filed in 2024 (%.1f%%)",
    format(nrow(frame_index), big.mark = ","), format(sum(frame_index$in_2024), big.mark = ","),
    100 * mean(frame_index$in_2024))
print(frame_index[, .N, by = .(form_2019, in_2024)][order(form_2019, in_2024)])
print(frame_index[, .N, by = .(size_2019, in_2024)][order(size_2019, in_2024)])

fwrite(frame_index, file.path(panel_dir, "ein2-stable-2019-2023.csv"))
fwrite(intersection_report, file.path(panel_dir, "intersection-report.csv"))
saveRDS(stable_eins, file.path(panel_dir, "ein2-stable-2019-2023.rds"))

# ---- 4. sample frame -------------------------------------------------------

sfw <- create_sfw(name = "fiscal-health-2019-2024", entity = "EIN2", time = "TAX_YEAR",
                  record = "OBJECTID", source = data_source()$root)
# The subset rule is the sampling frame: panelize() pushes it down to read time.
sfw <- add_rule(sfw, name = "stable_2019_2023", type = "subset", subset = stable_eins)
sfw <- add_rule(sfw, name = "dedup_filings", type = "dedup", group = "RETURN_GROUP_X",
                partial = "RETURN_PARTIAL_X", amended = "RETURN_AMENDED_X",
                timestamp = "RETURN_TIME_STAMP")
print(get_rules(sfw))

# ---- 5. panelize -----------------------------------------------------------

panel <- panelize(sfw = sfw, tables = TABLES, years = YEARS_ALL,
                  backend = "db", cache = "retain", path = panel_dir,
                  bmf = TRUE, verbose = TRUE)
saveRDS(panel, file.path(panel_dir, "panel-2019-2024.rds"))

dat <- panel_data(panel)
msg("panel: %s rows, %s organizations, %s columns",
    format(nrow(dat), big.mark = ","), format(length(unique(dat$EIN2)), big.mark = ","),
    format(ncol(dat), big.mark = ","))
print(table(dat$TAX_YEAR, dat$RETURN_TYPE, useNA = "ifany"))

# ---- 6. classification and imputation --------------------------------------

sfw <- classify_panel(sfw, dat)
saveRDS(sfw, file.path(panel_dir, "sample-frame.rds"))

fin_vars <- intersect(financial_fields("core"), names(dat))
msg("imputing single-year gaps in %d financial fields", length(fin_vars))
dat_imputed <- panel_impute(dat, types = "persistent", method = "interpolate",
                            max_gap_size = 1, vars = fin_vars, as_integers = TRUE)
saveRDS(dat_imputed, file.path(panel_dir, "panel-2019-2024-imputed.rds"))

print(panel_describe(dat_imputed))
msg("written to %s", panel_dir)
