# Build the exit-preserving cohort panel.
#
# panel_build.R keeps only organizations that filed in every year 2019-2023,
# which is the right frame for measuring change but removes exactly the cases a
# failure test needs. This script takes the opposite frame: every organization
# that filed a full 990 for tax year 2019, followed through 2024 whether or not
# it kept filing. Rows exist only for years an organization filed, so a gap in
# the panel is the outcome of interest.
#
# No downloads are needed: panelize() reuses the CSVs already cached under
# <data root>/PANEL by panel_build.R (overwrite = FALSE).
#
# Run with: Rscript dev/panel/panel_build_exits.R

suppressMessages({
  library(data.table)
  library(bit64)
  library(panel990)
})

COHORT_YEAR <- 2019
YEARS       <- 2019:2024
TABLES      <- c("P00", "P08", "P09", "P10")

data_root <- Sys.getenv("FISCAL_DATA_DIR", unset = file.path(path.expand("~"), "Documents", "fiscal-data"))
efile_dir  <- file.path(data_root, "efile")
panel_dir  <- file.path(data_root, "PANEL")
msg <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- 1. the 2019 cohort ----------------------------------------------------

header <- file.path(efile_dir, sprintf("F9-P00-T00-HEADER-%d.CSV", COHORT_YEAR))
stopifnot(file.exists(header))
h <- fread(header, select = c("EIN2", "TAX_YEAR", "RETURN_TYPE", "RETURN_GROUP_X",
                              "RETURN_PARTIAL_X", "RETURN_AMENDED_X", "RETURN_TIME_STAMP",
                              "TAX_PERIOD_END_DATE", "F9_00_GRO_RCPT"), showProgress = FALSE)
h <- h[TAX_YEAR == COHORT_YEAR]
h <- as.data.table(panel_deduplicate(as.data.frame(h), id = "EIN2", year = "TAX_YEAR",
                                     group = "RETURN_GROUP_X", partial = "RETURN_PARTIAL_X",
                                     amended = "RETURN_AMENDED_X", timestamp = "RETURN_TIME_STAMP",
                                     verbose = FALSE))
cohort <- h[RETURN_TYPE == "990", unique(as.character(EIN2))]
msg("%d cohort: %s full-990 filers (of %s filings that year)", COHORT_YEAR,
    format(length(cohort), big.mark = ","), format(nrow(h), big.mark = ","))

cohort_index <- h[RETURN_TYPE == "990", .(EIN2 = as.character(EIN2),
                                          gross_receipts_2019 = as.numeric(F9_00_GRO_RCPT),
                                          period_end_2019 = TAX_PERIOD_END_DATE)]
fwrite(cohort_index, file.path(panel_dir, "ein2-cohort-2019.csv"))
saveRDS(cohort, file.path(panel_dir, "ein2-cohort-2019.rds"))

# ---- 2. panel over the cohort, gaps preserved ------------------------------

sfw <- create_sfw(name = "fiscal-exit-cohort-2019", entity = "EIN2", time = "TAX_YEAR",
                  record = "OBJECTID", source = data_source()$root)
sfw <- add_rule(sfw, name = "cohort_2019", type = "subset", subset = cohort)
sfw <- add_rule(sfw, name = "dedup_filings", type = "dedup", group = "RETURN_GROUP_X",
                partial = "RETURN_PARTIAL_X", amended = "RETURN_AMENDED_X",
                timestamp = "RETURN_TIME_STAMP")

panel <- panelize(sfw = sfw, tables = TABLES, years = YEARS, backend = "db",
                  cache = "retain", path = panel_dir, bmf = TRUE, overwrite = FALSE,
                  verbose = TRUE)
saveRDS(panel, file.path(panel_dir, "panel-cohort-2019.rds"))

dat <- panel_data(panel)
msg("cohort panel: %s rows, %s organizations", format(nrow(dat), big.mark = ","),
    format(length(unique(dat$EIN2)), big.mark = ","))
print(table(dat$TAX_YEAR, dat$RETURN_TYPE, useNA = "ifany"))

# ---- 3. presence matrix: who filed what, when ------------------------------

presence <- dcast(as.data.table(dat)[, .(EIN2, TAX_YEAR, RETURN_TYPE)],
                  EIN2 ~ TAX_YEAR, value.var = "RETURN_TYPE",
                  fun.aggregate = function(z) if (length(z)) z[1] else NA_character_)
setnames(presence, as.character(YEARS), paste0("form", YEARS))
missing_cohort <- setdiff(cohort, presence$EIN2)
if (length(missing_cohort)) {
  presence <- rbind(presence, data.table(EIN2 = missing_cohort), fill = TRUE)
}
presence[, years_filed := rowSums(!is.na(.SD)), .SDcols = paste0("form", YEARS)]
saveRDS(presence, file.path(panel_dir, "presence-cohort-2019.rds"))
fwrite(presence, file.path(panel_dir, "presence-cohort-2019.csv"))

msg("filings per organization across %d-%d:", min(YEARS), max(YEARS))
print(presence[, .N, by = years_filed][order(years_filed)])
msg("still filing in 2024: %.1f%%", 100 * mean(!is.na(presence$form2024)))
msg("written to %s", panel_dir)
