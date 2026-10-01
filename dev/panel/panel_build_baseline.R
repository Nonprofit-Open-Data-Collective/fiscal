# Extend the fiscal panel backwards by three years (tax years 2016-2018) so
# that a shock in year t can be measured against a trailing three-year
# baseline rather than against year t-1 alone.
#
# Why. Measuring a shock as a fall from year t-1 confuses a genuine shock with
# the reversion leg of a windfall: an organization that lands a large grant or
# bequest in t-1 and returns to normal in t shows a >20% "drop" while nothing
# has gone wrong. A trailing three-year average is not moved much by a single
# windfall year, so the fall is measured against what the organization
# normally raises.
#
# Why the same four tables as panel_build.R. An earlier version of this script
# pulled revenue only, on the reasoning that a baseline is a revenue baseline.
# That is true of the shock INDICATOR but not of the estimates: the absorption
# identity is stated relative to baseline expenses, and the balance-sheet
# channels are movements relative to their own baseline movements. Pulling
# P09/P10 as well costs about 1.3 GB and buys a full three-year baseline for
# every shock year from 2019 on, including a clean pre-COVID window
# (2019 shocks against a 2016-2018 baseline) that the 2019-2024 panel alone
# cannot produce.
#
# Why membership is unchanged. The stable population stays "filed in every year
# 2019-2023", exactly as panel_build.R defines it. The three added years are
# OPTIONAL: an organization that did not e-file in 2016-2018 keeps its place in
# the panel and simply has no baseline for the early shock years. Requiring
# 2016-2018 filings would restrict the sample to early e-filers -- e-filing
# only became mandatory under the Taxpayer First Act, for tax years beginning
# on or after July 2019, and the 2016 header file is ~13% smaller than 2019's
# -- biasing the panel toward large, long-lived organizations on precisely the
# dimension H6 measures.
#
# Writes <data root>/PANEL/panel-2016-2018.rds. Run with:
#   Rscript dev/panel/panel_build_baseline.R

suppressMessages({
  library(data.table)
  library(bit64)
  library(panel990)
})

YEARS_BASE <- 2016:2018
TABLES     <- c("P00", "P08", "P09", "P10")   # same scope as panel_build.R

data_root <- Sys.getenv("FISCAL_DATA_DIR", unset = file.path(path.expand("~"), "Documents", "fiscal-data"))
panel_dir <- file.path(data_root, "PANEL")

msg <- function(...) cat(sprintf(...), "\n", sep = "")
options(timeout = max(3600, getOption("timeout")))

# ---- 1. the existing sampling frame ---------------------------------------

stable_path <- file.path(panel_dir, "ein2-stable-2019-2023.rds")
if (!file.exists(stable_path))
  stop("run dev/panel/panel_build.R first; missing ", stable_path)
stable_eins <- readRDS(stable_path)
msg("stable population from panel_build.R: %s organizations",
    format(length(stable_eins), big.mark = ","))

# ---- 2. pull the baseline years -------------------------------------------

sfw <- create_sfw(name = "fiscal-health-baseline-2016-2018", entity = "EIN2",
                  time = "TAX_YEAR", record = "OBJECTID", source = data_source()$root)
sfw <- add_rule(sfw, name = "stable_2019_2023", type = "subset", subset = stable_eins)
sfw <- add_rule(sfw, name = "dedup_filings", type = "dedup", group = "RETURN_GROUP_X",
                partial = "RETURN_PARTIAL_X", amended = "RETURN_AMENDED_X",
                timestamp = "RETURN_TIME_STAMP")

panel <- panelize(sfw = sfw, tables = TABLES, years = YEARS_BASE,
                  backend = "db", cache = "retain", path = panel_dir,
                  bmf = FALSE, verbose = TRUE)

dat <- as.data.table(panel_data(panel))
msg("baseline pull: %s rows, %s organizations, %s columns",
    format(nrow(dat), big.mark = ","), format(uniqueN(dat$EIN2), big.mark = ","),
    format(ncol(dat), big.mark = ","))

# ---- 3. keep the keys and the financial blocks -----------------------------

KEY  <- c("EIN2", "TAX_YEAR", "RETURN_TYPE", "TAX_PERIOD_END_DATE")
keep <- c(intersect(KEY, names(dat)),
          grep("^F9_(08|09|10)_", names(dat), value = TRUE))
out  <- dat[, ..keep]
out[, EIN2 := as.character(EIN2)]
msg("retained %d columns (%d financial fields)", ncol(out), ncol(out) - length(intersect(KEY, names(dat))))

# Coverage: how much of the stable frame has each baseline year on a full 990,
# and how much has all three. This is the denominator to quote whenever an
# early-window estimate is read.
cov_year <- out[RETURN_TYPE == "990", .(organizations = uniqueN(EIN2)), by = TAX_YEAR][order(TAX_YEAR)]
cov_year[, share_of_stable_frame := round(organizations / length(stable_eins), 3)]
print(cov_year)

full3 <- out[RETURN_TYPE == "990", .N, by = EIN2][N == 3L, EIN2]
msg("organizations with a full 990 in all three baseline years: %s (%.1f%% of the frame)",
    format(length(full3), big.mark = ","), 100 * length(full3) / length(stable_eins))

fwrite(cov_year, file.path(panel_dir, "baseline-coverage-2016-2018.csv"))
saveRDS(out, file.path(panel_dir, "panel-2016-2018.rds"))
msg("written to %s", file.path(panel_dir, "panel-2016-2018.rds"))
