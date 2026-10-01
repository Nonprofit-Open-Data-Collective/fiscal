# Shared data preparation for the baseline-relative hypothesis tests
# (h6-revenue-shock.Rmd, h5-windfall-allocation.Rmd).
#
# Both documents measure a year against the organization's own trailing,
# inflation-adjusted average rather than against year t-1: H6 looks at years
# that came in below it, H5 at years that came in above it. They must use an
# identical baseline or their estimates cannot be compared, so the construction
# lives here rather than being copied into each document.
#
# Expects `params` in scope with: panel_file, baseline_file, ref_year,
# base_window, shock_threshold. Leaves behind:
#
#   p                 organization-years with a complete baseline, in constant
#                     `ref_year` dollars, carrying base_* columns, the gap
#                     measures, and the shock/windfall flags
#   filings_by_year   full-990 December filings available, by tax year
#   cpi_table         the deflator actually applied
#   deflator(), num(), iqr_sd(), ols()
#
# Sign conventions are deliberately NOT set here. A channel is "a source of the
# shortfall" in H6 and "a use of the windfall" in H5, which is the same quantity
# with opposite signs, so each document builds its own channel table from the
# base_* columns this script provides.

suppressMessages({
  library(data.table)
  library(bit64)
})

BW <- as.integer(params$base_window)
num <- function(x) { x <- as.numeric(x); x[is.na(x)] <- 0; x }
iqr_sd <- function(v) unname(diff(quantile(v, c(0.25, 0.75), na.rm = TRUE)) / 1.349)

ols <- function(y, W) {
  XtXi <- solve(crossprod(W))
  bhat <- drop(XtXi %*% crossprod(W, y))
  res  <- drop(y - W %*% bhat)
  V <- XtXi %*% crossprod(W * res) %*% XtXi * nrow(W) / (nrow(W) - ncol(W))
  list(b = bhat, se = sqrt(diag(V)))
}

# ---- CPI-U, annual average, 1982-84 = 100 ---------------------------------
# BLS, cross-checked against the Minneapolis Fed's published series. Tax years
# here are December year-ends, so an annual average is the right deflator.
CPI <- c(`2016` = 240.007, `2017` = 245.120, `2018` = 251.107, `2019` = 255.657,
         `2020` = 258.811, `2021` = 270.970, `2022` = 292.655, `2023` = 304.702,
         `2024` = 313.689)
deflator <- function(y) unname(CPI[as.character(params$ref_year)] / CPI[as.character(y)])
cpi_table <- data.table(tax_year = names(CPI), cpi_u = unname(CPI),
                        multiplier_to_ref = round(deflator(names(CPI)), 4))

# ---- 1. load and stack -----------------------------------------------------

.main <- as.data.table(readRDS(params$panel_file))
.base <- as.data.table(readRDS(params$baseline_file))

# Membership is defined on the 2019-2023 window, the same population H1-H5 use.
# The 2016-2018 years are extra history, never a filter: an organization that
# did not e-file then keeps its place and simply has no baseline for the early
# years.
.forms <- .main[, .(n990 = sum(RETURN_TYPE == "990"),
                    nez  = sum(RETURN_TYPE == "990EZ")), by = EIN2]
.stable990 <- .forms[n990 >= 5 & nez == 0, EIN2]

.keep   <- c("EIN2", "TAX_YEAR", "RETURN_TYPE", "TAX_PERIOD_END_DATE",
             grep("^F9_(08|09|10)_", names(.main), value = TRUE))
.common <- intersect(.keep, intersect(names(.main), names(.base)))
x <- rbind(.main[, .common, with = FALSE], .base[, .common, with = FALSE])
rm(.main, .base, .forms); invisible(gc())

x <- x[EIN2 %chin% .stable990 & RETURN_TYPE == "990" &
       substr(TAX_PERIOD_END_DATE, 6, 7) == "12"]
x[, TAX_YEAR := as.integer(TAX_YEAR)]

filings_by_year <- x[, .(filings = .N, organizations = uniqueN(EIN2)),
                     by = TAX_YEAR][order(TAX_YEAR)]

# ---- 2. blocks, revenue components, constant dollars -----------------------

blk <- function(when) {
  s <- function(f) num(x[[paste0(f, "_", when)]])
  data.table(
    cash   = s("F9_10_ASSET_CASH") + s("F9_10_ASSET_SAVING"),
    oca    = s("F9_10_ASSET_PLEDGE_NET") + s("F9_10_ASSET_ACC_NET") +
             s("F9_10_ASSET_INV_SALE") + s("F9_10_ASSET_EXP_PREPAID"),
    fixed  = s("F9_10_ASSET_LAND_BLDG_NET"),
    assets = s("F9_10_ASSET_TOT"),
    lt     = s("F9_10_LIAB_TAX_EXEMPT_BOND") + s("F9_10_LIAB_MTG_NOTE") + s("F9_10_LIAB_NOTE_UNSEC"),
    st     = s("F9_10_LIAB_ACC_PAYABLE") + s("F9_10_LIAB_GRANT_PAYABLE") + s("F9_10_LIAB_REV_DEFERRED"),
    liab   = s("F9_10_LIAB_TOT"),
    na     = s("F9_10_NAFB_TOT"))
}
b <- blk("BOY"); e <- blk("EOY")
b[, `:=`(other_assets = assets - cash - oca - fixed, other_liab = liab - lt - st)]
e[, `:=`(other_assets = assets - cash - oca - fixed, other_liab = liab - lt - st)]

# Seven revenue components that partition Part VIII's total exactly; the
# seventh is a residual, so the partition holds by construction.
gov       <- num(x$F9_08_REV_CONTR_GOVT_GRANT)
dues      <- num(x$F9_08_REV_CONTR_MEMBSHIP_DUE)
evtc      <- num(x$F9_08_REV_CONTR_FUNDR_EVNT)
events    <- evtc + num(x$F9_08_REV_OTH_EVNT_NET_TOT) + num(x$F9_08_REV_OTH_GAMING_NET_TOT)
contr_oth <- num(x$F9_08_REV_CONTR_TOT) - gov - dues - evtc
program   <- num(x$F9_08_REV_PROG_TOT_TOT)
invest    <- num(x$F9_08_REV_OTH_INVEST_INCOME_TOT) + num(x$F9_08_REV_OTH_INVEST_BOND_TOT) +
             num(x$F9_08_REV_OTH_SALE_GAIN_NET_TOT)

dat <- data.table(
  EIN2 = x$EIN2, year = x$TAX_YEAR,
  R = num(x$F9_08_REV_TOT_TOT), E = num(x$F9_09_EXP_TOT_TOT),
  gov = gov, dues = dues, events = events, contr_oth = contr_oth,
  program = program, invest = invest,
  program_expenses     = num(x$F9_09_EXP_TOT_PROG),
  admin_expenses       = num(x$F9_09_EXP_TOT_MGMT),
  fundraising_expenses = num(x$F9_09_EXP_TOT_FUNDR))
dat[, other := R - gov - dues - events - contr_oth - program - invest]
dat[, unallocated_expenses := E - program_expenses - admin_expenses - fundraising_expenses]

for (v in c("cash", "oca", "fixed", "other_assets", "lt", "st", "other_liab",
            "assets", "liab", "na")) {
  dat[[paste0("d_",   v)]] <- e[[v]] - b[[v]]   # movement during the year
  dat[[paste0("boy_", v)]] <- b[[v]]            # the stock it moved from
}
setnames(dat, c("d_oca", "boy_oca"), c("d_other_current", "boy_other_current"))
rm(b, e, x); invisible(gc())

# Constant dollars, applied to every dollar quantity at once and before any
# baseline is formed.
DOLLARS <- setdiff(names(dat), c("EIN2", "year"))
dat[, (DOLLARS) := lapply(.SD, function(v) v * deflator(year)), .SDcols = DOLLARS]

# ---- 3. trailing baselines -------------------------------------------------
# A trailing mean, not panel990::panel_smooth(): that helper centres its window,
# so a three-year smooth would include the current year and the event would
# partly define its own baseline.

BASED <- c("R", "E", "gov", "dues", "events", "contr_oth", "program", "invest", "other",
           "program_expenses", "admin_expenses", "fundraising_expenses", "unallocated_expenses",
           "d_cash", "d_other_current", "d_fixed", "d_other_assets",
           "d_lt", "d_st", "d_other_liab", "d_assets", "d_na")

stopifnot(anyDuplicated(dat, by = c("EIN2", "year")) == 0L)
setorder(dat, EIN2, year)
for (k in seq_len(BW)) {
  dat[, paste0("y", k) := shift(year, k), by = EIN2]
  dat[, paste0("l", k, "_", BASED) := shift(.SD, k), by = EIN2, .SDcols = BASED]
}
# All BW prior tax years must be present and consecutive.
ok <- rep(TRUE, nrow(dat))
for (k in seq_len(BW)) {
  yk <- dat[[paste0("y", k)]]
  ok <- ok & !is.na(yk) & yk == dat$year - k
}
dat[, has_base := ok]
for (v in BASED) {
  acc <- dat[[paste0("l1_", v)]]
  if (BW > 1) for (k in 2:BW) acc <- acc + dat[[paste0("l", k, "_", v)]]
  dat[[paste0("base_", v)]] <- acc / BW
}

p <- dat[has_base == TRUE & base_R > 0 & base_E > 0 & boy_assets > 0]
rm(dat); invisible(gc())

p[, gap := (R - base_R) / base_R]   # negative below baseline, positive above

# Alternative baselines, used only in the sensitivity tables. Nominal values are
# recovered by undoing the deflator year by year, so the nominal comparison uses
# exactly the same filings as the real one.
p[, R_nom      := R / deflator(year)]
p[, base_R_nom := (l1_R / deflator(year - 1L) + l2_R / deflator(year - 2L) +
                   l3_R / deflator(year - 3L)) / 3]
p[, base_R2    := (l1_R + l2_R) / 2]
p[, gap_nom := ifelse(base_R_nom > 0, (R_nom - base_R_nom) / base_R_nom, NA_real_)]
p[, gap2    := ifelse(base_R2    > 0, (R - base_R2)        / base_R2,    NA_real_)]
p[, gap_t1  := ifelse(l1_R       > 0, (R - l1_R)           / l1_R,       NA_real_)]

TH <- params$shock_threshold
p[, shock       := gap    <= -TH]
p[, windfall    := gap    >=  TH]
p[, shock_t1    := !is.na(gap_t1) & gap_t1 <= -TH]
p[, windfall_t1 := !is.na(gap_t1) & gap_t1 >=  TH]
# How far the PRIOR year sat from the same baseline. Large and positive means a
# t-1 "shock" may be windfall reversion; large and negative means a t-1
# "windfall" may be recovery from a bad year.
p[, prior_windfall := (l1_R - base_R) / base_R]
p[, months_cash_boy := boy_cash / (base_E / 12)]
p[, size_band := cut(base_R, c(-Inf, 1e5, 5e5, 2e6, 1e7, Inf),
                     labels = c("under $100k", "$100k-500k", "$500k-2m", "$2m-10m", "$10m+"))]

# Largest absolute block deviation, scaled. Sign-free, so both documents trim on
# the same rows: blocks offset each other, and an organization-year with a huge
# swing into one block and out of another has a small net movement but enormous
# leverage over any single block's coefficient.
p[, max_abs_blk := pmax(abs(d_cash - base_d_cash), abs(d_other_current - base_d_other_current),
                        abs(d_fixed - base_d_fixed), abs(d_other_assets - base_d_other_assets),
                        abs(d_lt - base_d_lt), abs(d_st - base_d_st),
                        abs(d_other_liab - base_d_other_liab)) / base_E]
