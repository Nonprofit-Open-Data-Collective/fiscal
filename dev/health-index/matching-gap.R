# Matching-gap indicator: debt beyond what an organization's asset mix supports.
#
# Benchmarks come from the H1 (long-term matching) and H2 (short-term matching)
# models tested in matching-hypotheses-2023.Rmd. See that document and the
# ratio-correlation memo for the reasoning and the pre-specified plan.

library(splines)

fix64 <- function(x) {
  if (inherits(x, "integer64")) return(as.numeric(x))
  if (is.double(x)) {
    nz <- x[!is.na(x) & x != 0]
    if (length(nz) && all(abs(nz) < 1e-300)) {
      class(x) <- "integer64"
      return(as.numeric(x))
    }
  }
  x
}

#' Part X lines at one balance-sheet date, plus the filing year's flows.
#' Rows are all full-990 filers, in file order, so EOY and BOY frames align.
gap_lines <- function(d, when = c("EOY", "BOY")) {
  when <- match.arg(when)
  requireNamespace("bit64", quietly = TRUE)   # registers as.numeric() for integer64 columns
  d <- as.data.frame(d)
  d <- d[d$RETURN_TYPE == "990", ]
  has <- function(f) f %in% names(d)
  fin <- function(f) {
    x <- if (has(f)) suppressWarnings(as.numeric(fix64(d[[f]]))) else rep(0, nrow(d))
    x[is.na(x)] <- 0
    x
  }
  at <- function(stem) fin(paste0(stem, "_", when))
  una_raw <- at("F9_10_NAFB_UNRESTRICT")
  res_raw <- at("F9_10_NAFB_RESTRICT")
  m <- data.frame(
    ein   = if (has("EIN2")) as.character(d$EIN2) else NA_character_,
    cash  = at("F9_10_ASSET_CASH") + at("F9_10_ASSET_SAVING"),
    ncca  = at("F9_10_ASSET_PLEDGE_NET") + at("F9_10_ASSET_ACC_NET") +
            at("F9_10_ASSET_INV_SALE") + at("F9_10_ASSET_EXP_PREPAID"),
    fixed = at("F9_10_ASSET_LAND_BLDG_NET"),
    A     = at("F9_10_ASSET_TOT"),
    L     = at("F9_10_LIAB_TOT"),
    lt    = at("F9_10_LIAB_TAX_EXEMPT_BOND") + at("F9_10_LIAB_MTG_NOTE") + at("F9_10_LIAB_NOTE_UNSEC"),
    st    = at("F9_10_LIAB_ACC_PAYABLE") + at("F9_10_LIAB_GRANT_PAYABLE") + at("F9_10_LIAB_REV_DEFERRED"),
    una   = ifelse(una_raw == 0 & res_raw == 0, at("F9_10_NAFB_TOT"), una_raw),
    E     = fin("F9_09_EXP_TOT_TOT"),
    R     = fin("F9_08_REV_TOT_TOT"),
    stringsAsFactors = FALSE)
  for (v in c("cash", "ncca", "fixed", "lt", "st")) m[[v]] <- pmax(m[[v]], 0)
  m
}

#' Design matrix: the three asset blocks plus size controls. Pass `basis` from
#' a previous call to evaluate new data on the same spline bases.
gap_design <- function(m, basis = NULL) {
  other <- pmax(m$A - m$cash - m$ncca - m$fixed, 0)
  if (is.null(basis)) {
    basis <- list(E = ns(log(m$E), df = 4), O = ns(log1p(other), df = 4))
  }
  sE <- predict(basis$E, log(m$E)); colnames(sE) <- paste0("size_E", 1:4)
  sO <- predict(basis$O, log1p(other)); colnames(sO) <- paste0("size_other", 1:4)
  X <- cbind(intercept = 1,
             log_fixed = log1p(m$fixed), any_fixed = as.numeric(m$fixed > 0),
             log_cash  = log1p(m$cash),  any_cash  = as.numeric(m$cash > 0),
             log_ncca  = log1p(m$ncca),  any_ncca  = as.numeric(m$ncca > 0),
             sE, sO, no_other = as.numeric(other == 0))
  list(X = X, basis = basis)
}

#' Two-part benchmark: logistic for any debt, OLS on log amount with Duan smearing.
fit_benchmark <- function(y, X) {
  any <- y > 0
  logit <- suppressWarnings(glm.fit(X, as.numeric(any), family = binomial()))
  amount <- lm.fit(X[any, , drop = FALSE], log(y[any]))
  list(b_any = logit$coefficients, b_amount = amount$coefficients,
       smear = mean(exp(amount$residuals)), converged = logit$converged,
       n = length(y), n_positive = sum(any))
}

#' One-part alternative: Poisson pseudo-maximum likelihood on dollars, zeros
#' included. E[y | x] = exp(x'b) is estimated directly, so there is no
#' retransformation step, and with an intercept predicted totals equal actual
#' totals. Added after the two-part benchmark failed calibration.
fit_benchmark_ppml <- function(y, X) {
  scale <- 1e6
  f <- suppressWarnings(glm.fit(X, y / scale, family = quasipoisson(), control = list(maxit = 100)))
  list(type = "ppml", b = f$coefficients, scale = scale, converged = f$converged,
       n = length(y), n_positive = sum(y > 0), smear = NA_real_)
}

predict_benchmark <- function(fit, X) {
  if (identical(fit$type, "ppml")) return(exp(drop(X %*% fit$b)) * fit$scale)
  plogis(drop(X %*% fit$b_any)) * exp(drop(X %*% fit$b_amount)) * fit$smear
}

#' Fit both benchmarks on `m` and return the gaps for `m`.
matching_gap <- function(m, type = c("two_part", "ppml")) {
  type <- match.arg(type)
  fitter <- if (type == "ppml") fit_benchmark_ppml else fit_benchmark
  D <- gap_design(m)
  fits <- list(long = fitter(m$lt, D$X), short = fitter(m$st, D$X))
  list(fits = fits, basis = D$basis, gaps = apply_gap(m, fits, D$basis))
}

#' Apply fitted benchmarks to any lines frame (for example, beginning of year).
apply_gap <- function(m, fits, basis) {
  X <- gap_design(m, basis)$X
  pred_long  <- predict_benchmark(fits$long, X)
  pred_short <- predict_benchmark(fits$short, X)
  data.frame(
    ein = m$ein,
    pred_long = pred_long, pred_short = pred_short,
    gap_long  = (m$lt - pred_long) / m$A,
    gap_short = (m$st - pred_short) / m$A,
    gap_total = (m$lt + m$st - pred_long - pred_short) / m$A,
    debt_assets = m$L / m$A,
    stringsAsFactors = FALSE)
}
