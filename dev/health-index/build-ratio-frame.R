# Build the rules-applied, imputed ratio frame and its transformed versions.
#
# Mirrors the measurement model in vignettes/variance-preserving-normalization.Rmd
# so dimension discovery can be run on any e-file extract (e.g. a full tax year),
# not just the bundled dat10k sample. See that vignette for the reasoning behind
# every rule and imputation.

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

pct_rank    <- function(x) { r <- rank(x, na.last = "keep"); (r - 0.5) / sum(!is.na(x)) }
rank_normal <- function(x) qnorm(pct_rank(x))
fill_median <- function(x) { x[is.na(x)] <- median(x, na.rm = TRUE); x }

winsor <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[[1]]), q[[2]])
}

winsor_normalize <- function(x, p = 0.01) {
  w <- winsor(x, p)
  sk <- psych::skew(w)
  if (sk > 1) {
    s <- median(abs(w[w != 0]))
    w <- asinh(w / s)
  } else if (sk < -1) {
    r <- max(w) - w
    s <- median(r[r != 0])
    w <- -asinh(r / s)
  }
  as.numeric(scale(w))
}

# Reusable version of winsor_normalize(): fit stores every parameter
# (percentile bounds, skew direction, asinh scale, reflection anchor, mean, sd)
# so a later year can be placed on the reference-year scale.
fit_winsor_normalize <- function(df, p = 0.01) {
  specs <- lapply(names(df), function(v) {
    x <- df[[v]]
    q <- stats::quantile(x, c(p, 1 - p), na.rm = TRUE, names = FALSE)
    w <- pmin(pmax(x, q[1]), q[2])
    sk <- psych::skew(w)
    type <- if (sk > 1) "asinh_right" else if (sk < -1) "asinh_left" else "none"
    anchor <- max(w, na.rm = TRUE)
    s <- switch(type,
      asinh_right = stats::median(abs(w[w != 0]), na.rm = TRUE),
      asinh_left  = { r <- anchor - w; stats::median(r[r != 0], na.rm = TRUE) },
      none        = NA_real_)
    t <- .transform_spec(w, type, s, anchor)
    data.frame(variable = v, lo = q[1], hi = q[2], skew_winsorized = sk, type = type,
               scale_s = s, anchor = anchor, mean = mean(t, na.rm = TRUE),
               sd = stats::sd(t, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  do.call(rbind, specs)
}

.transform_spec <- function(w, type, s, anchor) {
  switch(type,
    asinh_right = asinh(w / s),
    asinh_left  = -asinh((anchor - w) / s),
    none        = w)
}

apply_winsor_normalize <- function(df, spec) {
  missing_vars <- setdiff(spec$variable, names(df))
  if (length(missing_vars)) stop("Missing variables: ", paste(missing_vars, collapse = ", "))
  out <- lapply(seq_len(nrow(spec)), function(i) {
    sp <- spec[i, ]
    w <- pmin(pmax(df[[sp$variable]], sp$lo), sp$hi)
    (.transform_spec(w, sp$type, sp$scale_s, sp$anchor) - sp$mean) / sp$sd
  })
  stats::setNames(as.data.frame(out), spec$variable)
}

# Stochastic conditional (row-plus-column) imputation on the normal-score scale.
cond_impute <- function(target, predictors, miss) {
  y   <- rank_normal(target)
  X   <- as.data.frame(lapply(predictors, rank_normal))
  fit <- stats::lm(y ~ ., data = cbind(y = y, X)[!miss, ])
  z   <- stats::predict(fit, newdata = X[miss, , drop = FALSE]) +
         stats::rnorm(sum(miss), 0, summary(fit)$sigma)
  out <- target
  out[miss] <- stats::quantile(target[!miss], stats::pnorm(z), names = FALSE, na.rm = TRUE)
  out
}

# Symmetric (arc) growth: change / mean(|new|, |old|). Bounded in [-2, 2];
# defined whenever either value is nonzero; 0 when both are zero.
sym_growth <- function(new, old) {
  ifelse(new == 0 & old == 0, 0, (new - old) / ((abs(new) + abs(old)) / 2))
}

#' @param d An e-file data frame (P00, P01, P08, P09, P10 fields).
#' @param growth Add one-year revenue and asset growth (`rev_growth`,
#'   `asset_growth`). Both come from the same return: Part I current- vs.
#'   prior-year revenue, and Part X end- vs. beginning-of-year total assets.
#' @param prior Optional prior-year e-file extract (EIN2 + F9_01_REV_TOT_CY).
#'   Used only to fill prior-year revenue that the current return leaves blank.
#' @return list(components, rules, frame, imputed_share, ein)
build_ratio_frame <- function(d, seed = 2023, growth = FALSE, prior = NULL) {
  set.seed(seed)
  requireNamespace("bit64", quietly = TRUE)
  d <- as.data.frame(d)
  d <- d[d$RETURN_TYPE == "990", ]
  fin <- function(f) {
    x <- if (f %in% names(d)) suppressWarnings(as.numeric(fix64(d[[f]]))) else rep(0, nrow(d))
    x[is.na(x)] <- 0
    x
  }
  k <- data.frame(
    cash    = fin("F9_10_ASSET_CASH_EOY") + fin("F9_10_ASSET_SAVING_EOY"),
    recv    = fin("F9_10_ASSET_PLEDGE_NET_EOY") + fin("F9_10_ASSET_ACC_NET_EOY"),
    othca   = fin("F9_10_ASSET_INV_SALE_EOY") + fin("F9_10_ASSET_EXP_PREPAID_EOY"),
    ap      = fin("F9_10_LIAB_ACC_PAYABLE_EOY"),
    cl      = fin("F9_10_LIAB_ACC_PAYABLE_EOY") + fin("F9_10_LIAB_GRANT_PAYABLE_EOY"),
    A = fin("F9_10_ASSET_TOT_EOY"), L = fin("F9_10_LIAB_TOT_EOY"),
    NA_eoy = fin("F9_10_NAFB_TOT_EOY"), NA_boy = fin("F9_10_NAFB_TOT_BOY"),
    UNA = fin("F9_10_NAFB_UNRESTRICT_EOY"), RES = fin("F9_10_NAFB_RESTRICT_EOY"),
    land = fin("F9_10_ASSET_LAND_BLDG_NET_EOY"), mtg = fin("F9_10_LIAB_MTG_NOTE_EOY"),
    R = fin("F9_08_REV_TOT_TOT"), E = fin("F9_09_EXP_TOT_TOT"), dep = fin("F9_09_EXP_DEPREC_TOT"),
    progrev = fin("F9_08_REV_PROG_TOT_TOT"),
    P = fin("F9_09_EXP_TOT_PROG"), M = fin("F9_09_EXP_TOT_MGMT"), Fu = fin("F9_09_EXP_TOT_FUNDR"))
  if (growth) {
    # Prior-year revenue keeps NA (blank = not reported, e.g. first-year filers).
    k$R_cy  <- fin("F9_01_REV_TOT_CY")
    k$R_py  <- if ("F9_01_REV_TOT_PY" %in% names(d))
      suppressWarnings(as.numeric(fix64(d[["F9_01_REV_TOT_PY"]]))) else NA_real_
    k$A_boy <- fin("F9_10_ASSET_TOT_BOY")
    k$R_py_source <- ifelse(is.na(k$R_py), NA_character_, "return")
    if (!is.null(prior)) {
      prior <- as.data.frame(prior)
      prior <- prior[!duplicated(prior$EIN2), ]
      prev_rev <- suppressWarnings(as.numeric(fix64(prior$F9_01_REV_TOT_CY)))
      fill <- prev_rev[match(d$EIN2, prior$EIN2)]
      use <- is.na(k$R_py) & !is.na(fill)
      k$R_py[use] <- fill[use]
      k$R_py_source[use] <- "prior-year return"
    }
  }
  keep <- k$E > 0
  k <- k[keep, ]
  k$ca   <- k$cash + k$recv + k$othca
  k$una  <- ifelse(k$UNA == 0 & k$RES == 0, k$NA_eoy, k$UNA)
  k$fsum <- k$P + k$M + k$Fu
  k$opE  <- ifelse(k$E - k$dep > 0, k$E - k$dep, k$E)

  rules <- with(k, data.frame(
    current        = ifelse(cl > 0, ca / cl, NA),
    months_cash    = (cash + recv) / (opE / 12),
    liquid_months  = (una - land + mtg) / (E / 12),
    cash_assets    = ifelse(A > 0, pmin(cash / A, 1), NA),
    debt_assets    = ifelse(A > 0, L / A, ifelse(L > 0, NA, 0)),
    debt_una       = ifelse(L > 0, L / (L + pmax(una, 0)), 0),
    payables_una   = ifelse(ap > 0, ap / (ap + pmax(una, 0)), 0),
    surplus_margin = (R - E) / pmax(R, E),
    return_assets  = ifelse(A > 0, (R - E) / A, NA),
    na_growth      = ifelse(NA_eoy == 0 & NA_boy == 0, 0,
                            (NA_eoy - NA_boy) / ((abs(NA_eoy) + abs(NA_boy)) / 2)),
    self_suff      = progrev / E,
    prog_exp       = ifelse(fsum > 0, P / fsum, NA),
    admin_exp      = ifelse(fsum > 0, M / fsum, NA),
    fundr_exp      = ifelse(fsum > 0, Fu / fsum, NA),
    land_assets    = ifelse(A > 0, land / A, 0),
    assets_rev     = A / ifelse(R > 0, R, E)))
  rules$debt_assets[k$A <= 0 & k$L > 0] <- max(rules$debt_assets, na.rm = TRUE)
  if (growth) {
    rules$rev_growth   <- ifelse(is.na(k$R_py), NA, sym_growth(k$R_cy, k$R_py))
    rules$asset_growth <- sym_growth(k$A, k$A_boy)
  }

  frame <- rules
  miss_cur <- with(k, cl == 0)
  preds <- as.data.frame(lapply(
    rules[, c("liquid_months", "surplus_margin", "debt_assets", "months_cash")], fill_median))
  frame$current <- cond_impute(rules$current, preds, miss_cur)
  base_preds <- c("liquid_months", "surplus_margin", "months_cash", "self_suff")
  for (v in names(frame)) if (anyNA(frame[[v]])) {
    frame[[v]] <- cond_impute(frame[[v]], frame[, setdiff(base_preds, v), drop = FALSE],
                              is.na(frame[[v]]))
  }
  stopifnot(!anyNA(frame))
  list(components = k, rules = rules, frame = frame,
       imputed_share = colMeans(is.na(rules)),
       ein = if ("EIN2" %in% names(d)) d$EIN2[keep] else NULL)
}
