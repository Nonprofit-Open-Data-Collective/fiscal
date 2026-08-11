#' Diagnose candidate fiscal-health indicators
#'
#' Computes variable-level diagnostics used to distinguish indicators that
#' participate in a shared multivariate structure from redundant, standalone,
#' or noisy indicators. Correlation summaries exclude each variable's
#' correlation with itself.
#'
#' `psych` is used, when installed, for KMO/MSA and a preliminary common-factor
#' communality screen. The remaining diagnostics are always computed. If
#' `psych` is unavailable or the factor screen fails, the relevant fields are
#' returned as `NA` and the reason is recorded in `warnings`.
#'
#' @param data A data frame containing candidate indicators.
#' @param variables Character vector of columns to diagnose. By default, all
#'   numeric columns are used.
#' @param corr_cutoff Absolute correlation used to flag redundancy.
#' @param kmo_low KMO/MSA threshold used to flag weak shared variance.
#' @param communality_low Threshold used to flag low communality.
#' @param mean_abs_cor_low Threshold used to flag weak connectedness.
#' @param smc_high Squared multiple correlation threshold.
#' @param vif_high Variance inflation factor threshold.
#' @param near_zero_sd Standard-deviation threshold for near-zero variance.
#' @param nfactors_comm Number of factors in the preliminary communality
#'   screen. The default is between one and three, based on the number of
#'   usable indicators.
#' @param rotate Rotation passed to [psych::fa()].
#' @param fm Factoring method passed to [psych::fa()].
#' @param use Missing-value rule passed to [stats::cor()].
#'
#' @return An object of class `fiscal_diagnostics`. Its `variables` component
#'   is a variable-level data frame; other components contain the correlation
#'   matrix, redundancy pairs, thresholds, preliminary model, and warnings.
#' @export
fiscal_diagnostics <- function(data,
                               variables = NULL,
                               corr_cutoff = 0.90,
                               kmo_low = 0.50,
                               communality_low = 0.20,
                               mean_abs_cor_low = 0.10,
                               smc_high = 0.90,
                               vif_high = 10,
                               near_zero_sd = 1e-6,
                               nfactors_comm = NULL,
                               rotate = "oblimin",
                               fm = "minres",
                               use = "pairwise.complete.obs") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  if (is.null(variables)) {
    variables <- names(data)[vapply(data, is.numeric, logical(1))]
  }
  if (!is.character(variables) || !length(variables)) {
    stop("`variables` must identify at least two numeric columns.", call. = FALSE)
  }
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars)) {
    stop("Unknown variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  nonnumeric <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(nonnumeric)) {
    stop("All selected variables must be numeric: ",
         paste(nonnumeric, collapse = ", "), call. = FALSE)
  }
  if (length(variables) < 2L) {
    stop("At least two numeric variables are required.", call. = FALSE)
  }

  thresholds <- list(
    corr_cutoff = corr_cutoff, kmo_low = kmo_low,
    communality_low = communality_low,
    mean_abs_cor_low = mean_abs_cor_low, smc_high = smc_high,
    vif_high = vif_high, near_zero_sd = near_zero_sd
  )
  if (any(!is.finite(unlist(thresholds)))) {
    stop("All diagnostic thresholds must be finite.", call. = FALSE)
  }

  x <- as.data.frame(data[variables])
  n_observed <- vapply(x, function(z) sum(!is.na(z)), integer(1))
  n_missing <- nrow(x) - n_observed
  sds <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
  zero_var <- is.na(sds) | sds <= near_zero_sd
  usable <- names(sds)[!zero_var]
  if (length(usable) < 2L) {
    stop("Fewer than two variables have usable variance.", call. = FALSE)
  }

  cor_mat <- stats::cor(x[usable], use = use)
  if (anyNA(cor_mat)) {
    stop("The correlation matrix contains missing values; check overlapping observations.",
         call. = FALSE)
  }
  offdiag <- abs(cor_mat)
  diag(offdiag) <- NA_real_
  mean_abs_cor <- rowMeans(offdiag, na.rm = TRUE)
  max_abs_cor <- apply(offdiag, 1L, max, na.rm = TRUE)

  idx <- which(upper.tri(cor_mat) & abs(cor_mat) >= corr_cutoff, arr.ind = TRUE)
  redundancy_pairs <- data.frame(
    var1 = rownames(cor_mat)[idx[, "row"]],
    var2 = colnames(cor_mat)[idx[, "col"]],
    r = if (nrow(idx)) cor_mat[idx] else numeric(),
    stringsAsFactors = FALSE
  )

  inv_cor <- tryCatch(solve(cor_mat), error = function(e) NULL)
  smc <- vif <- anti_image_diag <- rep(NA_real_, length(usable))
  names(smc) <- names(vif) <- names(anti_image_diag) <- usable
  diagnostic_warnings <- character()
  if (is.null(inv_cor)) {
    diagnostic_warnings <- c(diagnostic_warnings,
      "Correlation matrix is singular; SMC, VIF, and anti-image diagnostics are unavailable.")
  } else {
    vif[] <- diag(inv_cor)
    smc[] <- 1 - (1 / diag(inv_cor))
    anti_image_diag[] <- 1 / diag(inv_cor)
  }

  det_full <- determinant(cor_mat, logarithm = TRUE)
  full_log_det <- if (det_full$sign == 0) -Inf else as.numeric(det_full$modulus)
  delta_log_det <- vapply(seq_along(usable), function(i) {
    sub <- cor_mat[-i, -i, drop = FALSE]
    d <- determinant(sub, logarithm = TRUE)
    sub_log <- if (d$sign == 0) -Inf else as.numeric(d$modulus)
    sub_log - full_log_det
  }, numeric(1))
  names(delta_log_det) <- usable

  kmo_msa <- communality <- rep(NA_real_, length(usable))
  names(kmo_msa) <- names(communality) <- usable
  kmo_object <- fa_screen <- NULL
  if (!requireNamespace("psych", quietly = TRUE)) {
    diagnostic_warnings <- c(diagnostic_warnings,
      "Package `psych` is not installed; KMO and communality diagnostics are unavailable.")
  } else {
    kmo_object <- tryCatch(psych::KMO(cor_mat), error = function(e) e)
    if (inherits(kmo_object, "error")) {
      diagnostic_warnings <- c(diagnostic_warnings,
        paste0("KMO failed: ", conditionMessage(kmo_object)))
      kmo_object <- NULL
    } else {
      kmo_msa[names(kmo_object$MSAi)] <- kmo_object$MSAi
    }
    if (is.null(nfactors_comm)) {
      nfactors_comm <- max(1L, min(3L, floor(length(usable) / 3L)))
    }
    fa_screen <- tryCatch(
      suppressWarnings(psych::fa(x[usable], nfactors = nfactors_comm,
                                 fm = fm, rotate = rotate, scores = "none")),
      error = function(e) e
    )
    if (inherits(fa_screen, "error")) {
      diagnostic_warnings <- c(diagnostic_warnings,
        paste0("Preliminary factor screen failed: ", conditionMessage(fa_screen)))
      fa_screen <- NULL
    } else {
      communality[names(fa_screen$communality)] <- fa_screen$communality
    }
  }

  redundant_with <- setNames(rep("", length(usable)), usable)
  if (nrow(redundancy_pairs)) {
    for (v in usable) {
      hits <- redundancy_pairs$var2[redundancy_pairs$var1 == v]
      hits <- c(hits, redundancy_pairs$var1[redundancy_pairs$var2 == v])
      redundant_with[[v]] <- paste(sort(unique(hits)), collapse = "; ")
    }
  }

  tbl <- data.frame(
    variable = variables,
    n_observed = unname(n_observed[variables]),
    n_missing = unname(n_missing[variables]),
    missing_rate = unname(n_missing[variables] / nrow(x)),
    sd = unname(sds[variables]),
    kmo_msa = NA_real_, communality = NA_real_, smc = NA_real_, vif = NA_real_,
    mean_abs_cor = NA_real_, max_abs_cor = NA_real_, anti_image_diag = NA_real_,
    delta_log_det = NA_real_, n_redundant_links = 0L, redundant_with = "",
    stringsAsFactors = FALSE
  )
  m <- match(usable, tbl$variable)
  tbl$kmo_msa[m] <- kmo_msa[usable]
  tbl$communality[m] <- communality[usable]
  tbl$smc[m] <- smc[usable]
  tbl$vif[m] <- vif[usable]
  tbl$mean_abs_cor[m] <- mean_abs_cor[usable]
  tbl$max_abs_cor[m] <- max_abs_cor[usable]
  tbl$anti_image_diag[m] <- anti_image_diag[usable]
  tbl$delta_log_det[m] <- delta_log_det[usable]
  tbl$redundant_with[m] <- redundant_with[usable]
  tbl$n_redundant_links[m] <- ifelse(nzchar(redundant_with[usable]),
    lengths(strsplit(redundant_with[usable], "; ", fixed = TRUE)), 0L)

  tbl$flag_zero_variance <- zero_var[tbl$variable]
  tbl$flag_low_kmo <- !is.na(tbl$kmo_msa) & tbl$kmo_msa < kmo_low
  tbl$flag_low_communality <- !is.na(tbl$communality) & tbl$communality < communality_low
  tbl$flag_high_redundancy <- !is.na(tbl$max_abs_cor) & tbl$max_abs_cor >= corr_cutoff
  tbl$flag_weak_correlation <- !is.na(tbl$mean_abs_cor) & tbl$mean_abs_cor < mean_abs_cor_low
  tbl$flag_high_smc <- !is.na(tbl$smc) & tbl$smc >= smc_high
  tbl$flag_high_vif <- !is.na(tbl$vif) & tbl$vif >= vif_high

  out <- list(
    variables = tbl,
    correlation = cor_mat,
    redundancy_pairs = redundancy_pairs,
    thresholds = thresholds,
    settings = list(nfactors_comm = nfactors_comm, rotate = rotate, fm = fm, use = use),
    kmo = kmo_object,
    factor_screen = fa_screen,
    warnings = unique(diagnostic_warnings),
    call = match.call()
  )
  class(out) <- "fiscal_diagnostics"
  out
}

#' @export
print.fiscal_diagnostics <- function(x, ...) {
  cat("Fiscal indicator diagnostics\n")
  cat("  Indicators:", nrow(x$variables), "\n")
  cat("  Redundant pairs:", nrow(x$redundancy_pairs), "\n")
  cat("  Near-zero variance:", sum(x$variables$flag_zero_variance), "\n")
  if (length(x$warnings)) cat("  Warnings:", length(x$warnings), "\n")
  invisible(x)
}

