#' Apply a fitted fiscal-health measurement model
#'
#' Scores reference or new data using the transformations and coefficients
#' stored by [fiscal_health_fit()]. No model parameters are re-estimated.
#'
#' @param data A data frame containing every indicator required by `fit`.
#' @param fit A `fiscal_health_fit` object.
#' @param missing Missing-value rule. `"fit"` reuses the rule selected during
#'   fitting; `"median"` uses the reference-sample medians stored in `fit`;
#'   `"complete"` returns a missing dimension score if any indicator in that
#'   dimension is missing.
#' @param min_coverage Minimum proportion of a dimension's source indicators
#'   that must be observed before a score is reported. This applies even when
#'   reference medians are used.
#' @param prefix Prefix applied to generated score columns.
#' @param include_standalone Include standardized standalone indicators.
#' @param include_coverage Add an observation-level coverage column for every
#'   generated measure.
#' @param append If `TRUE`, append scores to `data`; otherwise return score and
#'   coverage columns only.
#' @param overwrite Allow generated columns to replace existing columns.
#'
#' @return A data frame of class `fiscal_health_scores`. Its attributes retain
#'   the fitted model, scoring audit, and generated column names.
#' @export
fiscal_health_score <- function(data,
                                fit,
                                missing = c("fit", "median", "complete"),
                                min_coverage = 0,
                                prefix = "fh_",
                                include_standalone = TRUE,
                                include_coverage = TRUE,
                                append = TRUE,
                                overwrite = FALSE) {
  missing <- match.arg(missing)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!inherits(fit, "fiscal_health_fit")) {
    stop("`fit` must be returned by fiscal_health_fit().", call. = FALSE)
  }
  if (!is.numeric(min_coverage) || length(min_coverage) != 1L ||
      !is.finite(min_coverage) || min_coverage < 0 || min_coverage > 1) {
    stop("`min_coverage` must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("`prefix` must be one character string.", call. = FALSE)
  }
  if (missing == "fit") missing <- fit$missing

  dimension_vars <- unique(unlist(lapply(fit$dimensions, `[[`, "indicators"),
                                  use.names = FALSE))
  standalone_vars <- if (isTRUE(include_standalone)) names(fit$standalone) else character()
  required <- unique(c(dimension_vars, standalone_vars))
  unknown <- setdiff(required, names(data))
  if (length(unknown)) {
    stop("Scoring data are missing required indicators: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  nonnumeric <- required[!vapply(data[required], is.numeric, logical(1))]
  if (length(nonnumeric)) {
    stop("Required indicators must be numeric: ", paste(nonnumeric, collapse = ", "),
         call. = FALSE)
  }

  measure_names <- c(names(fit$dimensions), standalone_vars)
  score_names <- paste0(prefix, measure_names)
  coverage_names <- paste0(score_names, "_coverage")
  generated <- c(score_names, if (isTRUE(include_coverage)) coverage_names)
  if (anyDuplicated(generated)) stop("Generated score names are not unique.", call. = FALSE)
  collisions <- intersect(generated, names(data))
  if (length(collisions) && !isTRUE(overwrite)) {
    stop("Generated columns already exist: ", paste(collisions, collapse = ", "),
         ". Set `overwrite = TRUE` to replace them.", call. = FALSE)
  }

  scores <- setNames(vector("list", length(measure_names)), measure_names)
  coverage <- setNames(vector("list", length(measure_names)), measure_names)
  audit_rows <- vector("list", length(measure_names))
  audit_i <- 0L

  for (dim_name in names(fit$dimensions)) {
    spec <- fit$dimensions[[dim_name]]
    vars <- spec$indicators
    x <- as.matrix(data[vars])
    observed <- !is.na(x)
    row_coverage <- rowMeans(observed)
    if (missing == "median") {
      for (j in seq_along(vars)) {
        x[is.na(x[, j]), j] <- spec$indicator_median[[vars[[j]]]]
      }
      eligible <- row_coverage >= min_coverage
    } else {
      eligible <- row_coverage == 1 & row_coverage >= min_coverage
    }
    z <- sweep(x, 2L, spec$indicator_center[vars], "-")
    z <- sweep(z, 2L, spec$indicator_scale[vars], "/")
    z <- sweep(z, 2L, spec$indicator_multiplier[vars], "*")
    raw_score <- as.numeric(z %*% spec$coefficients[vars])
    value <- (raw_score - spec$score_center) / spec$score_scale
    value[!eligible] <- NA_real_
    scores[[dim_name]] <- value
    coverage[[dim_name]] <- row_coverage
    audit_i <- audit_i + 1L
    audit_rows[[audit_i]] <- data.frame(
      measure = dim_name, measure_type = "dimension", method = spec$method,
      n_rows = nrow(data), n_scored = sum(!is.na(value)),
      n_below_coverage = sum(row_coverage < min_coverage),
      mean_coverage = mean(row_coverage), stringsAsFactors = FALSE
    )
  }

  if (isTRUE(include_standalone) && length(fit$standalone)) {
    for (measure in names(fit$standalone)) {
      spec <- fit$standalone[[measure]]
      x <- data[[spec$indicator]]
      observed <- !is.na(x)
      row_coverage <- as.numeric(observed)
      if (missing == "median") {
        x[!observed] <- spec$median
        eligible <- row_coverage >= min_coverage
      } else {
        eligible <- observed & row_coverage >= min_coverage
      }
      value <- ((x - spec$center) / spec$scale) * spec$multiplier
      value[!eligible] <- NA_real_
      scores[[measure]] <- value
      coverage[[measure]] <- row_coverage
      audit_i <- audit_i + 1L
      audit_rows[[audit_i]] <- data.frame(
        measure = measure, measure_type = "standalone",
        method = "standardized_indicator", n_rows = nrow(data),
        n_scored = sum(!is.na(value)),
        n_below_coverage = sum(row_coverage < min_coverage),
        mean_coverage = mean(row_coverage), stringsAsFactors = FALSE
      )
    }
  }

  score_df <- as.data.frame(scores, optional = TRUE, stringsAsFactors = FALSE)
  names(score_df) <- score_names
  if (isTRUE(include_coverage)) {
    coverage_df <- as.data.frame(coverage, optional = TRUE, stringsAsFactors = FALSE)
    names(coverage_df) <- coverage_names
    score_df <- cbind(score_df, coverage_df)
  }
  out <- if (isTRUE(append)) {
    if (length(collisions)) data[collisions] <- NULL
    cbind(data, score_df)
  } else score_df
  class(out) <- unique(c("fiscal_health_scores", class(out)))
  attr(out, "fiscal_health_fit") <- fit
  attr(out, "fiscal_health_audit") <- do.call(rbind, audit_rows[seq_len(audit_i)])
  attr(out, "fiscal_health_columns") <- list(
    scores = score_names,
    coverage = if (isTRUE(include_coverage)) coverage_names else character(),
    prefix = prefix, missing = missing, min_coverage = min_coverage
  )
  out
}

#' @export
print.fiscal_health_scores <- function(x, ...) {
  cols <- attr(x, "fiscal_health_columns")
  cat("Fiscal-health scores\n")
  cat("  Observations:", nrow(x), "\n")
  cat("  Score columns:", length(cols$scores), "\n")
  cat("  Missing-value rule:", cols$missing, "\n")
  cat("  Minimum coverage:", cols$min_coverage, "\n")
  NextMethod("print")
}

