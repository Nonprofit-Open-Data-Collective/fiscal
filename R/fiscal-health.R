#' Fit, score, and optionally aggregate fiscal health
#'
#' A convenience wrapper around [fiscal_health_fit()],
#' [fiscal_health_score()], [fiscal_weights()], and
#' [fiscal_health_index()]. It deliberately does not automate diagnostics,
#' triage, or dimension discovery because those stages require review and
#' substantive judgment.
#'
#' @param data A reference or scoring data frame.
#' @param dimensions A reviewed `fiscal_dimensions` object or named list of
#'   indicator groups. Required when `fit` is not supplied.
#' @param fit An existing `fiscal_health_fit` object for scoring new data.
#' @param weights A `fiscal_weights` object, a named numeric vector of custom
#'   weights, or `NULL` for equal weights.
#' @param index Construct the composite fiscal-health index.
#' @param fit_args Named list of additional arguments for
#'   [fiscal_health_fit()]. Used only when fitting a new model.
#' @param score_args Named list of additional arguments for
#'   [fiscal_health_score()].
#' @param weight_args Named list of additional arguments for
#'   [fiscal_weights()] when `weights` is not already a `fiscal_weights` object.
#' @param index_args Named list of additional arguments for
#'   [fiscal_health_index()].
#'
#' @return An object of class `fiscal_health_result` with `data`, `fit`,
#'   `weights`, and `audit` components.
#' @export
fiscal_health <- function(data,
                          dimensions = NULL,
                          fit = NULL,
                          weights = NULL,
                          index = TRUE,
                          fit_args = list(),
                          score_args = list(),
                          weight_args = list(),
                          index_args = list()) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.logical(index) || length(index) != 1L || is.na(index)) {
    stop("`index` must be TRUE or FALSE.", call. = FALSE)
  }
  arg_lists <- list(fit_args = fit_args, score_args = score_args,
                    weight_args = weight_args, index_args = index_args)
  bad_lists <- names(arg_lists)[!vapply(arg_lists, is.list, logical(1))]
  if (length(bad_lists)) stop("Argument bundles must be lists: ",
                              paste(bad_lists, collapse = ", "), call. = FALSE)
  unnamed <- names(arg_lists)[vapply(arg_lists, function(x) {
    length(x) && (is.null(names(x)) || any(!nzchar(names(x))))
  }, logical(1))]
  if (length(unnamed)) stop("Argument bundles must be named: ",
                            paste(unnamed, collapse = ", "), call. = FALSE)

  mode <- if (is.null(fit)) "fit_and_score" else "score_existing_fit"
  if (is.null(fit)) {
    if (is.null(dimensions)) {
      stop("Supply reviewed `dimensions` when `fit` is not provided.", call. = FALSE)
    }
    reserved <- intersect(names(fit_args), c("data", "dimensions"))
    if (length(reserved)) stop("Do not include reserved fit arguments: ",
                               paste(reserved, collapse = ", "), call. = FALSE)
    fit <- do.call(fiscal_health_fit,
                   c(list(data = data, dimensions = dimensions), fit_args))
  } else {
    if (!inherits(fit, "fiscal_health_fit")) {
      stop("`fit` must be returned by fiscal_health_fit().", call. = FALSE)
    }
    if (!is.null(dimensions)) {
      stop("Do not supply `dimensions` with an existing `fit`.", call. = FALSE)
    }
    if (length(fit_args)) {
      stop("`fit_args` cannot be used with an existing `fit`.", call. = FALSE)
    }
  }

  reserved <- intersect(names(score_args), c("data", "fit"))
  if (length(reserved)) stop("Do not include reserved score arguments: ",
                             paste(reserved, collapse = ", "), call. = FALSE)
  scored <- do.call(fiscal_health_score,
                    c(list(data = data, fit = fit), score_args))
  scoring_audit <- attr(scored, "fiscal_health_audit")

  weight_spec <- NULL
  index_audit <- NULL
  result_data <- scored
  if (isTRUE(index)) {
    if (inherits(weights, "fiscal_weights")) {
      if (length(weight_args)) {
        stop("`weight_args` cannot modify an existing fiscal_weights object.",
             call. = FALSE)
      }
      weight_spec <- weights
      fit_measures <- c(names(fit$dimensions), names(fit$standalone))
      unknown_weights <- setdiff(names(weight_spec$weights), fit_measures)
      if (length(unknown_weights)) {
        stop("The weighting specification contains measures absent from `fit`: ",
             paste(unknown_weights, collapse = ", "), call. = FALSE)
      }
    } else {
      if (!is.null(weights) && !is.numeric(weights)) {
        stop("`weights` must be NULL, a named numeric vector, or fiscal_weights object.",
             call. = FALSE)
      }
      reserved <- intersect(names(weight_args), c("fit", "weights"))
      if (length(reserved)) stop("Do not include reserved weight arguments: ",
                                 paste(reserved, collapse = ", "), call. = FALSE)
      if (isFALSE(score_args$include_standalone) &&
          is.null(weight_args$include_standalone)) {
        weight_args$include_standalone <- FALSE
      }
      weight_spec <- do.call(fiscal_weights,
        c(list(fit = fit, weights = weights), weight_args))
    }
    reserved <- intersect(names(index_args), c("data", "weights"))
    if (length(reserved)) stop("Do not include reserved index arguments: ",
                               paste(reserved, collapse = ", "), call. = FALSE)
    if (!is.null(score_args$prefix) && is.null(index_args$score_prefix)) {
      index_args$score_prefix <- score_args$prefix
    }
    result_data <- do.call(fiscal_health_index,
      c(list(data = scored, weights = weight_spec), index_args))
    index_audit <- attr(result_data, "fiscal_index_audit")
  } else if (!is.null(weights) || length(weight_args) || length(index_args)) {
    stop("Weight and index arguments require `index = TRUE`.", call. = FALSE)
  }

  out <- list(
    data = result_data,
    fit = fit,
    weights = weight_spec,
    audit = list(
      mode = mode,
      fit_warnings = fit$warnings,
      scoring = scoring_audit,
      index = index_audit
    ),
    call = match.call()
  )
  class(out) <- "fiscal_health_result"
  out
}

#' @export
print.fiscal_health_result <- function(x, ...) {
  cat("Fiscal-health result\n")
  cat("  Mode:", x$audit$mode, "\n")
  cat("  Observations:", nrow(x$data), "\n")
  cat("  Dimensions:", length(x$fit$dimensions), "\n")
  cat("  Standalone indicators:", length(x$fit$standalone), "\n")
  cat("  Composite index:", if (is.null(x$weights)) "no" else "yes", "\n")
  invisible(x)
}
