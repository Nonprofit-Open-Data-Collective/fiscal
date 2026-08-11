#' Define weights for a composite fiscal-health index
#'
#' Creates a validated weighting specification for already-scored dimensions
#' and standalone indicators. It does not discover dimensions or fit a
#' measurement model.
#'
#' @param fit Optional `fiscal_health_fit` object identifying the available
#'   measures and their types.
#' @param weights Optional named numeric vector of custom weights. If omitted,
#'   all included measures receive equal weight.
#' @param include_standalone Include standalone indicators from `fit` in an
#'   automatically generated or validated specification.
#' @param normalize Normalize weights to sum to one.
#' @param missing Composite-index missing-score rule: `"reweight"` reallocates
#'   weight across available scores; `"complete"` requires every positively
#'   weighted score.
#' @param min_coverage Minimum weighted coverage required to report the index.
#' @param name Optional descriptive name for the weighting specification.
#'
#' @return An object of class `fiscal_weights`.
#' @export
fiscal_weights <- function(fit = NULL,
                           weights = NULL,
                           include_standalone = TRUE,
                           normalize = TRUE,
                           missing = c("reweight", "complete"),
                           min_coverage = 1,
                           name = NULL) {
  missing <- match.arg(missing)
  if (!is.null(fit) && !inherits(fit, "fiscal_health_fit")) {
    stop("`fit` must be returned by fiscal_health_fit().", call. = FALSE)
  }
  if (!is.numeric(min_coverage) || length(min_coverage) != 1L ||
      !is.finite(min_coverage) || min_coverage < 0 || min_coverage > 1) {
    stop("`min_coverage` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("`normalize` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(name) && (!is.character(name) || length(name) != 1L || is.na(name))) {
    stop("`name` must be NULL or one character string.", call. = FALSE)
  }

  dimensions <- if (is.null(fit)) character() else names(fit$dimensions)
  standalones <- if (is.null(fit) || !isTRUE(include_standalone)) character()
                 else names(fit$standalone)
  available <- c(dimensions, standalones)

  method <- if (is.null(weights)) "equal" else "custom"
  if (is.null(weights)) {
    if (!length(available)) {
      stop("Supply `fit` or a named `weights` vector.", call. = FALSE)
    }
    weights <- setNames(rep(1, length(available)), available)
  } else {
    if (!is.numeric(weights) || !length(weights) || is.null(names(weights)) ||
        any(!nzchar(names(weights))) || anyDuplicated(names(weights))) {
      stop("`weights` must be a nonempty, uniquely named numeric vector.", call. = FALSE)
    }
    if (any(!is.finite(weights)) || any(weights < 0)) {
      stop("Weights must be finite and nonnegative.", call. = FALSE)
    }
    if (!any(weights > 0)) stop("At least one weight must be positive.", call. = FALSE)
    if (length(available)) {
      unknown <- setdiff(names(weights), available)
      omitted <- setdiff(available, names(weights))
      if (length(unknown)) stop("Weights supplied for unknown measures: ",
                                paste(unknown, collapse = ", "), call. = FALSE)
      if (length(omitted)) stop("Weights are required for every included measure: ",
                                paste(omitted, collapse = ", "), call. = FALSE)
      weights <- weights[available]
    }
  }
  original_weights <- weights
  if (isTRUE(normalize)) weights <- weights / sum(weights)

  measure_type <- rep("unspecified", length(weights))
  names(measure_type) <- names(weights)
  measure_type[names(weights) %in% dimensions] <- "dimension"
  measure_type[names(weights) %in% standalones] <- "standalone"
  table <- data.frame(
    measure = names(weights),
    measure_type = unname(measure_type),
    original_weight = as.numeric(original_weights),
    weight = as.numeric(weights),
    stringsAsFactors = FALSE
  )
  out <- list(
    weights = weights,
    table = table,
    method = method,
    normalized = normalize,
    missing = missing,
    min_coverage = min_coverage,
    include_standalone = include_standalone,
    name = name,
    fit = fit,
    call = match.call()
  )
  class(out) <- "fiscal_weights"
  out
}

#' @export
print.fiscal_weights <- function(x, ...) {
  cat("Fiscal-health weights\n")
  if (!is.null(x$name)) cat("  Name:", x$name, "\n")
  cat("  Method:", x$method, "\n")
  cat("  Measures:", length(x$weights), "\n")
  cat("  Missing-score rule:", x$missing, "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' Construct a composite fiscal-health index
#'
#' Aggregates scores produced by [fiscal_health_score()] using a
#' [fiscal_weights()] specification.
#'
#' @param data A data frame, normally returned by [fiscal_health_score()].
#' @param weights A `fiscal_weights` object.
#' @param score_prefix Prefix used on the component score columns.
#' @param index_name Name of the composite index column.
#' @param coverage_name Name of its weighted-coverage column.
#' @param append If `TRUE`, append the index to `data`; otherwise return only
#'   the index and coverage columns.
#' @param overwrite Allow replacement of existing output columns.
#'
#' @return A data frame containing the composite index and weighted coverage.
#' @export
fiscal_health_index <- function(data,
                                weights,
                                score_prefix = "fh_",
                                index_name = "fiscal_health",
                                coverage_name = paste0(index_name, "_coverage"),
                                append = TRUE,
                                overwrite = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!inherits(weights, "fiscal_weights")) {
    stop("`weights` must be returned by fiscal_weights().", call. = FALSE)
  }
  if (!is.character(score_prefix) || length(score_prefix) != 1L || is.na(score_prefix)) {
    stop("`score_prefix` must be one character string.", call. = FALSE)
  }
  output_names <- c(index_name, coverage_name)
  if (any(!nzchar(output_names)) || anyDuplicated(output_names)) {
    stop("Index and coverage names must be nonempty and distinct.", call. = FALSE)
  }
  component_columns <- paste0(score_prefix, names(weights$weights))
  missing_columns <- setdiff(component_columns, names(data))
  if (length(missing_columns)) {
    stop("Component score columns are missing: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  nonnumeric <- component_columns[
    !vapply(data[component_columns], is.numeric, logical(1))]
  if (length(nonnumeric)) stop("Component scores must be numeric: ",
                               paste(nonnumeric, collapse = ", "), call. = FALSE)
  collisions <- intersect(output_names, names(data))
  if (length(collisions) && !isTRUE(overwrite)) {
    stop("Output columns already exist: ", paste(collisions, collapse = ", "),
         ". Set `overwrite = TRUE` to replace them.", call. = FALSE)
  }

  x <- as.matrix(data[component_columns])
  w <- as.numeric(weights$weights)
  positive <- w > 0
  observed <- !is.na(x)
  available_weight <- as.numeric(observed[, positive, drop = FALSE] %*% w[positive])
  total_weight <- sum(w[positive])
  weighted_coverage <- available_weight / total_weight
  x_zero <- x
  x_zero[is.na(x_zero)] <- 0
  numerator <- as.numeric(x_zero %*% w)

  if (weights$missing == "complete") {
    eligible <- rowSums(!observed[, positive, drop = FALSE]) == 0L
    index <- numerator / total_weight
  } else {
    eligible <- available_weight > 0
    index <- numerator / available_weight
  }
  eligible <- eligible & weighted_coverage >= weights$min_coverage
  index[!eligible] <- NA_real_

  index_df <- data.frame(index, weighted_coverage, stringsAsFactors = FALSE)
  names(index_df) <- output_names
  out <- if (isTRUE(append)) {
    if (length(collisions)) data[collisions] <- NULL
    cbind(data, index_df)
  } else index_df
  attr(out, "fiscal_weights") <- weights
  attr(out, "fiscal_index_audit") <- list(
    n_rows = nrow(data), n_scored = sum(!is.na(index)),
    n_below_coverage = sum(weighted_coverage < weights$min_coverage),
    mean_coverage = mean(weighted_coverage), component_columns = component_columns
  )
  out
}

