#' Fit a reusable fiscal-health measurement model
#'
#' Converts reviewed candidate dimensions into a reusable scoring
#' specification. The fitted object stores indicator orientation, centers,
#' scales, missing-value treatment, dimension coefficients, and dimension-score
#' standardization parameters. It can therefore be applied unchanged to later
#' panel years by a scoring function.
#'
#' @param data Reference data used to estimate the measurement model.
#' @param dimensions A `fiscal_dimensions` object or a named list whose elements
#'   are character vectors of indicators.
#' @param dimension_names Optional named character vector mapping discovered
#'   dimension names to substantive names.
#' @param scoring Dimension scoring method. Supply one value for every dimension
#'   or a named vector. Supported methods are `"standardized_mean"`, `"pca"`,
#'   and `"factor"`.
#' @param standalone Optional character vector of standalone indicators. The
#'   default uses standalones carried by a `fiscal_dimensions` object.
#' @param indicator_direction Optional named vector overriding indicator
#'   directions with `"higher"`, `"lower"`, or `"context"`. Other directions
#'   are inferred from [fiscal_metrics()] when possible.
#' @param missing Training-data missing-value rule: `"median"` or
#'   `"complete"`. Median replacement here is statistical missing-value
#'   treatment, not e-file data normalization.
#' @param min_indicators Minimum number of indicators required in a modeled
#'   dimension.
#' @param fm Factoring method passed to [psych::fa()] for factor scores.
#' @param strict_direction If `TRUE`, error when an indicator is context
#'   dependent or absent from the metric registry without an explicit override.
#'
#' @return An object of class `fiscal_health_fit` containing dimension and
#'   standalone scoring specifications, metadata, warnings, and the source
#'   discovery object.
#' @export
fiscal_health_fit <- function(data,
                              dimensions,
                              dimension_names = NULL,
                              scoring = "standardized_mean",
                              standalone = NULL,
                              indicator_direction = NULL,
                              missing = c("median", "complete"),
                              min_indicators = 2L,
                              fm = "minres",
                              strict_direction = FALSE) {
  missing <- match.arg(missing)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (missing(dimensions)) stop("`dimensions` is required.", call. = FALSE)

  discovery <- NULL
  if (inherits(dimensions, "fiscal_dimensions")) {
    discovery <- dimensions
    valid_dims <- dimensions$clusters$dimension[
      dimensions$clusters$candidate_type == "multi_indicator"]
    groups <- lapply(valid_dims, function(z) {
      dimensions$assignments$variable[
        dimensions$assignments$primary_dimension == z &
          dimensions$assignments$status != "unassigned"]
    })
    names(groups) <- valid_dims
    if (is.null(standalone)) standalone <- dimensions$triage_standalone
  } else if (is.list(dimensions) && !is.null(names(dimensions)) &&
             all(nzchar(names(dimensions)))) {
    groups <- dimensions
  } else {
    stop("`dimensions` must be a fiscal_dimensions object or a named list.",
         call. = FALSE)
  }
  if (!length(groups)) stop("No multi-indicator dimensions were supplied.", call. = FALSE)
  if (anyDuplicated(names(groups))) stop("Dimension names must be unique.", call. = FALSE)

  if (!is.null(dimension_names)) {
    if (is.null(names(dimension_names)) || any(!names(dimension_names) %in% names(groups))) {
      stop("`dimension_names` must be named by existing dimensions.", call. = FALSE)
    }
    new_names <- names(groups)
    new_names[match(names(dimension_names), names(groups))] <- unname(dimension_names)
    if (any(!nzchar(new_names)) || anyDuplicated(new_names)) {
      stop("Renamed dimensions must be nonempty and unique.", call. = FALSE)
    }
    names(groups) <- new_names
  }
  min_indicators <- as.integer(min_indicators)
  if (is.na(min_indicators) || min_indicators < 2L) {
    stop("`min_indicators` must be at least 2.", call. = FALSE)
  }
  group_sizes <- lengths(groups)
  if (any(group_sizes < min_indicators)) {
    stop("Dimensions below `min_indicators`: ",
         paste(names(groups)[group_sizes < min_indicators], collapse = ", "),
         call. = FALSE)
  }
  all_indicators <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(all_indicators)) {
    dup <- unique(all_indicators[duplicated(all_indicators)])
    stop("Indicators cannot appear in multiple dimensions: ",
         paste(dup, collapse = ", "), call. = FALSE)
  }
  if (is.null(standalone)) standalone <- character()
  standalone <- unique(standalone)
  if (length(intersect(all_indicators, standalone))) {
    stop("An indicator cannot be both modeled and standalone.", call. = FALSE)
  }
  required <- unique(c(all_indicators, standalone))
  unknown <- setdiff(required, names(data))
  if (length(unknown)) stop("Unknown indicators: ", paste(unknown, collapse = ", "),
                            call. = FALSE)
  nonnumeric <- required[!vapply(data[required], is.numeric, logical(1))]
  if (length(nonnumeric)) stop("All indicators must be numeric: ",
                               paste(nonnumeric, collapse = ", "), call. = FALSE)

  allowed_scoring <- c("standardized_mean", "pca", "factor")
  if (length(scoring) == 1L) {
    scoring <- setNames(rep(scoring, length(groups)), names(groups))
  } else {
    if (is.null(names(scoring)) || !all(names(groups) %in% names(scoring))) {
      stop("Multiple `scoring` values must be named for every dimension.", call. = FALSE)
    }
    scoring <- scoring[names(groups)]
  }
  if (any(!scoring %in% allowed_scoring)) {
    stop("Unknown scoring method; use: ", paste(allowed_scoring, collapse = ", "),
         call. = FALSE)
  }
  if (any(scoring == "factor") && !requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` is required for factor scoring.", call. = FALSE)
  }

  registry <- fiscal_metrics()
  registry_fields <- c("metric", "raw", "winsorized", "standardized", "percentile")
  direction_lookup <- character()
  for (field in registry_fields) {
    vals <- registry[[field]]
    direction_lookup[vals] <- registry$direction
  }
  if (!is.null(indicator_direction)) {
    if (is.null(names(indicator_direction)) || any(!nzchar(names(indicator_direction)))) {
      stop("`indicator_direction` must be a named character vector.", call. = FALSE)
    }
    if (any(!indicator_direction %in% c("higher", "lower", "context"))) {
      stop("Indicator directions must be higher, lower, or context.", call. = FALSE)
    }
    bad_names <- setdiff(names(indicator_direction), required)
    if (length(bad_names)) stop("Direction supplied for unused indicators: ",
                                paste(bad_names, collapse = ", "), call. = FALSE)
  }
  directions <- unname(direction_lookup[required])
  names(directions) <- required
  directions[is.na(directions) | !nzchar(directions)] <- "unknown"
  if (!is.null(indicator_direction)) {
    directions[names(indicator_direction)] <- unname(indicator_direction)
  }
  unresolved <- names(directions)[directions %in% c("context", "unknown")]
  fit_warnings <- character()
  if (length(unresolved)) {
    msg <- paste0("Direction is context-dependent or unknown for: ",
                  paste(unresolved, collapse = ", "),
                  ". They are left unoriented; supply `indicator_direction` to override.")
    if (isTRUE(strict_direction)) stop(msg, call. = FALSE)
    fit_warnings <- c(fit_warnings, msg)
  }
  multipliers <- ifelse(directions == "lower", -1, 1)
  names(multipliers) <- required

  fit_indicator_transform <- function(vars) {
    x <- as.data.frame(data[vars])
    med <- vapply(x, stats::median, numeric(1), na.rm = TRUE)
    if (any(!is.finite(med))) {
      stop("Cannot calculate finite medians for: ",
           paste(names(med)[!is.finite(med)], collapse = ", "), call. = FALSE)
    }
    if (missing == "median") {
      for (v in vars) x[[v]][is.na(x[[v]])] <- med[[v]]
      rows <- rep(TRUE, nrow(x))
    } else {
      rows <- stats::complete.cases(x)
      x <- x[rows, , drop = FALSE]
    }
    if (nrow(x) < 3L) stop("Fewer than three complete rows for indicators: ",
                           paste(vars, collapse = ", "), call. = FALSE)
    center <- vapply(x, mean, numeric(1))
    scale <- vapply(x, stats::sd, numeric(1))
    if (any(!is.finite(scale) | scale == 0)) {
      stop("Zero or undefined variance for: ",
           paste(names(scale)[!is.finite(scale) | scale == 0], collapse = ", "),
           call. = FALSE)
    }
    z <- sweep(as.matrix(x), 2L, center, "-")
    z <- sweep(z, 2L, scale, "/")
    z <- sweep(z, 2L, multipliers[vars], "*")
    list(z = z, median = med, center = center, scale = scale,
         multiplier = multipliers[vars], direction = directions[vars],
         rows = which(rows))
  }

  specs <- lapply(names(groups), function(dim_name) {
    vars <- groups[[dim_name]]
    trans <- fit_indicator_transform(vars)
    cor_mat <- stats::cor(trans$z)
    method <- unname(scoring[[dim_name]])
    fitted_model <- NULL
    if (method == "standardized_mean") {
      coefficients <- setNames(rep(1 / length(vars), length(vars)), vars)
    } else if (method == "pca") {
      eig <- eigen(cor_mat, symmetric = TRUE)
      coefficients <- eig$vectors[, 1L]
      if (sum(coefficients) < 0) coefficients <- -coefficients
      coefficients <- setNames(coefficients / sqrt(sum(coefficients^2)), vars)
      fitted_model <- eig
    } else {
      fitted_model <- suppressWarnings(psych::fa(cor_mat, nfactors = 1L,
                                                 rotate = "none", fm = fm,
                                                 scores = "regression",
                                                 n.obs = nrow(trans$z)))
      load <- as.numeric(fitted_model$loadings[, 1L])
      coefficients <- tryCatch(as.numeric(solve(cor_mat, load)),
                               error = function(e) load)
      if (sum(coefficients) < 0) coefficients <- -coefficients
      coefficients <- setNames(coefficients / sqrt(sum(coefficients^2)), vars)
    }
    raw_score <- as.numeric(trans$z %*% coefficients)
    score_center <- mean(raw_score)
    score_scale <- stats::sd(raw_score)
    if (!is.finite(score_scale) || score_scale == 0) {
      stop("Dimension `", dim_name, "` produced a constant score.", call. = FALSE)
    }
    list(name = dim_name, indicators = vars, method = method,
         coefficients = coefficients, indicator_center = trans$center,
         indicator_scale = trans$scale, indicator_median = trans$median,
         indicator_direction = trans$direction,
         indicator_multiplier = trans$multiplier,
         score_center = score_center, score_scale = score_scale,
         rows_used = trans$rows, correlation = cor_mat, model = fitted_model)
  })
  names(specs) <- names(groups)

  standalone_specs <- lapply(standalone, function(v) {
    trans <- fit_indicator_transform(v)
    list(name = v, indicator = v, center = trans$center[[v]],
         scale = trans$scale[[v]], median = trans$median[[v]],
         direction = trans$direction[[v]], multiplier = trans$multiplier[[v]],
         rows_used = trans$rows)
  })
  names(standalone_specs) <- standalone

  metadata <- do.call(rbind, lapply(specs, function(s) data.frame(
    measure = s$name, measure_type = "dimension", method = s$method,
    source_variables = paste(s$indicators, collapse = "; "),
    n_indicators = length(s$indicators), stringsAsFactors = FALSE)))
  if (length(standalone_specs)) {
    metadata <- rbind(metadata, do.call(rbind, lapply(standalone_specs, function(s)
      data.frame(measure = s$name, measure_type = "standalone",
                 method = "standardized_indicator", source_variables = s$indicator,
                 n_indicators = 1L, stringsAsFactors = FALSE))))
  }

  out <- list(
    dimensions = specs,
    standalone = standalone_specs,
    metadata = metadata,
    missing = missing,
    directions = directions,
    warnings = fit_warnings,
    discovery = discovery,
    training_n = nrow(data),
    settings = list(min_indicators = min_indicators, fm = fm,
                    strict_direction = strict_direction),
    call = match.call()
  )
  class(out) <- "fiscal_health_fit"
  out
}

#' @export
print.fiscal_health_fit <- function(x, ...) {
  cat("Fitted fiscal-health measurement model\n")
  cat("  Dimensions:", length(x$dimensions), "\n")
  cat("  Standalone indicators:", length(x$standalone), "\n")
  cat("  Reference observations:", x$training_n, "\n")
  cat("  Missing-value rule:", x$missing, "\n")
  if (length(x$warnings)) cat("  Direction warnings:", length(x$warnings), "\n")
  invisible(x)
}
