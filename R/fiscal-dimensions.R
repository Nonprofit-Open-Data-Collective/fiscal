#' Discover candidate fiscal-health dimensions
#'
#' Uses PCA or exploratory factor analysis to propose empirical dimensions from
#' indicators retained by [fiscal_triage()]. This function discovers and
#' describes candidate groupings; it does not name dimensions, orient scores,
#' or construct a composite fiscal-health index.
#'
#' @param data A data frame containing the candidate indicators.
#' @param triage An optional `fiscal_triage` object. When supplied, only its
#'   `selected_for_dimensions` indicators enter the model and its standalone
#'   indicators are carried into the result.
#' @param variables Optional character vector of indicators. Supply either
#'   `triage` or `variables`, not both.
#' @param method Dimension-discovery method: principal components analysis
#'   (`"pca"`) or exploratory factor analysis (`"efa"`).
#' @param nfactors Number of dimensions. If `NULL`, parallel analysis is used.
#' @param rotate Rotation passed to [psych::principal()] or [psych::fa()].
#' @param fm Factoring method used when `method = "efa"`.
#' @param missing Missing-data rule. `"complete"` uses complete rows;
#'   `"median"` replaces missing values with variable medians for this model.
#' @param primary_loading Minimum absolute loading for assignment to a
#'   candidate dimension.
#' @param cross_loading Minimum absolute secondary loading used to flag a
#'   cross-loading indicator.
#' @param min_indicators Minimum assigned indicators for a proposed cluster to
#'   be treated as a multi-indicator dimension rather than a singleton.
#' @param parallel_iter Number of simulated datasets used by parallel analysis
#'   when `nfactors` is `NULL`.
#' @param seed Optional seed for reproducible parallel analysis.
#'
#' @return An object of class `fiscal_dimensions` containing the fitted model,
#'   loading and assignment tables, proposed clusters, singleton and unassigned
#'   indicators, triage standalones, and the complete modeling specification.
#' @export
fiscal_dimensions <- function(data,
                              triage = NULL,
                              variables = NULL,
                              method = c("pca", "efa"),
                              nfactors = NULL,
                              rotate = "oblimin",
                              fm = "minres",
                              missing = c("complete", "median"),
                              primary_loading = 0.40,
                              cross_loading = 0.30,
                              min_indicators = 2L,
                              parallel_iter = 100L,
                              seed = NULL) {
  method <- match.arg(method)
  missing <- match.arg(missing)
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` is required by fiscal_dimensions().", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.null(triage) && !inherits(triage, "fiscal_triage")) {
    stop("`triage` must be returned by fiscal_triage().", call. = FALSE)
  }
  if (!is.null(triage) && !is.null(variables)) {
    stop("Supply either `triage` or `variables`, not both.", call. = FALSE)
  }
  if (!is.null(triage)) variables <- triage$selected_for_dimensions
  if (is.null(variables)) {
    variables <- names(data)[vapply(data, is.numeric, logical(1))]
  }
  if (!is.character(variables) || length(variables) < 2L) {
    stop("At least two indicators are required for dimension discovery.", call. = FALSE)
  }
  unknown <- setdiff(variables, names(data))
  if (length(unknown)) stop("Unknown variables: ", paste(unknown, collapse = ", "),
                            call. = FALSE)
  nonnumeric <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(nonnumeric)) stop("All indicators must be numeric: ",
                               paste(nonnumeric, collapse = ", "), call. = FALSE)
  if (!is.numeric(primary_loading) || length(primary_loading) != 1L ||
      !is.finite(primary_loading) || primary_loading <= 0 || primary_loading > 1) {
    stop("`primary_loading` must be a number in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(cross_loading) || length(cross_loading) != 1L ||
      !is.finite(cross_loading) || cross_loading <= 0 || cross_loading > 1) {
    stop("`cross_loading` must be a number in (0, 1].", call. = FALSE)
  }
  min_indicators <- as.integer(min_indicators)
  parallel_iter <- as.integer(parallel_iter)
  if (is.na(min_indicators) || min_indicators < 2L) {
    stop("`min_indicators` must be at least 2.", call. = FALSE)
  }
  if (is.na(parallel_iter) || parallel_iter < 20L) {
    stop("`parallel_iter` must be at least 20.", call. = FALSE)
  }

  x <- as.data.frame(data[variables])
  rows_used <- rep(TRUE, nrow(x))
  medians <- setNames(rep(NA_real_, length(variables)), variables)
  if (missing == "complete") {
    rows_used <- stats::complete.cases(x)
    x_model <- x[rows_used, , drop = FALSE]
  } else {
    x_model <- x
    for (v in variables) {
      medians[[v]] <- stats::median(x_model[[v]], na.rm = TRUE)
      if (!is.finite(medians[[v]])) {
        stop("Cannot calculate a finite median for `", v, "`.", call. = FALSE)
      }
      x_model[[v]][is.na(x_model[[v]])] <- medians[[v]]
    }
  }
  if (nrow(x_model) < 3L) stop("Fewer than three usable rows remain.", call. = FALSE)
  sds <- vapply(x_model, stats::sd, numeric(1), na.rm = TRUE)
  unusable <- names(sds)[!is.finite(sds) | sds == 0]
  if (length(unusable)) stop("Indicators with zero or undefined variance: ",
                             paste(unusable, collapse = ", "), call. = FALSE)
  cor_mat <- stats::cor(x_model)

  factor_selection <- "specified"
  parallel <- NULL
  if (is.null(nfactors)) {
    factor_selection <- "parallel"
    if (!is.null(seed)) {
      old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit({
        if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
          rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(seed)
    }
    parallel <- suppressWarnings(psych::fa.parallel(
      cor_mat, n.obs = nrow(x_model), fa = if (method == "pca") "pc" else "fa",
      n.iter = parallel_iter, plot = FALSE, error.bars = FALSE
    ))
    nfactors <- if (method == "pca") parallel$ncomp else parallel$nfact
    if (is.null(nfactors) || !is.finite(nfactors) || nfactors < 1L) nfactors <- 1L
  }
  nfactors <- as.integer(nfactors)
  if (length(nfactors) != 1L || is.na(nfactors) || nfactors < 1L ||
      nfactors > length(variables)) {
    stop("`nfactors` must be between 1 and the number of indicators.", call. = FALSE)
  }

  model <- if (method == "pca") {
    suppressWarnings(psych::principal(cor_mat, nfactors = nfactors,
                                      rotate = rotate, scores = FALSE,
                                      n.obs = nrow(x_model)))
  } else {
    suppressWarnings(psych::fa(cor_mat, nfactors = nfactors, rotate = rotate,
                               fm = fm, scores = "none", n.obs = nrow(x_model)))
  }
  loading_matrix <- unclass(model$loadings)
  if (is.null(dim(loading_matrix))) {
    loading_matrix <- matrix(loading_matrix, ncol = 1L,
                             dimnames = list(variables, "Dimension1"))
  }
  colnames(loading_matrix) <- paste0("Dimension", seq_len(ncol(loading_matrix)))
  abs_loadings <- abs(loading_matrix)
  primary_index <- max.col(abs_loadings, ties.method = "first")
  primary_abs <- abs_loadings[cbind(seq_len(nrow(abs_loadings)), primary_index)]
  primary_signed <- loading_matrix[cbind(seq_len(nrow(loading_matrix)), primary_index)]
  secondary_abs <- if (ncol(abs_loadings) > 1L) {
    apply(abs_loadings, 1L, function(z) sort(z, decreasing = TRUE)[2L])
  } else rep(0, nrow(abs_loadings))
  primary_dimension <- colnames(loading_matrix)[primary_index]
  assigned <- primary_abs >= primary_loading
  is_cross_loading <- assigned & secondary_abs >= cross_loading

  assignments <- data.frame(
    variable = rownames(loading_matrix),
    primary_dimension = ifelse(assigned, primary_dimension, NA_character_),
    primary_loading = unname(primary_signed),
    primary_abs_loading = unname(primary_abs),
    secondary_abs_loading = unname(secondary_abs),
    cross_loading = unname(is_cross_loading),
    status = ifelse(!assigned, "unassigned",
                    ifelse(is_cross_loading, "cross_loading", "assigned")),
    stringsAsFactors = FALSE
  )
  loading_table <- cbind(
    data.frame(variable = rownames(loading_matrix), stringsAsFactors = FALSE),
    as.data.frame(loading_matrix, stringsAsFactors = FALSE)
  )

  assigned_groups <- split(assignments$variable[assigned], primary_dimension[assigned])
  all_dimensions <- paste0("Dimension", seq_len(nfactors))
  clusters <- lapply(all_dimensions, function(dim_name) {
    members <- assigned_groups[[dim_name]]
    if (is.null(members)) members <- character()
    data.frame(
      dimension = dim_name,
      n_indicators = length(members),
      indicators = paste(members, collapse = "; "),
      candidate_type = if (length(members) >= min_indicators) "multi_indicator"
                       else if (length(members) == 1L) "singleton" else "empty",
      stringsAsFactors = FALSE
    )
  })
  cluster_table <- do.call(rbind, clusters)

  out <- list(
    method = method,
    nfactors = nfactors,
    factor_selection = factor_selection,
    model = model,
    parallel = parallel,
    correlation = cor_mat,
    loadings = loading_table,
    assignments = assignments,
    clusters = cluster_table,
    multi_indicator_dimensions = cluster_table$dimension[
      cluster_table$candidate_type == "multi_indicator"],
    singleton_indicators = assignments$variable[
      assignments$primary_dimension %in% cluster_table$dimension[
        cluster_table$candidate_type == "singleton"]],
    cross_loading_indicators = assignments$variable[assignments$cross_loading],
    unassigned_indicators = assignments$variable[assignments$status == "unassigned"],
    triage_standalone = if (is.null(triage)) character() else triage$retained_standalone,
    variables = variables,
    rows_used = which(rows_used),
    missing = list(method = missing, medians = medians),
    thresholds = list(primary_loading = primary_loading,
                      cross_loading = cross_loading,
                      min_indicators = min_indicators),
    settings = list(rotate = rotate, fm = fm, parallel_iter = parallel_iter,
                    seed = seed),
    call = match.call()
  )
  class(out) <- "fiscal_dimensions"
  out
}

#' @export
print.fiscal_dimensions <- function(x, ...) {
  cat("Fiscal dimension discovery\n")
  cat("  Method:", toupper(x$method), "\n")
  cat("  Dimensions:", x$nfactors, "(", x$factor_selection, ")\n")
  cat("  Multi-indicator dimensions:", length(x$multi_indicator_dimensions), "\n")
  cat("  Cross-loading indicators:", length(x$cross_loading_indicators), "\n")
  cat("  Unassigned indicators:", length(x$unassigned_indicators), "\n")
  invisible(x)
}

