#' Triage candidate fiscal-health indicators
#'
#' Converts [fiscal_diagnostics()] results into an auditable recommendation.
#' Standalone indicators are excluded from dimension estimation but retained in
#' the final measurement system. Recommendations can be overridden explicitly.
#'
#' @param diagnostics A `fiscal_diagnostics` object.
#' @param keep_from_redundant Rule for choosing a provisional representative
#'   from a connected redundancy group.
#' @param overrides Optional named character vector assigning indicators to
#'   `"retain"`, `"standalone"`, `"noise"`, `"redundant"`, or `"review"`.
#'
#' @return An object of class `fiscal_triage` containing the decision table,
#'   redundancy groups, dimension candidates, standalone indicators, excluded
#'   indicators, and thresholds inherited from the diagnostics.
#' @export
fiscal_triage <- function(diagnostics,
                          keep_from_redundant = c("higher_kmo", "higher_mean_abs_cor", "first"),
                          overrides = NULL) {
  if (!inherits(diagnostics, "fiscal_diagnostics")) {
    stop("`diagnostics` must be returned by fiscal_diagnostics().", call. = FALSE)
  }
  keep_from_redundant <- match.arg(keep_from_redundant)
  d <- diagnostics$variables

  adjacency <- setNames(vector("list", nrow(d)), d$variable)
  pairs <- diagnostics$redundancy_pairs
  if (nrow(pairs)) {
    for (i in seq_len(nrow(pairs))) {
      a <- pairs$var1[[i]]; b <- pairs$var2[[i]]
      adjacency[[a]] <- unique(c(adjacency[[a]], b))
      adjacency[[b]] <- unique(c(adjacency[[b]], a))
    }
  }
  visited <- setNames(rep(FALSE, length(adjacency)), names(adjacency))
  groups <- list()
  for (v in names(adjacency)) {
    if (visited[[v]] || !length(adjacency[[v]])) next
    queue <- v; component <- character()
    while (length(queue)) {
      current <- queue[[1]]; queue <- queue[-1]
      if (visited[[current]]) next
      visited[[current]] <- TRUE
      component <- c(component, current)
      queue <- unique(c(queue, adjacency[[current]][!visited[adjacency[[current]]]]))
    }
    groups[[length(groups) + 1L]] <- sort(unique(component))
  }

  rank_value <- function(z, fallback = -Inf) ifelse(is.na(z), fallback, z)
  group_rows <- lapply(seq_along(groups), function(i) {
    vars <- groups[[i]]
    sub <- d[match(vars, d$variable), , drop = FALSE]
    ord <- switch(keep_from_redundant,
      higher_kmo = order(-rank_value(sub$kmo_msa),
                         -rank_value(sub$mean_abs_cor), sub$variable),
      higher_mean_abs_cor = order(-rank_value(sub$mean_abs_cor),
                                  -rank_value(sub$kmo_msa), sub$variable),
      first = order(sub$variable)
    )
    keeper <- sub$variable[ord[1L]]
    data.frame(group_id = i, members = paste(vars, collapse = "; "),
               kept = keeper, dropped = paste(setdiff(vars, keeper), collapse = "; "),
               stringsAsFactors = FALSE)
  })
  redundancy_groups <- if (length(group_rows)) do.call(rbind, group_rows) else
    data.frame(group_id = integer(), members = character(), kept = character(),
               dropped = character(), stringsAsFactors = FALSE)
  redundant_drops <- unique(unlist(lapply(seq_along(groups), function(i) {
    setdiff(groups[[i]], redundancy_groups$kept[redundancy_groups$group_id == i])
  }), use.names = FALSE))

  decision <- rep("retain", nrow(d))
  reason <- rep("Eligible for multivariate dimension modeling.", nrow(d))
  decision[d$flag_zero_variance] <- "noise"
  reason[d$flag_zero_variance] <- "Near-zero or undefined variance."

  noise_rule <- !d$flag_zero_variance & d$flag_low_kmo &
    d$flag_low_communality & d$flag_weak_correlation
  decision[noise_rule] <- "noise"
  reason[noise_rule] <- "Low KMO, low communality, and weak overall connectedness."

  standalone_rule <- decision == "retain" & d$flag_low_communality
  decision[standalone_rule] <- "standalone"
  reason[standalone_rule] <- paste0(
    "Low communality; retain as a possible standalone indicator rather than ",
    "forcing it into a shared dimension.")

  red_idx <- match(redundant_drops, d$variable, nomatch = 0L)
  red_idx <- red_idx[red_idx > 0L & decision[red_idx] != "noise"]
  decision[red_idx] <- "redundant"
  reason[red_idx] <- "Provisional drop from a highly correlated redundancy group."

  overridden <- rep(FALSE, nrow(d))
  if (!is.null(overrides)) {
    if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
      stop("`overrides` must be a named character vector.", call. = FALSE)
    }
    allowed <- c("retain", "standalone", "noise", "redundant", "review")
    if (any(!overrides %in% allowed)) {
      stop("Override decisions must be one of: ", paste(allowed, collapse = ", "),
           call. = FALSE)
    }
    unknown <- setdiff(names(overrides), d$variable)
    if (length(unknown)) stop("Unknown override variables: ",
                              paste(unknown, collapse = ", "), call. = FALSE)
    oi <- match(names(overrides), d$variable)
    decision[oi] <- unname(overrides)
    reason[oi] <- paste0("User override: ", unname(overrides), ".")
    overridden[oi] <- TRUE
  }

  decisions <- cbind(d, decision = decision, reason = reason,
                     overridden = overridden, stringsAsFactors = FALSE)
  out <- list(
    decisions = decisions,
    redundancy_groups = redundancy_groups,
    selected_for_dimensions = decisions$variable[decisions$decision == "retain"],
    retained_standalone = decisions$variable[decisions$decision == "standalone"],
    excluded_redundant = decisions$variable[decisions$decision == "redundant"],
    excluded_noise = decisions$variable[decisions$decision == "noise"],
    review = decisions$variable[decisions$decision == "review"],
    retained_for_final_system = decisions$variable[decisions$decision %in% c("retain", "standalone")],
    thresholds = diagnostics$thresholds,
    rule = keep_from_redundant,
    call = match.call()
  )
  class(out) <- "fiscal_triage"
  out
}

#' @export
print.fiscal_triage <- function(x, ...) {
  cat("Fiscal indicator triage\n")
  cat("  Dimension candidates:", length(x$selected_for_dimensions), "\n")
  cat("  Standalone indicators:", length(x$retained_standalone), "\n")
  cat("  Redundant exclusions:", length(x$excluded_redundant), "\n")
  cat("  Noise exclusions:", length(x$excluded_noise), "\n")
  invisible(x)
}
