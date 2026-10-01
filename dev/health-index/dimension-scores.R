# Dimension scores for the fiscal health index.
#
# Members are the retained indicator sets from dimension-discovery-2023.Rmd.
# Each member is winsorized and normalized by the measurement model in
# build-ratio-frame.R, then averaged within a dimension and re-standardized.
# Every score is oriented so that HIGHER IS HEALTHIER, which flips leverage.
# Asset structure is carried for reporting only: it has no healthy direction.

source("build-ratio-frame.R")

MEMBERS <- list(
  reserves        = c("months_cash", "liquid_months", "assets_rev", "current"),
  performance     = c("surplus_margin", "return_assets", "na_growth"),
  leverage        = c("debt_assets", "payables_una"),
  asset_structure = c("cash_assets", "land_assets"))

FLIP <- c(reserves = 1, performance = 1, leverage = -1, asset_structure = 1)

#' @param d One tax year of full-990 filings (an e-file style data frame).
#' @return data.table: EIN2, the dimension scores, and a few raw quantities.
dimension_scores <- function(d, seed = 2023) {
  built <- build_ratio_frame(d, seed = seed)
  z <- as.data.frame(lapply(built$frame[unlist(MEMBERS)], winsor_normalize))
  out <- data.table::data.table(EIN2 = built$ein)
  for (nm in names(MEMBERS)) {
    s <- rowMeans(z[, MEMBERS[[nm]], drop = FALSE])
    out[[nm]] <- as.numeric(scale(FLIP[[nm]] * s))
  }
  k <- built$components
  out[, `:=`(surplus_per_expense = (k$R - k$E) / k$E,
             cash_months = 12 * (k$cash + k$recv) / pmax(k$E, 1),
             E = k$E, A = k$A)]
  out[]
}

#' Weighted composite of the directional dimensions, rescaled to 0-100 by rank.
#' @param scores Output of dimension_scores().
#' @param weights Named vector over dimensions; zero-weight dimensions are ignored.
composite_index <- function(scores, weights) {
  weights <- weights[weights > 0]
  m <- as.matrix(scores[, names(weights), with = FALSE])
  raw <- drop(m %*% (weights / sum(weights)))
  100 * (rank(raw, ties.method = "average") - 0.5) / length(raw)
}
