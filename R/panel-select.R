#' Filter a panel by composition type
#'
#' Filters a panel data frame using the ID-level classifications produced by
#' `panel_composition()`.
#'
#' Accepted group labels may be singular or plural:
#'
#' - `"balanced"`
#' - `"entrant"` / `"entrants"`
#' - `"exit"` / `"exits"`
#' - `"interloper"` / `"interlopers"`
#'
#' @param df A panel data frame.
#' @param panel_types Output from `panel_composition()`, or a classification
#' data frame with columns for `id` and `group`.
#' @param keep Character vector of groups to retain.
#' Default is `c( "balanced", "entrants", "exits", "interlopers" )`.
#' @param id Name of the ID variable. Default is `"EIN2"`.
#'
#' @return A filtered data frame containing only rows for IDs in the selected
#' panel composition groups.
#'
#' @export
panel_filter_types <- function(
    df,
    panel_types,
    keep = c( "balanced", "entrants", "exits", "interlopers" ),
    id   = "EIN2"
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( !id %in% names( df ) ) {
    stop( paste0( "`id` column not found in `df`: ", id ) )
  }

  class_df <- .get_panel_classification(
    x  = panel_types,
    id = id
  )

  keep <- .normalize_panel_groups( keep )

  keep_ids <- class_df[ class_df$group %in% keep, id ]

  df[ df[[ id ]] %in% keep_ids, , drop = FALSE ]
}