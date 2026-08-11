#' Filter a panel by composition type and spell balance
#'
#' Compatibility wrapper around `panel990::panel_filter()`.
#'
#' @param df A panel data frame.
#' @param panel_types A per-ID classification, classification summary, or data
#'   with appended `panel_*` columns.
#' @param keep Panel types to retain: `"persistent"`, `"entrant"`, `"exit"`,
#'   `"transient"`, or `"empty"`.
#' @param spell_balance Spell values to retain: `"seamless"` and/or
#'   `"segmented"`.
#' @param id Name of the ID variable.
#' @return Filtered input rows.
#' @export
panel_filter_types <- function(
    df,
    panel_types,
    keep = .PANEL_TYPES,
    spell_balance = .PANEL_SPELL_BALANCE,
    id = "EIN2"
) {
  panel990::panel_filter(
    df,
    panel_type = keep,
    spell = spell_balance,
    classification = panel_types,
    id = id
  )
}
