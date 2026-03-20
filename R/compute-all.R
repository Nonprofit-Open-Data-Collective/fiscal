###---------------------------------------------------
###   COMPUTE ALL FISCAL HEALTH METRICS
###---------------------------------------------------

#' Compute all fiscal health metrics in one call
#'
#' @description
#' Applies every `get_*()` ratio function in the `fiscal` package to
#' a data frame in a single call. The data frame is sanitized once up front
#' using [sanitize_financials()], then each function is called with
#' `sanitize = FALSE` to avoid redundant imputation passes.
#'
#' @param df A `data.frame` containing IRS 990 efile financial fields.
#' @param metrics Character vector of metric variants to return. Any combination
#'   of `"ratio"`, `"w"`, `"z"`, `"p"`. Defaults to all four.
#' @param append_to_df Logical. If `TRUE` (default), returns the original
#'   `df` with metric columns appended. If `FALSE`, returns only
#'   `.IDVARS` identifier columns plus the selected metric columns.
#' @param winsorize Winsorization proportion (default `0.98`).
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return A `data.frame`.
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- compute_all( dat10k )
#' dim( d )
#'
#' @export
compute_all <- function( df,
                         metrics       = c("ratio","w","z","p"),
                         append_to_df  = TRUE,
                         winsorize     = 0.98,
                         verbose       = TRUE ) {

  valid_metrics <- c("ratio","w","z","p")
  bad <- setdiff( metrics, valid_metrics )
  if ( length(bad) > 0 )
    stop( "Invalid metrics value(s): ", paste(bad, collapse=", "),
          ". Choose from: ", paste(valid_metrics, collapse=", ") )

  if (verbose) message( "Sanitizing financial fields ..." )
  df_sanitized <- sanitize_financials( df )

  .try_ratio <- function( fn, label, ... ) {
    if (verbose) message( "  Computing ", label, " ..." )
    tryCatch(
      fn( df_sanitized, ..., winsorize = winsorize, sanitize = FALSE ),
      error = function(e) {
        warning( label, "() skipped: ", conditionMessage(e), call. = FALSE )
        df_sanitized
      }
    )
  }

  # ---- Liquidity ----
  df_sanitized <- .try_ratio( get_current_ratio,          "get_current_ratio" )
  df_sanitized <- .try_ratio( get_quick_ratio,            "get_quick_ratio" )
  df_sanitized <- .try_ratio( get_cash_liquidity_ratio,   "get_cash_liquidity_ratio" )
  df_sanitized <- .try_ratio( get_cash_assets_ratio,      "get_cash_assets_ratio" )
  df_sanitized <- .try_ratio( get_cash_on_hand,           "get_cash_on_hand" )
  df_sanitized <- .try_ratio( get_cash_burn_ratio,        "get_cash_burn_ratio" )
  df_sanitized <- .try_ratio( get_days_cash_operations,   "get_days_cash_operations" )
  df_sanitized <- .try_ratio( get_days_cash_investments,  "get_days_cash_investments" )
  df_sanitized <- .try_ratio( get_months_cash_operations, "get_months_cash_operations" )
  df_sanitized <- .try_ratio( get_liquid_assets_months,   "get_liquid_assets_months" )

  # ---- Leverage & solvency ----
  df_sanitized <- .try_ratio( get_debt_assets_ratio,      "get_debt_assets_ratio" )
  df_sanitized <- .try_ratio( get_debt_equity_ratio,      "get_debt_equity_ratio" )
  df_sanitized <- .try_ratio( get_debt_netassets_ratio,   "get_debt_netassets_ratio" )
  df_sanitized <- .try_ratio( get_debt_shortterm_ratio,   "get_debt_shortterm_ratio" )
  df_sanitized <- .try_ratio( get_debt_secured_ratio,     "get_debt_secured_ratio" )
  df_sanitized <- .try_ratio( get_debt_unsecured_ratio,   "get_debt_unsecured_ratio" )

  # ---- Revenue composition ----
  df_sanitized <- .try_ratio( get_self_sufficiency_ratio,      "get_self_sufficiency_ratio" )
  df_sanitized <- .try_ratio( get_earned_income_ratio,         "get_earned_income_ratio" )
  df_sanitized <- .try_ratio( get_grants_govt_ratio,           "get_grants_govt_ratio" )
  df_sanitized <- .try_ratio( get_donations_revenue_ratio,     "get_donations_revenue_ratio" )
  df_sanitized <- .try_ratio( get_investment_income_ratio,     "get_investment_income_ratio" )
  df_sanitized <- .try_ratio( get_assets_revenue_ratio,        "get_assets_revenue_ratio" )
  df_sanitized <- .try_ratio( get_revenue_fedcampaign_ratio,   "get_revenue_fedcampaign_ratio" )
  df_sanitized <- .try_ratio( get_revenue_membdues_ratio,      "get_revenue_membdues_ratio" )
  df_sanitized <- .try_ratio( get_revenue_fundevents_ratio,    "get_revenue_fundevents_ratio" )
  df_sanitized <- .try_ratio( get_revenue_reltdorgs_ratio,     "get_revenue_reltdorgs_ratio" )
  df_sanitized <- .try_ratio( get_revenue_programs_ratio,      "get_revenue_programs_ratio" )

  # ---- Expense ratios ----
  df_sanitized <- .try_ratio( get_program_expenses_ratio,         "get_program_expenses_ratio" )
  df_sanitized <- .try_ratio( get_expenses_admin_ratio,           "get_expenses_admin_ratio" )
  df_sanitized <- .try_ratio( get_overhead_ratio,                 "get_overhead_ratio" )
  df_sanitized <- .try_ratio( get_fundraising_efficiency_ratio,   "get_fundraising_efficiency_ratio" )
  df_sanitized <- .try_ratio( get_profit_margin_postdepr,         "get_profit_margin_postdepr" )
  df_sanitized <- .try_ratio( get_profit_margin_predepr,          "get_profit_margin_predepr" )
  df_sanitized <- .try_ratio( get_expenses_grants_ratio,          "get_expenses_grants_ratio" )
  df_sanitized <- .try_ratio( get_expenses_membbenefits_ratio,    "get_expenses_membbenefits_ratio" )
  df_sanitized <- .try_ratio( get_expenses_compensation_ratio,    "get_expenses_compensation_ratio" )
  df_sanitized <- .try_ratio( get_expenses_feesforservice_ratio,  "get_expenses_feesforservice_ratio" )
  df_sanitized <- .try_ratio( get_expenses_affiliates_ratio,      "get_expenses_affiliates_ratio" )

  # ---- Balance sheet structure ----
  df_sanitized <- .try_ratio( get_equity_ratio,               "get_equity_ratio" )
  df_sanitized <- .try_ratio( get_netassets_composition_ratio,"get_netassets_composition_ratio" )
  df_sanitized <- .try_ratio( get_operating_reserve_ratio,    "get_operating_reserve_ratio" )
  df_sanitized <- .try_ratio( get_land_assets_gross_ratio,    "get_land_assets_gross_ratio" )
  df_sanitized <- .try_ratio( get_land_assets_net_ratio,      "get_land_assets_net_ratio" )
  df_sanitized <- .try_ratio( get_investments_assets_ratio,   "get_investments_assets_ratio" )
  df_sanitized <- .try_ratio( get_netassets_growth_ratio,     "get_netassets_growth_ratio" )

  # ---- Performance ----
  df_sanitized <- .try_ratio( get_surplus_margin_ratio,    "get_surplus_margin_ratio" )
  df_sanitized <- .try_ratio( get_return_assets_ratio,     "get_return_assets_ratio" )
  df_sanitized <- .try_ratio( get_return_netassets_ratio,  "get_return_netassets_ratio" )

  # ---- Filter to requested metric variants ----
  original_cols <- colnames( df )
  all_new_cols  <- setdiff( colnames( df_sanitized ), original_cols )

  suffix_pattern <- paste0(
    "(",
    paste( c(
      if ( "ratio" %in% metrics ) "^[a-z][a-z0-9_]+$",
      if ( "w"     %in% metrics ) "_w$",
      if ( "z"     %in% metrics ) "_z$",
      if ( "p"     %in% metrics ) "_p$"
    ), collapse = "|" ),
    ")"
  )
  keep_cols <- all_new_cols[ grepl( suffix_pattern, all_new_cols ) ]

  if (verbose) message( "Done. ", length(keep_cols), " metric column(s) produced." )

  if ( append_to_df ) {
    result <- df
    result[ , keep_cols ] <- dplyr::select( df_sanitized, dplyr::all_of( keep_cols ) )
    return( as.data.frame( result ) )
  } else {
    id_cols <- intersect( .IDVARS, colnames( df_sanitized ) )
    return( as.data.frame( dplyr::select( df_sanitized, dplyr::all_of( c( id_cols, keep_cols ) ) ) ) )
  }
}
