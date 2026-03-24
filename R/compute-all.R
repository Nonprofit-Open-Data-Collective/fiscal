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
    if (verbose) message( "\n}} Computing ", label, " ..." )
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
      if ( "ratio" %in% metrics ) "^[a-z][a-z0-9_]+$(?<!_w)(?<!_z)(?<!_p)",
      if ( "w"     %in% metrics ) "_w$",
      if ( "z"     %in% metrics ) "_z$",
      if ( "p"     %in% metrics ) "_p$"
    ), collapse = "|" ),
    ")"
  )
  keep_cols <- all_new_cols[ grepl( suffix_pattern, all_new_cols, perl = TRUE ) ]

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


#' Compute all fiscal health metrics year-by-year across a panel
#'
#' @description
#' A panel-safe wrapper around [compute_all()] that splits the data by
#' year, computes every fiscal health metric independently within each
#' year's slice, then stacks the results. This ensures that
#' winsorization bounds, normalization parameters, and percentile ranks
#' are all estimated from the cross-sectional distribution of a **single
#' year** rather than from the pooled multi-year distribution.
#'
#' **Why year-by-year computation matters:**
#'
#' [compute_all()] passes each raw ratio vector through
#' [apply_transformations()], which fits winsorization bounds and a
#' normalization model on the data it receives. On a pooled panel those
#' distributions are a mixture of N years, so:
#'
#' - A ratio at the 95th percentile in 2021 may rank at the 80th
#'   percentile in the pooled distribution if 2022–2024 values are higher.
#' - Winsorization clips to pooled extremes, not year-specific extremes.
#' - Z-scores are centred and scaled on pooled moments.
#'
#' Splitting by year before computing avoids all three problems: every
#' `_w`, `_z`, and `_p` column is interpretable as a within-year
#' percentile or standardised value, exactly as it would be for a
#' single-year call to [compute_all()].
#'
#' @param df A `data.frame` containing IRS 990 efile financial fields
#'   for multiple tax years. Must contain the column named by `year`.
#' @param year Name of the column that identifies the tax year. Default
#'   `"TAX_YEAR"`.
#' @param metrics Character vector of metric variants to return. Any
#'   combination of `"ratio"`, `"w"`, `"z"`, `"p"`. Default: all four.
#' @param append_to_df Logical. If `TRUE` (default), returns the original
#'   `df` with metric columns appended. If `FALSE`, returns only
#'   `.IDVARS` identifier columns plus the selected metric columns.
#' @param winsorize Winsorization proportion (default `0.98`).
#' @param verbose Logical. If `TRUE` (default), prints per-year progress
#'   from [compute_all()].
#'
#' @return A `data.frame` with the same row order as `df` and the same
#'   metric columns that [compute_all()] would produce, but with
#'   `_w` / `_z` / `_p` values computed within each year's
#'   cross-sectional distribution.
#'
#' @details
#' ## Implementation
#'
#' Internally the function:
#'
#' 1. Validates that `year` is present and that the panel contains more
#'    than one year (a single-year panel works but a warning is issued
#'    since [compute_all()] would give identical results).
#' 2. Tags each row with its original position so output rows can be
#'    restored to the input order after splitting and stacking.
#' 3. Splits `df` into a list of single-year slices using
#'    `data.table::split()`.
#' 4. Calls [compute_all()] on each slice with `verbose` passed through
#'    so per-year messages are visible (or suppressed) as requested.
#' 5. Stacks the per-year results with `data.table::rbindlist()`, which
#'    handles any columns that are present in some years but absent in
#'    others (e.g. if a 990EZ-only year lacks Part VIII columns) by
#'    filling with `NA`.
#' 6. Restores the original row order and drops the internal row-tag
#'    column.
#'
#' ## Panel size considerations
#'
#' On large panels (e.g. 5 years × 50 000 orgs) the per-year loop runs
#' [compute_all()] five times on ~50 000-row slices rather than once on
#' a 250 000-row frame. Each slice call is faster and uses less peak
#' memory; the `data.table::rbindlist()` stack is O(N) with no copies.
#'
#' @seealso [compute_all()], [get_panel()], [deduplicate()],
#'   [panel_smooth()]
#'
#' @examples
#' \dontrun{
#' panel <- get_panel( years = 2019:2022 )
#' panel <- deduplicate( panel )
#'
#' # Year-safe metric computation
#' panel_ratios <- compute_all_panel( panel )
#'
#' # Compare: without year splitting (pooled — incorrect for _w/_z/_p)
#' panel_ratios_pooled <- compute_all( panel )
#'
#' # Check that percentile ranks are within-year:
#' library( data.table )
#' setDT( panel_ratios )
#' panel_ratios[ , range(debt_assets_p), by = TAX_YEAR ]
#' # All years should show 1 to 100.
#' }
#'
#' @export
compute_all_panel <- function( df,
                               year         = "TAX_YEAR",
                               metrics      = c("ratio","w","z","p"),
                               append_to_df = TRUE,
                               winsorize    = 0.98,
                               verbose      = TRUE ) {

  # ---- input checks -------------------------------------------------------
  if ( !is.data.frame(df) )
    stop( "`df` must be a data.frame." )

  if ( !year %in% names(df) )
    stop( paste0( "`year` column not found in df: \"", year, "\". ",
                  "Set year= to the name of your tax-year column." ) )

  years_present <- sort( unique( df[[year]] ) )
  n_years       <- length( years_present )

  if ( n_years == 0 )
    stop( "No non-NA values found in year column \"", year, "\"." )

  if ( n_years == 1 )
    warning( "Panel contains only one year (",  years_present, "). ",
             "compute_all() would give the same result. ",
             "Proceeding anyway.", call. = FALSE )

  # ---- tag rows with original position for order restoration --------------
  n_cols_input <- ncol(df)   # capture before adding .row_order.
  df[[".row_order."]] <- seq_len( nrow(df) )

  # ---- split by year, compute, collect ------------------------------------
  year_list <- split( df, df[[year]] )

  if (verbose) message(
    "Computing metrics year-by-year across ",
    n_years, " year(s): ",
    paste( years_present, collapse = ", " ), " ..."
  )

  result_list <- vector( "list", n_years )

  for ( i in seq_along(year_list) ) {

    yr    <- years_present[[i]]
    slice <- year_list[[i]]

    if (verbose) message( "\n===  Year ", yr,
                          " (", nrow(slice), " rows)  ===" )

    result_list[[i]] <- tryCatch(
      compute_all(
        df           = slice,
        metrics      = metrics,
        append_to_df = append_to_df,
        winsorize    = winsorize,
        verbose      = verbose
      ),
      error = function(e) {
        warning( "Year ", yr, " compute_all() failed: ",
                 conditionMessage(e),
                 " — returning input slice unchanged.", call. = FALSE )
        # Return the slice so the year is present with NA metric columns
        slice
      }
    )
  }

  # ---- stack and restore original order -----------------------------------
  if (verbose) message( "\nStacking ", n_years, " year(s) ..." )

  out <- data.table::rbindlist( result_list, fill = TRUE )

  # Restore the original row order then drop the tag
  data.table::setorder( out, .row_order. )
  out[[".row_order."]] <- NULL

  n_new <- ncol(out) - n_cols_input
  if (verbose) message(
    "Done. ", n_new, " metric column(s) added across ",
    n_years, " year(s)."
  )

  as.data.frame( out )
}
