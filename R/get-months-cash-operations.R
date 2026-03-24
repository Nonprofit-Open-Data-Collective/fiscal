###---------------------------------------------------
###   MONTHS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Months of Operating Cash on Hand
#'
#' @description
#' Number of months the organization can operate using available liquid assets.
#'
#' **Formula:**
#' ```
#' moch = liquid_assets / monthly_expenses
#'
#' liquid_assets    = cash + savings + pledges_receivable + accounts_receivable
#' monthly_expenses = ( total_expenses - depreciation ) / 12
#' ```
#'
#' **Definitional Range**
#'
#' Bounded below at zero; unbounded above. The typical operating range for nonprofits
#' is approximately \[0, 24\] months, with values above 12 months uncommon for
#' operating organizations (more typical for foundations).
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **Less than 1 month**: Acute liquidity risk.
#'   - **1-3 months**: Below adequate; common guidance is at least 3 months.
#'   - **3-6 months**: Generally considered healthy for most operating nonprofits.
#'   - **6+ months**: Strong reserve position.
#'   - The Nonprofit Finance Fund recommends 3-6 months as a target.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Short-term investments (savings), EOY.
#' @param pledges_receivable Pledges and grants receivable, EOY.
#' @param accounts_receivable Accounts receivable, EOY.
#' @param total_expenses Total functional expenses.
#' @param depreciation Depreciation, depletion, and amortization.
#' @param numerator Optional. A pre-aggregated column name for liquid assets, bypassing the
#'   individual `cash`, `savings`, `pledges_receivable`, and
#'   `accounts_receivable` arguments. Cannot be combined with those arguments.
#' @param denominator Optional. A pre-aggregated column name for the denominator (annual
#'   non-depreciation expenses). The function divides this by 12 internally. Cannot be
#'   combined with `total_expenses` or `depreciation`.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zp"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_months_cash_operations( df,
#'   cash                 = "F9_10_ASSET_CASH_EOY",
#'   savings              = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable   = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable  = "F9_10_ASSET_ACC_NET_EOY",
#'   total_expenses       = "F9_09_EXP_TOT_TOT",
#'   depreciation         = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98 ,
#'   range     = "zp",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `months_cash_ops`   - months of operating cash on hand (raw)
#'     - `months_cash_ops_w` - winsorized version
#'     - `months_cash_ops_z` - standardized z-score (based on winsorized values)
#'     - `months_cash_ops_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' Months of cash on hand is the monthly expression of [get_days_cash_operations()],
#' expressing the same liquidity concept in a unit that is more natural for annual
#' budget planning and board reporting. It is one of the most commonly reported metrics
#' in nonprofit financial dashboards and funder due diligence.
#'
#' ## Formula variations and their sources
#'
#' The denominator uses (total expenses - depreciation) / 12. Subtracting depreciation
#' follows standard practice (Zietlow et al. 2007) because the ratio measures cash
#' coverage of cash expenses. Some formulations use total expenses without the
#' depreciation adjustment; others use a rolling 12-month average of monthly expenses
#' to smooth seasonal variation, which is not possible with annual 990 data.
#'
#' ## Canonical citations
#'
#'
#'   - Nonprofit Finance Fund. *State of the Nonprofit Sector Survey* (annual).
#'     - Uses months of cash as a primary financial health indicator.
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley.
#'   - GuideStar/Candid. *Nonprofit Finance Indicators*. - Months of cash is
#'     one of the core metrics in the GuideStar financial health profile.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings (`savings`)
#'   - `F9_10_ASSET_PLEDGE_NET_EOY`: 
#'     Net pledges receivable (`pledges_receivable`)
#'   - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable (`accounts_receivable`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'   - `F9_09_EXP_DEPREC_TOT`: Depreciation and amortization (`depreciation`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, NA values in
#'   the financial input columns are imputed to zero before the ratio is computed,
#'   respecting form scope: Part X and VIII/IX fields (990 only) are imputed only
#'   for 990 filers; Part I summary fields (990 + 990EZ) are imputed for all filers.
#'   The returned dataframe always contains the original unmodified input columns.
#'
#' @param summarize Logical. If `TRUE`, prints a `summary()` of
#'   the results and plots density curves for all four output columns
#'   (raw, winsorized, z-score, percentile). Defaults to `FALSE`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' d <- get_months_cash_operations( df = dat10k )
#' head( d[ , c( "months_cash_ops", "months_cash_ops_w", "months_cash_ops_z", "months_cash_ops_p" ) ] )
#'
#' @export
get_months_cash_operations <- function( df,
                      cash                = "F9_10_ASSET_CASH_EOY",
                      savings             = "F9_10_ASSET_SAVING_EOY",
                      pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                      accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                      total_expenses      = "F9_09_EXP_TOT_TOT",
                      depreciation        = "F9_09_EXP_DEPREC_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98  ,
                     range     = "zp" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( cash ) | !is.null( savings ) |
                         !is.null( pledges_receivable ) | !is.null( accounts_receivable )
  using_component_den <- !is.null( total_expenses ) | !is.null( depreciation )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual asset arguments (cash, savings, pledges_receivable, accounts_receivable), not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR (total_expenses + depreciation), not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual asset columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or (total_expenses + depreciation)." )
  }

  all_cols <- c( cash, savings, pledges_receivable, accounts_receivable,
                 total_expenses, depreciation, numerator, denominator )
  vars <- c( cash, savings, pledges_receivable, accounts_receivable, total_expenses, depreciation, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ cash ]] + dt[[ savings ]] +
           dt[[ pledges_receivable ]] + dt[[ accounts_receivable ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]] / 12
  } else {
    den <- ( dt[[ total_expenses ]] - dt[[ depreciation ]] ) / 12
  }

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Monthly operating expenses equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  moch <- num / den

  v <- apply_transformations( moch, winsorize, range )
  MONTHS_CASH_OPS <- data.frame( months_cash_ops   = v$raw,
                      months_cash_ops_w = v$winsorized,
                      months_cash_ops_z = v$z,
                      months_cash_ops_p = v$pctile )

  if ( summarize ) {
    print( summary( MONTHS_CASH_OPS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( MONTHS_CASH_OPS$months_cash_ops, na.rm = TRUE ), main = "MONTHS_CASH_OPS (raw)" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_w, na.rm = TRUE ), main = "MONTHS_CASH_OPS Winsorized" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_z, na.rm = TRUE ), main = "MONTHS_CASH_OPS Standardized (Z)" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_p, na.rm = TRUE ), main = "MONTHS_CASH_OPS Percentile" )
  }

  return( cbind( df, MONTHS_CASH_OPS ) )
}
