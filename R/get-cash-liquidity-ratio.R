###---------------------------------------------------
###   CASH RATIO
###---------------------------------------------------

#' @title
#' Cash Ratio
#'
#' @description
#' Measures immediate liquidity using only cash and cash equivalents against current liabilities.
#'
#' **Formula:**
#' ```
#' casr = ( cash + savings ) / current_liabilities
#'
#' current_liabilities = accounts_payable + grants_payable
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Short-term investments (savings), EOY.
#' @param accounts_payable Accounts payable and accrued expenses, EOY.
#' @param grants_payable Grants and similar amounts payable, EOY.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_cash_liquidity_ratio( df,
#'   cash             = "F9_10_ASSET_CASH_EOY",
#'   savings          = "F9_10_ASSET_SAVING_EOY",
#'   accounts_payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
#'   grants_payable   = "F9_10_LIAB_GRANT_PAYABLE_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `cash_liq`   - cash ratio (raw)
#'     - `cash_liq_w` - winsorized version
#'     - `cash_liq_z` - standardized z-score (based on winsorized values)
#'     - `cash_liq_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The cash liquidity ratio is the most conservative liquidity test: it asks whether
#' an organization could cover its most immediate obligations using only cash and
#' savings, without collecting any receivables or selling any investments. It is
#' sometimes called the absolute liquidity ratio or cash coverage ratio. For nonprofits
#' with long or uncertain receivables collection cycles (e.g., government reimbursement
#' contracts, multi-year pledge collections), it provides a pessimistic but useful
#' lower bound on liquidity.
#'
#' This ratio is stricter than both the quick ratio ([get_quick_ratio()])
#' and the current ratio ([get_current_ratio()]), which both include
#' receivables in the numerator.
#'
#' ## Formula variations and their sources
#'
#' The commercial equivalent (sometimes called the "super-quick" ratio) uses only
#' cash and marketable securities. The nonprofit version here uses cash (line 1B) plus
#' savings/temporary investments (line 2B) as the numerator, and accounts payable plus
#' grants payable as the denominator. This follows the same liability proxy used across
#' the package (Tuckman & Chang 1991).
#'
#' ## Why this formula was chosen
#'
#' Cash and savings are the two fields on the 990 that most unambiguously represent
#' immediately available funds. The denominator (accounts payable + grants payable)
#' represents the most pressing and legally enforceable near-term obligations. Together
#' this pair provides the most conservative 990-based liquidity measure available.
#'
#' ## Canonical citations
#'
#'
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460.
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley.
#'
#'
#' ## Definitional range
#'
#' Bounded below at zero and unbounded above. A ratio of 1.0 means cash exactly covers
#' current payables. The empirical range for nonprofits is approximately \[0, 20\],
#' with most values between 0.1 and 5.0. Very high values are common for grant-making
#' foundations and endowed organizations that carry minimal accounts payable.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - Values below 0.5 suggest the organization cannot cover even half of its
#'     near-term payables from cash alone and is reliant on receivables collection
#'     or short-term borrowing to meet obligations.
#'   - Values of 1.0 or above indicate full cash coverage of current liabilities.
#'   - Because this is a highly conservative measure, values somewhat below 1.0
#'     are normal and not necessarily problematic if receivables are healthy.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY (`savings`)
#'   - `F9_10_LIAB_ACC_PAYABLE_EOY`: 
#'     Accounts payable and accrued expenses, EOY (`accounts_payable`)
#'   - `F9_10_LIAB_GRANT_PAYABLE_EOY`: 
#'     Grants and similar amounts payable, EOY (`grants_payable`)
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
#' d <- get_cash_liquidity_ratio( df = dat10k )
#' head( d[ , c( "cash_liq", "cash_liq_w", "cash_liq_z", "cash_liq_p" ) ] )
#'
#' @export
get_cash_liquidity_ratio <- function( df,
                      cash             = "F9_10_ASSET_CASH_EOY",
                      savings          = "F9_10_ASSET_SAVING_EOY",
                      accounts_payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
                      grants_payable   = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  for ( arg in list( cash, savings, accounts_payable, grants_payable ) ) {
    if ( is.null( arg ) ) stop( "All four column arguments must be specified." )
    if ( length( arg ) > 2 ) stop( "Each column argument must be one or two column names." )
  }

  all_cols <- c( cash, savings, accounts_payable, grants_payable )
  vars <- c( cash, savings, accounts_payable, grants_payable )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  c_val <- resolve_col( dt, cash )
  s_val <- resolve_col( dt, savings )
  ap    <- resolve_col( dt, accounts_payable )
  gp    <- resolve_col( dt, grants_payable )

  den <- ap + gp

  message( paste0( "Current liabilities equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  casr <- ( c_val + s_val ) / den

  v <- winsorize_var( casr, winsorize )
  CASH_LIQ <- data.frame( cash_liq   = v$raw,
                      cash_liq_w = v$winsorized,
                      cash_liq_z = v$z,
                      cash_liq_p = v$pctile )

  if ( summarize ) {
    print( summary( CASH_LIQ ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( CASH_LIQ$cash_liq, na.rm = TRUE ), main = "CASH_LIQ (raw)" )
    plot( density( CASH_LIQ$cash_liq_w, na.rm = TRUE ), main = "CASH_LIQ Winsorized" )
    plot( density( CASH_LIQ$cash_liq_z, na.rm = TRUE ), main = "CASH_LIQ Standardized (Z)" )
    plot( density( CASH_LIQ$cash_liq_p, na.rm = TRUE ), main = "CASH_LIQ Percentile" )
  }

  return( cbind( df, CASH_LIQ ) )
}
