###---------------------------------------------------
###   CURRENT RATIO
###---------------------------------------------------

#' @importFrom magrittr "%>%"

#' @title
#' Current Ratio
#'
#' @description
#' Measures short-term liquidity by comparing current assets to current liabilities.
#'
#' **Formula:**
#' ```
#' cr = current_assets / current_liabilities
#'
#' current_assets      = cash + savings + pledges_receivable + accounts_receivable
#'                       + investment_sales + prepaid_expenses
#' current_liabilities = accounts_payable + grants_payable
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Short-term investments (savings), EOY.
#' @param pledges_receivable Net pledges and grants receivable, EOY.
#' @param accounts_receivable Accounts receivable, net, EOY.
#' @param investment_sales Investments held for sale or use, EOY.
#' @param prepaid_expenses Prepaid expenses and deferred charges, EOY.
#' @param accounts_payable Accounts payable and accrued expenses, EOY.
#' @param grants_payable Grants and similar amounts payable, EOY.
#' @param numerator Optional. A pre-aggregated column name for current assets, bypassing
#'   the individual asset arguments. Cannot be combined with those arguments.
#' @param denominator Optional. A pre-aggregated column name for current liabilities,
#'   bypassing `accounts_payable` and `grants_payable`. Cannot be combined
#'   with those arguments.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_current_ratio( df,
#'   cash                = "F9_10_ASSET_CASH_EOY",
#'   savings             = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
#'   investment_sales    = "F9_10_ASSET_INV_SALE_EOY",
#'   prepaid_expenses    = "F9_10_ASSET_EXP_PREPAID_EOY",
#'   accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
#'   grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `current`   - current ratio (raw)
#'     - `current_w` - winsorized version
#'     - `current_z` - standardized z-score (based on winsorized values)
#'     - `current_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The current ratio is the most widely used measure of short-term liquidity in both
#' commercial and nonprofit finance. It asks whether an organization has sufficient
#' near-term assets to meet its near-term obligations without requiring asset sales,
#' additional borrowing, or emergency fundraising. For nonprofits it is particularly
#' important as a stress-test, because grant revenue often arrives in irregular cycles
#' that do not align with payroll and vendor payment schedules.
#'
#' Unlike the quick ratio ([get_quick_ratio()]), the current ratio includes
#' prepaid expenses and investments held for sale, making it a somewhat more generous
#' (and less conservative) measure of liquidity. In practice for nonprofits, the two
#' ratios often move together since prepaid expenses are rarely a large share of assets.
#'
#' ## Formula variations and their sources
#'
#' The standard commercial definition (current assets / current liabilities) uses balance
#' sheet categories that are explicitly labelled as current. The 990 balance sheet (Part X)
#' does not use current vs. non-current labels. This implementation follows the
#' operationalization in Tuckman & Chang (1991) and subsequent studies by defining:
#'
#'
#'   - **Current assets**: cash + savings + pledges receivable + accounts
#'     receivable + investments held for sale + prepaid expenses (Part X lines 1, 2, 3,
#'     4, 8, 9).
#'   - **Current liabilities**: accounts payable + grants payable (Part X
#'     lines 17, 18). Mortgage and note obligations are excluded as long-term.
#'
#'
#' Some analysts also include inventory (Part X line 11) in current assets, but that
#' line is not widely populated in nonprofit 990 filings and is excluded here.
#'
#' ## Why this formula was chosen
#'
#' The asset components selected are the most reliably current items available on the
#' 990 balance sheet. The liability components exclude long-term debt, which matches
#' the intent of the current ratio (near-term coverage) while working within the
#' structural constraints of the 990. This formulation is consistent with the majority
#' of nonprofit financial health studies and the NCCS financial indicators project.
#'
#' ## Canonical citations
#'
#'
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460.
#'   - Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. *Nonprofit Management and Leadership*, 11(2),
#'     199-210.
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley.
#'
#'
#' ## Definitional range
#'
#' The current ratio is bounded below at zero and unbounded above. Values below 1.0
#' mean current liabilities exceed current assets. The empirical range for nonprofits
#' is approximately \[0, 15\], with most organizations clustered between 0.5 and 5.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - A ratio of 1.0 is the conventional adequacy threshold: liabilities are
#'     exactly covered by current assets.
#'   - Tuckman & Chang (1991) use a ratio below 1.0 as a financial vulnerability
#'     indicator. Values below 0.5 indicate acute short-term risk.
#'   - Values above 3.0-4.0 may indicate excess cash or slow collection of
#'     receivables rather than strong financial health.
#'   - Zietlow et al. (2007) report nonprofit sector medians generally in the
#'     \[1.0, 2.5\] range, with substantial subsector variation.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings and short-term investments, EOY (`savings`)
#'   - `F9_10_ASSET_PLEDGE_NET_EOY`: 
#'     Net pledges receivable, EOY (`pledges_receivable`)
#'   - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY (`accounts_receivable`)
#'   - `F9_10_ASSET_INV_SALE_EOY`: Investments held for sale, EOY (`investment_sales`)
#'   - `F9_10_ASSET_EXP_PREPAID_EOY`: 
#'     Prepaid expenses and deferred charges, EOY (`prepaid_expenses`)
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
#' d <- get_current_ratio( df = dat10k )
#' head( d[ , c( "current", "current_w", "current_z", "current_p" ) ] )
#'
#' @export
get_current_ratio <- function( df,
                    cash                = "F9_10_ASSET_CASH_EOY",
                    savings             = "F9_10_ASSET_SAVING_EOY",
                    pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                    accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                    investment_sales    = "F9_10_ASSET_INV_SALE_EOY",
                    prepaid_expenses    = "F9_10_ASSET_EXP_PREPAID_EOY",
                    accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
                    grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                    numerator   = NULL,
                    denominator = NULL,
                    winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( cash ) | !is.null( savings ) |
                         !is.null( pledges_receivable ) | !is.null( accounts_receivable ) |
                         !is.null( investment_sales ) | !is.null( prepaid_expenses )
  using_component_den <- !is.null( accounts_payable ) | !is.null( grants_payable )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual asset arguments, not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR the payable arguments (accounts_payable, grants_payable), not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual asset columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or the payable columns." )
  }

  all_cols <- c( cash, savings, pledges_receivable, accounts_receivable,
                 investment_sales, prepaid_expenses,
                 accounts_payable, grants_payable,
                 numerator, denominator )
  vars <- c( cash, savings, pledges_receivable, accounts_receivable, investment_sales, prepaid_expenses, accounts_payable, grants_payable, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ cash ]] + dt[[ savings ]] + dt[[ pledges_receivable ]] +
           dt[[ accounts_receivable ]] + dt[[ investment_sales ]] + dt[[ prepaid_expenses ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ accounts_payable ]] + dt[[ grants_payable ]]
  }

  message( paste0( "Current liabilities equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  cr <- num / den

  v <- winsorize_var( cr, winsorize )
  CURRENT <- data.frame( current   = v$raw,
                    current_w = v$winsorized,
                    current_z = v$z,
                    current_p = v$pctile )

  if ( summarize ) {
    print( summary( CURRENT ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( CURRENT$current, na.rm = TRUE ), main = "CURRENT (raw)" )
    plot( density( CURRENT$current_w, na.rm = TRUE ), main = "CURRENT Winsorized" )
    plot( density( CURRENT$current_z, na.rm = TRUE ), main = "CURRENT Standardized (Z)" )
    plot( density( CURRENT$current_p, na.rm = TRUE ), main = "CURRENT Percentile" )
  }

  return( cbind( df, CURRENT ) )
}
