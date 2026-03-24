###---------------------------------------------------
###   QUICK RATIO
###---------------------------------------------------

#' @title
#' Quick Ratio
#'
#' @description
#' Measures liquidity using only the most liquid assets, excluding inventory and prepaid expenses.
#'
#' **Formula:**
#' ```
#' qr = quick_assets / current_liabilities
#'
#' quick_assets        = cash + savings + pledges_receivable + accounts_receivable
#' current_liabilities = accounts_payable + grants_payable
#' ```
#'
#' **Definitional Range**
#'
#' The quick ratio is bounded below at zero (no asset values are negative on a
#' properly prepared balance sheet) and is unbounded above. In practice the empirical
#' range for nonprofits is approximately \[0, 10\], with extreme values above 5 typically
#' indicating cash hoarding, an unusually large receivables balance, or an organization
#' with minimal current liabilities (e.g., a grant-making foundation with no accounts
#' payable).
#'
#' Values below zero can occur when net receivables are negative (allowances for
#' uncollectible pledges exceed gross pledges) or when the denominator carries
#' accounting adjustments. Very large values can result when current liabilities are
#' near zero, producing ratios that are mathematically valid but practically
#' uninterpretable. The default winsorization at the 1st/99th percentiles addresses both
#' extremes.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **Above 1.0**: Conventionally adequate; liquid assets cover current liabilities.
#'   - **0.5-1.0**: Potential short-term stress.
#'   - **Below 0.5**: Common vulnerability threshold (Tuckman & Chang 1991).
#'   - Very high values (above 3-5) may signal excess cash hoarding rather than
#'     financial health.
#'   - A declining trend over consecutive years is a stronger warning signal than
#'     any single year's value.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Short-term investments (savings), EOY.
#' @param pledges_receivable Net pledges and grants receivable, EOY.
#' @param accounts_receivable Accounts receivable, net, EOY.
#' @param accounts_payable Accounts payable and accrued expenses, EOY.
#' @param grants_payable Grants and similar amounts payable, EOY.
#' @param numerator Optional. A pre-aggregated column for quick assets. Cannot be combined
#'   with the individual asset arguments.
#' @param denominator Optional. A pre-aggregated column for current liabilities. Cannot be
#'   combined with `accounts_payable` or `grants_payable`.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zp"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_quick_ratio( df,
#'   cash                = "F9_10_ASSET_CASH_EOY",
#'   savings             = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
#'   accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
#'   grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98 ,
#'   range     = "zp",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `quick`   - quick ratio (raw)
#'     - `quick_w` - winsorized version
#'     - `quick_z` - standardized z-score (based on winsorized values)
#'     - `quick_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The quick ratio (also called the acid-test ratio) is a short-term liquidity measure
#' that asks whether an organization could pay all of its current obligations immediately,
#' using only assets that can be converted to cash within days or weeks rather than months.
#' Unlike the current ratio ([get_current_ratio()]), it excludes inventory and
#' prepaid expenses, which are less reliably liquid. For nonprofits this distinction is
#' especially meaningful: prepaid expenses (insurance premiums, deposits) cannot be
#' recovered quickly, so including them in a liquidity test is misleading.
#'
#' The quick ratio is most useful for detecting near-term insolvency risk, stress-testing
#' an organization's ability to absorb a sudden revenue disruption, and comparing
#' liquidity positions across organizations of different sizes. It is a common component
#' of multi-ratio financial health scoring models in the nonprofit literature.
#'
#' ## Formula variations and their sources
#'
#' The standard for-profit formula (Brigham & Houston 2019) defines quick assets as
#' cash + marketable securities + net receivables, and current liabilities as all
#' obligations due within one year. Nonprofit applications require two adaptations:
#'
#'
#'   - **Receivables**: Pledges receivable are included in the nonprofit
#'     version because they are a normal operating asset with a defined cash-in timeline.
#'     Some analysts exclude multi-year pledges (only the current-year portion is truly
#'     liquid), but the 990 Part X does not break pledges into current vs. long-term
#'     portions, so the full net amount is used here.
#'   - **Current liabilities**: For-profit firms sum all current liabilities
#'     (accounts payable, short-term debt, accrued liabilities, current portion of
#'     long-term debt, deferred revenue, etc.). The 990 balance sheet does not label
#'     liabilities as current vs. long-term. This implementation uses accounts payable
#'     plus grants payable as the best available proxy for near-term obligations,
#'     following Tuckman & Chang (1991) and Greenlee & Trussel (2000). Mortgage notes
#'     payable (line 23B) are excluded because they are long-term in nature, and
#'     unsecured notes (line 24B) are excluded because they typically carry defined
#'     maturity dates outside a 12-month window.
#'
#'
#' An alternative numerator used by some analysts (Hager 2001) adds short-term investments
#' held for sale (`F9_10_ASSET_INV_SALE_EOY`) on the grounds that these can be
#' liquidated quickly. That field is excluded here because many nonprofits classify
#' long-term endowment holdings under the same line, making it an unreliable current
#' asset proxy.
#'
#' ## Why this formula was chosen
#'
#' The formula implemented here follows the approach used in the majority of empirical
#' nonprofit financial health studies: a restricted numerator (cash, savings, and
#' receivables only) divided by near-term payables. This is the most conservative
#' defensible version given the ambiguity of 990 balance sheet categories. It is
#' consistent with the operationalization in Tuckman & Chang (1991), the foundational
#' paper establishing financial vulnerability indicators for nonprofits, and with the
#' panel analyses in Greenlee & Trussel (2000) and Keating et al. (2005).
#'
#' ## Canonical citations
#'
#'
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460. - Introduced the four-indicator financial
#'     vulnerability framework for nonprofits; liquidity is a core component.
#'   - Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. *Nonprofit Management and Leadership*, 11(2),
#'     199-210. - Applied and extended Tuckman & Chang using logistic regression on
#'     IRS 990 data.
#'   - Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005). Assessing
#'     financial vulnerability in the nonprofit sector. *Harvard Business School
#'     Working Paper 04-016*. - Comprehensive review of ratio-based vulnerability
#'     measures, including liquidity.
#'   - Hager, M.A. (2001). Financial vulnerability among arts organizations: A test
#'     of the Tuckman-Chang measures. *Nonprofit and Voluntary Sector Quarterly*,
#'     30(2), 376-392. - Tests alternative operationalizations including extended
#'     liquidity numerators in an arts sector context.
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley. - Practitioner text with the most detailed
#'     discussion of quick ratio benchmarks specific to nonprofits.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings and short-term investments, EOY (`savings`)
#'   - `F9_10_ASSET_PLEDGE_NET_EOY`: 
#'     Net pledges receivable, EOY (`pledges_receivable`)
#'   - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY (`accounts_receivable`)
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
#' d <- get_quick_ratio( df = dat10k )
#' head( d[ , c( "quick", "quick_w", "quick_z", "quick_p" ) ] )
#'
#' @export
get_quick_ratio <- function( df,
                    cash                = "F9_10_ASSET_CASH_EOY",
                    savings             = "F9_10_ASSET_SAVING_EOY",
                    pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                    accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                    accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
                    grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
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
  using_component_den <- !is.null( accounts_payable ) | !is.null( grants_payable )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual asset arguments (cash, savings, pledges_receivable, accounts_receivable), not both." )
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
                 accounts_payable, grants_payable, numerator, denominator )
  vars <- c( cash, savings, pledges_receivable, accounts_receivable, accounts_payable, grants_payable, numerator, denominator )
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
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ accounts_payable ]] + dt[[ grants_payable ]]
  }

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Current liabilities equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  qr <- num / den

  v <- apply_transformations( qr, winsorize, range )
  QUICK <- data.frame( quick   = v$raw,
                    quick_w = v$winsorized,
                    quick_z = v$z,
                    quick_p = v$pctile )

  if ( summarize ) {
    print( summary( QUICK ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( QUICK$quick, na.rm = TRUE ), main = "QUICK (raw)" )
    plot( density( QUICK$quick_w, na.rm = TRUE ), main = "QUICK Winsorized" )
    plot( density( QUICK$quick_z, na.rm = TRUE ), main = "QUICK Standardized (Z)" )
    plot( density( QUICK$quick_p, na.rm = TRUE ), main = "QUICK Percentile" )
  }

  return( cbind( df, QUICK ) )
}
