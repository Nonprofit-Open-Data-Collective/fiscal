###---------------------------------------------------
###   CASH ON HAND
###---------------------------------------------------

#' @title
#' Cash on Hand
#'
#' @description
#' Absolute dollar amount of cash and liquid savings held at end of year.
#'
#' **Formula:**
#' ```
#' coh = cash + savings
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Short-term investments (savings), EOY.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_cash_on_hand( df,
#'   cash    = "F9_10_ASSET_CASH_EOY",
#'   savings = "F9_10_ASSET_SAVING_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `cash_on_hand`   - cash on hand in dollars (raw)
#'     - `cash_on_hand_w` - winsorized version
#'     - `cash_on_hand_z` - standardized z-score (based on winsorized values)
#'     - `cash_on_hand_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' Cash on hand is a simple dollar-denominated liquidity measure rather than a ratio.
#' It answers the most direct version of the liquidity question: how much immediately
#' spendable money does the organization have right now? It is most useful for
#' communicating financial position to non-technical audiences (boards, funders),
#' for absolute comparison against known fixed costs (monthly payroll, rent), and
#' as the numerator input for ratio-based measures such as months of cash
#' ([get_months_cash_operations()]) and days of cash
#' ([get_days_cash_operations()]).
#'
#' ## Formula variations and their sources
#'
#' Some practitioners define cash on hand as cash only (Part X line 1B), excluding
#' savings. Others include marketable securities or investments held for sale. This
#' implementation uses cash plus savings (lines 1B + 2B) as the most common definition
#' in the nonprofit financial management literature (Zietlow et al. 2007), since
#' savings accounts and money market funds are effectively as liquid as operating
#' cash for most organizations.
#'
#' ## Why this formula was chosen
#'
#' Lines 1B (cash) and 2B (savings and temporary cash investments) are the two most
#' reliably liquid items on the 990 balance sheet and are the least subject to
#' valuation uncertainty. Pledges and receivables are excluded because their timing
#' and collectibility are uncertain. This definition is consistent with the "cash and
#' cash equivalents" concept in nonprofit financial management practice.
#'
#' ## Canonical citations
#'
#'
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley. - Standard practitioner reference for
#'     cash management in nonprofits.
#'   - Calabrese, T.D. (2013). Running on empty: The operating reserves of U.S.
#'     nonprofit organizations. *Nonprofit Management and Leadership*, 23(3),
#'     281-302. - Examines operating cash and reserves as components of financial
#'     sustainability.
#'
#'
#' ## Definitional range
#'
#' Cash on hand is bounded below at zero (cannot be negative on a properly prepared
#' balance sheet) and unbounded above. Unlike ratio measures, the scale is in dollars,
#' so comparison across organizations of different sizes requires normalization. Use
#' [get_months_cash_operations()] or [get_days_cash_operations()]
#' for size-adjusted comparisons.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - As an absolute measure, benchmarks depend entirely on the organization's
#'     expense base. A common practitioner rule is to maintain at least 60-90 days
#'     of operating expenses in liquid cash.
#'   - The Nonprofit Finance Fund recommends a minimum of three months of operating
#'     expenses in accessible reserves, of which cash and savings should form the core.
#'   - Very large cash balances relative to annual expenses may attract scrutiny
#'     from donors and regulators about whether resources are being deployed toward mission.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY (`savings`)
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
#' d <- get_cash_on_hand( df = dat10k )
#' head( d[ , c( "cash_on_hand", "cash_on_hand_w", "cash_on_hand_z", "cash_on_hand_p" ) ] )
#'
#' @export
get_cash_on_hand <- function( df,
                     cash    = "F9_10_ASSET_CASH_EOY",
                     savings = "F9_10_ASSET_SAVING_EOY",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, cash, savings, "cash", "savings" )

  if ( length( cash )    > 2 ) stop( "`cash` must be one or two column names."    )
  if ( length( savings ) > 2 ) stop( "`savings` must be one or two column names." )

  vars <- c( cash, savings )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  c_val <- resolve_col( dt, cash )
  s_val <- resolve_col( dt, savings )

  coh <- c_val + s_val

  v <- winsorize_var( coh, winsorize )
  CASH_ON_HAND <- data.frame( cash_on_hand   = v$raw,
                     cash_on_hand_w = v$winsorized,
                     cash_on_hand_z = v$z,
                     cash_on_hand_p = v$pctile )

  if ( summarize ) {
    print( summary( CASH_ON_HAND ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( CASH_ON_HAND$cash_on_hand, na.rm = TRUE ), main = "CASH_ON_HAND (raw)" )
    plot( density( CASH_ON_HAND$cash_on_hand_w, na.rm = TRUE ), main = "CASH_ON_HAND Winsorized" )
    plot( density( CASH_ON_HAND$cash_on_hand_z, na.rm = TRUE ), main = "CASH_ON_HAND Standardized (Z)" )
    plot( density( CASH_ON_HAND$cash_on_hand_p, na.rm = TRUE ), main = "CASH_ON_HAND Percentile" )
  }

  return( cbind( df, CASH_ON_HAND ) )
}
