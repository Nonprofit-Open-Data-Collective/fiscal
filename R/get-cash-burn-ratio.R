###---------------------------------------------------
###   BURN RATE
###---------------------------------------------------

#' @title
#' Burn Rate
#'
#' @description
#' Monthly rate at which an organization draws down its cash reserves.
#'
#' **Formula:**
#' ```
#' brr = ( cash_boy - cash_eoy ) / months_in_period
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash_eoy Cash on hand, end of year.
#' @param cash_boy Cash on hand, beginning of year.
#' @param months_in_period Number of months in the reporting period. Defaults to 12 for
#'   a standard annual filing. Adjust for short-year filers.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_cash_burn_ratio( df,
#'   cash_eoy         = "F9_10_ASSET_CASH_EOY",
#'   cash_boy         = "F9_10_ASSET_CASH_BOY",
#'   months_in_period = 12,
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `cash_burn`   - monthly burn rate in dollars (raw)
#'     - `cash_burn_w` - winsorized version
#'     - `cash_burn_z` - standardized z-score (based on winsorized values)
#'     - `cash_burn_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The burn rate ratio compares end-of-year cash to beginning-of-year cash, measuring
#' the rate at which an organization is accumulating or depleting its cash position over
#' a single fiscal year. A ratio below 1.0 means the organization ended the year with
#' less cash than it started with (burning cash); a ratio above 1.0 means it accumulated
#' cash. It is most useful for detecting multi-year cash erosion trends before they
#' reach a crisis point.
#'
#' ## Formula variations and their sources
#'
#' The term "burn rate" originates in startup finance, where it refers to monthly cash
#' outflows. The nonprofit adaptation here is an annual version: EOY cash divided by BOY
#' cash. An alternative formulation computes the dollar change (EOY - BOY) divided by
#' annual expenses to express the burn as a fraction of the operating budget, but that
#' version requires an additional variable and is better captured by the days/months of
#' cash functions ([get_days_cash_operations()], [get_months_cash_operations()]).
#'
#' ## Why this formula was chosen
#'
#' The EOY/BOY ratio is the simplest formulation and requires only two fields from the
#' Part X balance sheet. It directly answers "is the cash position improving or
#' deteriorating year-over-year?" without requiring expense data, making it calculable
#' even for organizations where Part IX data is incomplete.
#'
#' ## Canonical citations
#'
#'
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley. - Discusses cash trend analysis for nonprofits.
#'   - Nonprofit Finance Fund. (Annual). *State of the Nonprofit Sector Survey*.
#'     - Annual survey tracks cash position changes as a sector-wide indicator.
#'
#'
#' ## Definitional range
#'
#' Bounded below at zero (cash cannot be negative). Values below 1.0 indicate cash
#' depletion; values above 1.0 indicate cash accumulation. Ratios near zero indicate
#' near-complete cash exhaustion. The ratio is undefined (NA) when beginning-of-year
#' cash is zero, which occurs in the first year of operation or after a complete cash
#' drawdown.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - A ratio consistently below 1.0 over multiple years is a warning sign.
#'     A single year below 1.0 may reflect planned spending from reserves.
#'   - A ratio consistently above 1.0 suggests the organization is building
#'     liquidity reserves, which is generally positive up to a point.
#'   - Ratios below 0.50 in a single year (cash halved) warrant investigation
#'     into whether the decline reflects a structural revenue problem or a planned
#'     capital expenditure.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash on hand, end of year (`cash_eoy`)
#'   - `F9_10_ASSET_CASH_BOY`: Cash on hand, beginning of year (`cash_boy`)
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
#' d <- get_cash_burn_ratio( df = dat10k )
#' head( d[ , c( "cash_burn", "cash_burn_w", "cash_burn_z", "cash_burn_p" ) ] )
#'
#' @export
get_cash_burn_ratio <- function( df,
                     cash_eoy         = "F9_10_ASSET_CASH_EOY",
                     cash_boy         = "F9_10_ASSET_CASH_BOY",
                     months_in_period = 12,
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( is.null( cash_eoy ) ) stop( "`cash_eoy` cannot be NULL." )
  if ( is.null( cash_boy ) ) stop( "`cash_boy` cannot be NULL." )

  if ( !is.numeric( months_in_period ) || months_in_period <= 0 )
    stop( "`months_in_period` must be a positive number." )

  if ( length( cash_eoy ) > 2 ) stop( "`cash_eoy` must be one or two column names." )
  if ( length( cash_boy ) > 2 ) stop( "`cash_boy` must be one or two column names." )

  vars <- c( cash_eoy, cash_boy )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  eoy <- resolve_col( dt, cash_eoy )
  boy <- resolve_col( dt, cash_boy )

  brr <- ( boy - eoy ) / months_in_period

  v <- winsorize_var( brr, winsorize )
  CASH_BURN <- data.frame( cash_burn   = v$raw,
                     cash_burn_w = v$winsorized,
                     cash_burn_z = v$z,
                     cash_burn_p = v$pctile )

  if ( summarize ) {
    print( summary( CASH_BURN ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( CASH_BURN$cash_burn, na.rm = TRUE ), main = "CASH_BURN (raw)" )
    plot( density( CASH_BURN$cash_burn_w, na.rm = TRUE ), main = "CASH_BURN Winsorized" )
    plot( density( CASH_BURN$cash_burn_z, na.rm = TRUE ), main = "CASH_BURN Standardized (Z)" )
    plot( density( CASH_BURN$cash_burn_p, na.rm = TRUE ), main = "CASH_BURN Percentile" )
  }

  return( cbind( df, CASH_BURN ) )
}
