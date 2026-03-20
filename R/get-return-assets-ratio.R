###---------------------------------------------------
###   RETURN ON ASSETS (ROA)
###---------------------------------------------------

#' @title
#' Return on Assets
#'
#' @description
#' Net surplus or deficit as a share of total assets.
#'
#' **Formula:**
#' ```
#' roa = revenues_less_expenses / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param revenues_less_expenses Revenues less expenses (net surplus or deficit) for the
#'   current year. Accepts one or two column names; if two are provided they are coalesced
#'   with the 990 value taking priority over 990EZ.
#'
#' @param total_assets Total assets, EOY.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_return_assets_ratio( df,
#'   revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
#'   total_assets           = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `return_assets`   - return on assets (raw)
#'     - `return_assets_w` - winsorized version
#'     - `return_assets_z` - standardized z-score (based on winsorized values)
#'     - `return_assets_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' Return on assets (ROA) measures how effectively an organization uses its entire asset
#' base to generate a financial surplus. In the nonprofit context this is sometimes called
#' the return on investment ratio, though "investment" here means the total asset base
#' rather than financial securities. A positive ROA indicates the organization ended the
#' year with more resources than it spent; a negative ROA indicates a deficit year.
#'
#' ROA is most useful as a longitudinal indicator - watching whether an organization is
#' becoming more or less efficient at generating surpluses per dollar of assets over
#' time - and for cross-sectional comparisons within subsectors where asset structures
#' are similar. It is frequently used as a dependent variable in financial vulnerability
#' models and as a component of composite fiscal health scores.
#'
#' ## Formula variations and their sources
#'
#' In commercial accounting, ROA is typically computed as net income divided by average
#' total assets (the mean of beginning- and end-of-year values), to avoid the distortion
#' of large mid-year asset changes. The nonprofit literature generally uses end-of-year
#' assets as the denominator (Greenlee & Trussel 2000, Keating et al. 2005) because
#' beginning-of-year balance sheet data is not reliably available on 990 filings prior
#' to recent years, and because the analytical focus is on the stock of resources at the
#' time of reporting rather than a flow average.
#'
#' The numerator also differs from the commercial definition. Commercial ROA uses net
#' income after taxes. For nonprofits, the equivalent is revenues less expenses
#' (`F9_01_EXP_REV_LESS_EXP_CY`, Part I line 19), which is the IRS-reported
#' summary figure for the change in net assets before other adjustments. Some studies
#' use the Part XI reconciliation value instead, but that field is 990-only and has
#' higher rates of missing data. The Part I summary field is available on both 990 and
#' 990EZ forms and is used here for broader coverage.
#'
#' ## Why this formula was chosen
#'
#' End-of-year total assets in the denominator is the most common operationalization in
#' the nonprofit empirical literature and is the most reproducible given 990 data
#' availability. The Part I revenues-less-expenses numerator was chosen over the Part XI
#' reconciliation figure because it covers 990EZ filers, has lower rates of missingness,
#' and is the value most analysts expect when referencing the "net income" line on the
#' annual filing. The result is closely related to [get_surplus_margin_ratio()]
#' (which scales the same numerator by total revenue rather than total assets), and
#' to [get_return_netassets_ratio()] (which scales by beginning net assets).
#'
#' ## Canonical citations
#'
#'
#'   - Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. *Nonprofit Management and Leadership*, 11(2),
#'     199-210. - One of the earliest systematic applications of ROA to nonprofit
#'     financial health prediction using 990 data.
#'   - Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005). Assessing
#'     financial vulnerability in the nonprofit sector. *Harvard Business School
#'     Working Paper 04-016*. - Provides comparative analysis of ROA alongside other
#'     fiscal health indicators.
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460. - Foundational paper; ROA variants appear
#'     as components of composite vulnerability scores in subsequent applications.
#'   - Wicker, P., Feiler, S. & Breuer, C. (2013). Organizational mission and
#'     financial vulnerability: A financial health analysis of German nonprofit sport
#'     clubs. *VOLUNTAS: International Journal of Voluntary and Nonprofit
#'     Organizations*, 24(4), 991-1013. - Applies ROA in a cross-national nonprofit
#'     context; demonstrates subsector variation.
#'
#'
#' ## Definitional range
#'
#' ROA is unbounded in both directions. A ratio of zero means expenses exactly equalled
#' revenues. Positive values indicate surplus years; negative values indicate deficit
#' years. For nonprofits, where perpetual surpluses raise concerns about mission
#' prioritization and perpetual deficits signal sustainability risk, the meaningful
#' range is approximately \[-0.20, 0.20\] for most organizations in most years.
#' Values outside this range are not impossible but typically reflect unusual
#' circumstances (large asset disposals, one-time gifts, major capital campaigns, or
#' accounting restatements) rather than stable operating performance.
#'
#' Values can be distorted by very small total asset bases (newly formed organizations)
#' or by organizations whose assets consist almost entirely of illiquid fixed property,
#' making the denominator a poor proxy for the operational resource base.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - **Near-zero is normal**: Unlike commercial firms where consistent positive
#'     ROA is expected, nonprofits typically operate close to break-even by design.
#'     An ROA consistently near zero is a sign of disciplined budget management, not
#'     poor performance.
#'   - **Moderate positive surplus**: Values in the range of 0.02 to 0.07
#'     (2-7\%) are generally considered healthy - the organization is building modest
#'     reserves without appearing to hoard resources at the expense of mission delivery.
#'   - **Sustained negative ROA**: Two or more consecutive years with ROA
#'     below -0.05 is a common threshold used in financial vulnerability studies to
#'     classify an organization as at risk (Greenlee & Trussel 2000, Keating et al.
#'     2005).
#'   - **Subsector variation**: Arts and culture organizations tend to have
#'     more variable ROA than human services organizations with stable government
#'     contracts. Health care nonprofits tend toward thin positive margins.
#'   - **Asset structure matters**: Capital-intensive organizations
#'     (hospitals, universities, housing providers) have large denominators and will
#'     mechanically produce low ROA even when financially healthy. Comparisons across
#'     capital structures require caution.
#'
#'
#' ## Variables used:
#'
#'   - `F9_01_EXP_REV_LESS_EXP_CY`: 
#'     Revenues less expenses, current year (`revenues_less_expenses`, 990 + 990EZ)
#'   - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`, 990)
#'   - `F9_01_NAFB_ASSET_TOT_EOY`: Total assets from Part I (`total_assets`, 990EZ)
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
#' d <- get_return_assets_ratio( df = dat10k )
#' head( d[ , c( "return_assets", "return_assets_w", "return_assets_z", "return_assets_p" ) ] )
#'
#' @export
get_return_assets_ratio <- function( df,
                     revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
                     total_assets           = c( "F9_10_ASSET_TOT_EOY",
                                                 "F9_01_NAFB_ASSET_TOT_EOY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, revenues_less_expenses, total_assets,
                   "revenues_less_expenses", "total_assets" )

  if ( length( revenues_less_expenses ) > 2 )
    stop( "`revenues_less_expenses` must be one or two column names." )
  if ( length( total_assets ) > 2 )
    stop( "`total_assets` must be one or two column names." )

  all_cols <- c( revenues_less_expenses, total_assets )
  vars <- c( revenues_less_expenses, total_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  s <- resolve_col( dt, revenues_less_expenses )
  a <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( a == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  a[ a == 0 ] <- NA

  roa <- s / a

  v <- winsorize_var( roa, winsorize )
  RETURN_ASSETS <- data.frame( return_assets   = v$raw,
                     return_assets_w = v$winsorized,
                     return_assets_z = v$z,
                     return_assets_p = v$pctile )

  if ( summarize ) {
    print( summary( RETURN_ASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( RETURN_ASSETS$return_assets, na.rm = TRUE ), main = "RETURN_ASSETS (raw)" )
    plot( density( RETURN_ASSETS$return_assets_w, na.rm = TRUE ), main = "RETURN_ASSETS Winsorized" )
    plot( density( RETURN_ASSETS$return_assets_z, na.rm = TRUE ), main = "RETURN_ASSETS Standardized (Z)" )
    plot( density( RETURN_ASSETS$return_assets_p, na.rm = TRUE ), main = "RETURN_ASSETS Percentile" )
  }

  return( cbind( df, RETURN_ASSETS ) )
}
