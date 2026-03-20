###---------------------------------------------------
###   ASSET REVENUE RATIO
###---------------------------------------------------

#' @title
#' Asset Revenue Ratio
#'
#' @description
#' Measures the size of an organization's asset base relative to its annual revenue.
#'
#' **Formula:**
#' ```
#' arr = total_assets / total_revenue
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param total_assets Total assets, EOY.
#' @param total_revenue Total revenue. Accepts one or two column names; if two are provided
#'   they are coalesced with the 990 value taking priority over 990EZ.
#'
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_assets_revenue_ratio( df,
#'   total_assets  = "F9_10_ASSET_TOT_EOY",
#'   total_revenue = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `assets_rev`   - asset revenue ratio (raw)
#'     - `assets_rev_w` - winsorized version
#'     - `assets_rev_z` - standardized z-score (based on winsorized values)
#'     - `assets_rev_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The asset revenue ratio measures how many dollars of assets are held per dollar of
#' annual revenue. It is an asset intensity measure: capital-intensive organizations
#' (hospitals, universities, housing providers with large real estate portfolios) show
#' high ratios; lean operating nonprofits show low ratios. It can also be interpreted
#' as an approximate measure of how long the organization could theoretically operate
#' on its asset base - though this is not a direct liquidity measure.
#'
#' A related interpretation is efficiency: a lower ratio may indicate more efficient
#' use of assets to generate revenue, though this is not universally true for nonprofits
#' where asset accumulation may reflect reserve-building rather than operational
#' inefficiency.
#'
#' ## Formula variations and their sources
#'
#' Total assets EOY / total revenue. The inverse (revenue / assets) is sometimes called
#' the asset turnover ratio and is more common in commercial analysis. For nonprofits,
#' the assets-to-revenue direction is more intuitive because it expresses asset intensity
#' in terms of revenue multiples. Some studies use average assets ((BOY + EOY)/2) in
#' the denominator to account for mid-year asset changes, but the EOY value is used
#' here for consistency and data availability.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good: Executive
#'     compensation in nonprofit organizations. *Policy and Society*, 20(4), 94-112.
#'   - Bowman, W. (2011). Financial capacity and sustainability of ordinary
#'     nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.
#'
#'
#' ## Definitional range
#'
#' Bounded below at zero; unbounded above. The ratio is undefined when revenue is zero.
#' Typical operating nonprofits show values in the \[0.5, 5.0\] range. Endowed
#' organizations and capital-intensive nonprofits may show values of 10 or higher.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - There is no universal benchmark. The ratio is most meaningful for
#'     within-subsector comparisons.
#'   - A ratio below 1.0 means annual revenue exceeds total assets - common for
#'     lean service organizations with minimal physical assets.
#'   - Very high ratios (above 10) typically indicate either a capital-heavy asset
#'     base (real estate, equipment) or a small revenue base relative to accumulated
#'     assets (endowed organizations).
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_TOT_EOY`: Total assets, end of year (`total_assets`, 990)
#'   - `F9_08_REV_TOT_TOT`: Total revenue from Part VIII (`total_revenue`, 990)
#'   - `F9_01_REV_TOT_CY`: Total revenue from Part I (`total_revenue`, 990EZ fallback)
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
#' d <- get_assets_revenue_ratio( df = dat10k )
#' head( d[ , c( "assets_rev", "assets_rev_w", "assets_rev_z", "assets_rev_p" ) ] )
#'
#' @export
get_assets_revenue_ratio <- function( df,
                     total_assets  = "F9_10_ASSET_TOT_EOY",
                     total_revenue = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, total_assets, total_revenue, "total_assets", "total_revenue" )

  if ( length( total_assets )  > 2 ) stop( "`total_assets` must be one or two column names."  )
  if ( length( total_revenue ) > 2 ) stop( "`total_revenue` must be one or two column names." )

  vars <- c( total_assets, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  a <- resolve_col( dt, total_assets )
  r <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( r == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  r[ r == 0 ] <- NA

  arr <- a / r

  v <- winsorize_var( arr, winsorize )
  ASSETS_REV <- data.frame( assets_rev   = v$raw,
                     assets_rev_w = v$winsorized,
                     assets_rev_z = v$z,
                     assets_rev_p = v$pctile )

  if ( summarize ) {
    print( summary( ASSETS_REV ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( ASSETS_REV$assets_rev, na.rm = TRUE ), main = "ASSETS_REV (raw)" )
    plot( density( ASSETS_REV$assets_rev_w, na.rm = TRUE ), main = "ASSETS_REV Winsorized" )
    plot( density( ASSETS_REV$assets_rev_z, na.rm = TRUE ), main = "ASSETS_REV Standardized (Z)" )
    plot( density( ASSETS_REV$assets_rev_p, na.rm = TRUE ), main = "ASSETS_REV Percentile" )
  }

  return( cbind( df, ASSETS_REV ) )
}
