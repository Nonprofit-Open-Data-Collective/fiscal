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
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param total_assets Total assets, EOY. (On 990: Part X, line 16B; On EZ: Part II, line 25B; \code{F9_10_ASSET_TOT_EOY})
#' @param total_revenue Total revenue. Accepts one or two column names; if two are provided
#'   they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT};
#'   On EZ: Part I, line 9; \code{F9_01_REV_TOT_CY})
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
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{arr}   — asset revenue ratio (raw)
#'     \item \code{arr_w} — winsorized version
#'     \item \code{arr_z} — standardized z-score (based on winsorized values)
#'     \item \code{arr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The asset revenue ratio indicates how large an organization's asset base is relative to
#' the revenue it generates each year. A higher ratio may indicate asset-heavy operations,
#' significant endowment holdings, or declining revenue.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_TOT_EOY}: Total assets, EOY (\code{total_assets})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue from Part VIII (\code{total_revenue}, 990)
#'   \item \code{F9_01_REV_TOT_CY}: Total revenue from Part I (\code{total_revenue}, 990EZ)
#' }
#'
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, NA values in
#'   the financial input columns are imputed to zero before the ratio is computed,
#'   respecting form scope: Part X and VIII/IX fields (990 only) are imputed only
#'   for 990 filers; Part I summary fields (990 + 990EZ) are imputed for all filers.
#'   The returned dataframe always contains the original unmodified input columns.
#'
#' @param summarize Logical. If \code{TRUE}, prints a \code{summary()} of
#'   the results and plots density curves for all four output columns
#'   (raw, winsorized, z-score, percentile). Defaults to \code{FALSE}.
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
