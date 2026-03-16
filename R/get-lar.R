###---------------------------------------------------
###   LAND ASSET RATIO
###---------------------------------------------------

#' @title
#' Land Asset Ratio
#'
#' @description
#' Share of total assets invested in land, buildings, and equipment.
#'
#' **Formula:**
#' ```
#' lar = land_buildings_equipment / total_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param land_buildings Net land, buildings, and equipment (after depreciation), EOY.
#'   (On 990: Part X, line 10c; \code{F9_10_ASSET_LAND_BLDG_DEPREC})
#' @param total_assets Total assets, EOY. (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_lar( df,
#'   land_buildings = "F9_10_ASSET_LAND_BLDG_DEPREC",
#'   total_assets   = "F9_10_ASSET_TOT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{lar}   — land asset ratio (raw)
#'     \item \code{lar_w} — winsorized version
#'     \item \code{lar_z} — standardized z-score (based on winsorized values)
#'     \item \code{lar_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The land asset ratio measures what share of total assets are held as fixed property
#' (land, buildings, and equipment). Higher values may indicate capital-intensive operations
#' or limited asset liquidity.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_LAND_BLDG_DEPREC}: Net land, buildings, and equipment, EOY (\code{land_buildings})
#'   \item \code{F9_10_ASSET_TOT_EOY}: Total assets, EOY (\code{total_assets})
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
#' d <- get_lar( df = dat10k )
#' head( d[ , c( "lar", "lar_w", "lar_z", "lar_p" ) ] )
#'
#' @export
get_lar <- function( df,
                     land_buildings = "F9_10_ASSET_LAND_BLDG_DEPREC",
                     total_assets   = "F9_10_ASSET_TOT_EOY",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, land_buildings, total_assets, "land_buildings", "total_assets" )

  if ( length( land_buildings ) > 2 ) stop( "`land_buildings` must be one or two column names." )
  if ( length( total_assets )   > 2 ) stop( "`total_assets` must be one or two column names."   )

  vars <- c( land_buildings, total_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  l <- resolve_col( dt, land_buildings )
  a <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( a == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  a[ a == 0 ] <- NA

  lar <- l / a

  v <- winsorize_var( lar, winsorize )
  LAR <- data.frame( lar   = v$raw,
                     lar_w = v$winsorized,
                     lar_z = v$z,
                     lar_p = v$pctile )

  if ( summarize ) {
    print( summary( LAR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( LAR$lar, na.rm = TRUE ), main = "LAR (raw)" )
    plot( density( LAR$lar_w, na.rm = TRUE ), main = "LAR Winsorized" )
    plot( density( LAR$lar_z, na.rm = TRUE ), main = "LAR Standardized (Z)" )
    plot( density( LAR$lar_p, na.rm = TRUE ), main = "LAR Percentile" )
  }

  return( cbind( df, LAR ) )
}
