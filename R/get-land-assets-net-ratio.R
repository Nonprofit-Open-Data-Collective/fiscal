###---------------------------------------------------
###   LAND, BUILDINGS, AND EQUIPMENT TO ASSETS RATIO (NET)
###---------------------------------------------------

#' @title
#' Land, Buildings, and Equipment to Assets Ratio (Net)
#'
#' @description
#' Net land, buildings, and equipment as a share of total assets.
#'
#' **Formula:**
#' ```
#' land_assets_net = land_bldg_equip_net / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param land_bldg_equip_net Net land, buildings, and equipment EOY (after accumulated depreciation).
#')
#' @param total_assets Total assets, EOY.
#')
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @details
#' ## Primary uses and key insights
#'
#' The land, buildings, and equipment to assets ratio (net version) measures what
#' share of total assets is represented by the net book value of fixed physical assets
#' after accumulated depreciation. This is the standard version used in most nonprofit
#' financial analyses, since it reflects current accounting value. High values indicate
#' capital-intensive organizations; low values indicate lean service organizations.
#' Very high values (above 0.60) may suggest illiquidity risk. Available on both 990
#' and 990EZ forms (PZ scope).
#'
#' ## Formula variations and their sources
#'
#' Net land, buildings, and equipment (Part X line 10cB) / total assets (line 16B).
#' The gross version ([get_land_assets_gross_ratio()]) uses accumulated
#' depreciation instead of net book value.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
#'     and Society*, 20(4), 94-112.
#'   - Bowman, W. (2011). Financial capacity and sustainability of ordinary
#'     nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.
#'
#'
#' ## Definitional range
#'
#' Bounded \[0, 1\] in normal conditions. Heavily skewed toward zero because many
#' nonprofits own no real property.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - Values below 0.10 are typical for lean service organizations.
#'   - Values above 0.50 indicate more than half of assets are fixed property,
#'     limiting financial flexibility.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_LAND_BLDG_NET_EOY`: 
#'     Net land, buildings, and equipment, EOY (`land_bldg_equip_net`)
#'   - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_land_assets_net_ratio( df,
#'   land_bldg_equip_net       = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   total_assets              = "F9_10_ASSET_TOT_EOY",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four columns appended:
#'   `land_assets_net`, `land_assets_net_w`,
#'   `land_assets_net_z`, `land_assets_net_p`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_land_assets_net_ratio( df = dat10k )
#' head( d[ , c( "land_assets_net", "land_assets_net_w", "land_assets_net_z", "land_assets_net_p" ) ] )
#'
#' @export
get_land_assets_net_ratio <- function( df,
                     land_bldg_equip_net       = "F9_10_ASSET_LAND_BLDG_NET_EOY",
                     total_assets              = "F9_10_ASSET_TOT_EOY",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, land_bldg_equip_net, total_assets,
                   "land_bldg_equip_net", "total_assets" )

  vars <- c( land_bldg_equip_net, total_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, land_bldg_equip_net )
  den <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  land_assets_net <- num / den

  v <- winsorize_var( land_assets_net, winsorize )
  LAND_ASSETS_NET <- data.frame(
    land_assets_net   = v$raw,
    land_assets_net_w = v$winsorized,
    land_assets_net_z = v$z,
    land_assets_net_p = v$pctile )

  if ( summarize ) {
    print( summary( LAND_ASSETS_NET ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( LAND_ASSETS_NET$land_assets_net,   na.rm = TRUE ), main = "LAND_ASSETS_NET (raw)" )
    plot( density( LAND_ASSETS_NET$land_assets_net_w, na.rm = TRUE ), main = "LAND_ASSETS_NET Winsorized" )
    plot( density( LAND_ASSETS_NET$land_assets_net_z, na.rm = TRUE ), main = "LAND_ASSETS_NET Standardized (Z)" )
    plot( density( LAND_ASSETS_NET$land_assets_net_p, na.rm = TRUE ), main = "LAND_ASSETS_NET Percentile" )
  }

  return( cbind( df, LAND_ASSETS_NET ) )
}
