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
#' **Definitional Range**
#'
#' Bounded \[0, 1\] in normal conditions. The distribution is highly right-skewed:
#' most nonprofits with no owned real estate show values near zero, while
#' capital-intensive organizations (healthcare, housing, higher education) may show
#' values above 0.50.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - Most informative for within-subsector comparisons.
#'   - High values indicate illiquidity risk: if the majority of assets are fixed
#'     property, the organization has limited ability to quickly convert assets to cash.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param land_buildings Net land, buildings, and equipment (after depreciation), EOY.
#'
#' @param total_assets Total assets, EOY.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_land_assets_gross_ratio( df,
#'   land_buildings = "F9_10_ASSET_LAND_BLDG_DEPREC",
#'   total_assets   = "F9_10_ASSET_TOT_EOY",
#'   winsorize = 0.98 ,
#'   range     = "zo",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `land_assets_gross`   - land asset ratio (raw)
#'     - `land_assets_gross_w` - winsorized version
#'     - `land_assets_gross_z` - standardized z-score (based on winsorized values)
#'     - `land_assets_gross_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The land, buildings, and equipment to assets ratio (gross version) measures what
#' share of total assets is represented by the gross book value of fixed physical
#' assets before accumulated depreciation is netted out. A high ratio indicates a
#' capital-intensive organization with a large physical footprint; a low ratio
#' indicates a lean service organization whose assets are primarily financial.
#'
#' The gross version uses the accumulated depreciation field (Part X line 10b) as a
#' proxy for the gross value of the fixed asset base. This is distinct from the net
#' version ([get_land_assets_net_ratio()]), which uses the net book value
#' after depreciation (Part X line 10cB). The gross version gives a better picture
#' of the original investment in fixed assets; the net version better reflects
#' current book value.
#'
#' ## Formula variations and their sources
#'
#' F9_10_ASSET_LAND_BLDG_DEPREC (accumulated depreciation, line 10b) / total assets.
#' Note: this field contains the accumulated depreciation amount, not the gross cost.
#' Using it as a proxy for the scale of fixed asset investment is an approximation.
#' For a cleaner measure of fixed asset intensity, [get_land_assets_net_ratio()]
#' uses the net value directly.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
#'     and Society*, 20(4), 94-112. - Asset composition analysis for nonprofits.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_LAND_BLDG_DEPREC`: 
#'     Accumulated depreciation on land and buildings (`land_buildings`)
#'   - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)
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
#' d <- get_land_assets_gross_ratio( df = dat10k )
#' head( d[ , c( "land_assets_gross", "land_assets_gross_w", "land_assets_gross_z", "land_assets_gross_p" ) ] )
#'
#' @export
get_land_assets_gross_ratio <- function( df,
                     land_buildings = "F9_10_ASSET_LAND_BLDG_DEPREC",
                     total_assets   = "F9_10_ASSET_TOT_EOY",
                     winsorize = 0.98  ,
                     range     = "zo" ,
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

  nan.count <- sum( a == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total assets equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  a[ a == 0 ] <- NaN

  lar <- l / a

  v <- apply_transformations( lar, winsorize, range )
  LAND_ASSETS_GROSS <- data.frame( land_assets_gross   = v$raw,
                     land_assets_gross_w = v$winsorized,
                     land_assets_gross_z = v$z,
                     land_assets_gross_p = v$pctile )

  if ( summarize ) {
    print( summary( LAND_ASSETS_GROSS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( LAND_ASSETS_GROSS$land_assets_gross, na.rm = TRUE ), main = "LAND_ASSETS_GROSS (raw)" )
    plot( density( LAND_ASSETS_GROSS$land_assets_gross_w, na.rm = TRUE ), main = "LAND_ASSETS_GROSS Winsorized" )
    plot( density( LAND_ASSETS_GROSS$land_assets_gross_z, na.rm = TRUE ), main = "LAND_ASSETS_GROSS Standardized (Z)" )
    plot( density( LAND_ASSETS_GROSS$land_assets_gross_p, na.rm = TRUE ), main = "LAND_ASSETS_GROSS Percentile" )
  }

  return( cbind( df, LAND_ASSETS_GROSS ) )
}
