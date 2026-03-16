###---------------------------------------------------
###   DAYS OF OPERATING CASH AND INVESTMENTS
###---------------------------------------------------

#' @title
#' Days of Operating Cash and Investments
#'
#' @description
#' Measures days of operating coverage using liquid and investment assets, net of
#' fixed property obligations.
#'
#' **Formula:**
#' ```
#' doci = investable_assets / daily_expenses
#'
#' investable_assets = unrestricted_net_assets + investments
#'                     - ( land_buildings_equipment + mortgages_payable )
#' daily_expenses    = ( total_expenses - depreciation ) / 365
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param net_assets Unrestricted net assets, EOY. (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param investments Investments held for sale or use, EOY. (On 990: Part X, line 11c; \code{F9_10_ASSET_INV_SALE_EOY})
#' @param land_buildings Net land, buildings, and equipment, EOY. (On 990: Part X, line 10c; \code{F9_10_ASSET_LAND_BLDG_NET_EOY})
#' @param mortgages_payable Mortgages and notes payable, EOY. (On 990: Part X, line 23B; \code{F9_10_LIAB_MTG_NOTE_EOY})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param depreciation Depreciation, depletion, and amortization. (On 990: Part IX, line 22A; \code{F9_09_EXP_DEPREC_TOT})
#' @param numerator Optional. A pre-aggregated column for investable assets. Cannot be
#'   combined with the individual component arguments.
#' @param denominator Optional. A pre-aggregated column for annual non-depreciation expenses
#'   (the function divides this by 365 internally). Cannot be combined with
#'   \code{total_expenses} or \code{depreciation}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_doci( df,
#'   net_assets        = "F9_10_NAFB_UNRESTRICT_EOY",
#'   investments       = "F9_10_ASSET_INV_SALE_EOY",
#'   land_buildings    = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   mortgages_payable = "F9_10_LIAB_MTG_NOTE_EOY",
#'   total_expenses    = "F9_09_EXP_TOT_TOT",
#'   depreciation      = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{doci}   — days of operating cash and investments (raw)
#'     \item \code{doci_w} — winsorized version
#'     \item \code{doci_z} — standardized z-score (based on winsorized values)
#'     \item \code{doci_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Days of operating cash and investments extends \code{\link{get_doch}} by using a broader
#' measure of available resources. The numerator starts with unrestricted net assets and
#' investments, then subtracts land, buildings, and equipment (which are illiquid) and the
#' mortgages payable against them. This gives a better estimate of truly accessible financial
#' resources.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{net_assets})
#'   \item \code{F9_10_ASSET_INV_SALE_EOY}: Investments held for sale, EOY (\code{investments})
#'   \item \code{F9_10_ASSET_LAND_BLDG_NET_EOY}: Net land, buildings, and equipment, EOY (\code{land_buildings})
#'   \item \code{F9_10_LIAB_MTG_NOTE_EOY}: Mortgages and notes payable, EOY (\code{mortgages_payable})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#'   \item \code{F9_09_EXP_DEPREC_TOT}: Depreciation, depletion, and amortization (\code{depreciation})
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
#' d <- get_doci( df = dat10k )
#' head( d[ , c( "doci", "doci_w", "doci_z", "doci_p" ) ] )
#'
#' @export
get_doci <- function( df,
                      net_assets        = "F9_10_NAFB_UNRESTRICT_EOY",
                      investments       = "F9_10_ASSET_INV_SALE_EOY",
                      land_buildings    = "F9_10_ASSET_LAND_BLDG_NET_EOY",
                      mortgages_payable = "F9_10_LIAB_MTG_NOTE_EOY",
                      total_expenses    = "F9_09_EXP_TOT_TOT",
                      depreciation      = "F9_09_EXP_DEPREC_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( net_assets ) | !is.null( investments ) |
                         !is.null( land_buildings ) | !is.null( mortgages_payable )
  using_component_den <- !is.null( total_expenses ) | !is.null( depreciation )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual component arguments, not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR (total_expenses + depreciation), not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual component columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or (total_expenses + depreciation)." )
  }

  all_cols <- c( net_assets, investments, land_buildings, mortgages_payable,
                 total_expenses, depreciation, numerator, denominator )
  vars <- c( net_assets, investments, land_buildings, mortgages_payable, total_expenses, depreciation, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ net_assets ]] + dt[[ investments ]] -
           ( dt[[ land_buildings ]] + dt[[ mortgages_payable ]] )
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]] / 365
  } else {
    den <- ( dt[[ total_expenses ]] - dt[[ depreciation ]] ) / 365
  }

  message( paste0( "Daily operating expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  doci <- num / den

  v <- winsorize_var( doci, winsorize )
  DOCI <- data.frame( doci   = v$raw,
                      doci_w = v$winsorized,
                      doci_z = v$z,
                      doci_p = v$pctile )

  if ( summarize ) {
    print( summary( DOCI ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DOCI$doci, na.rm = TRUE ), main = "DOCI (raw)" )
    plot( density( DOCI$doci_w, na.rm = TRUE ), main = "DOCI Winsorized" )
    plot( density( DOCI$doci_z, na.rm = TRUE ), main = "DOCI Standardized (Z)" )
    plot( density( DOCI$doci_p, na.rm = TRUE ), main = "DOCI Percentile" )
  }

  return( cbind( df, DOCI ) )
}
