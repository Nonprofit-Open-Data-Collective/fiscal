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
#' get_days_cash_investments( df,
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
#'     \item \code{days_cash_inv}   — days of operating cash and investments (raw)
#'     \item \code{days_cash_inv_w} — winsorized version
#'     \item \code{days_cash_inv_z} — standardized z-score (based on winsorized values)
#'     \item \code{days_cash_inv_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' Days of cash and investments extends \code{\link{get_days_cash_operations}} by
#' incorporating investment assets into the liquidity numerator. It asks: if the
#' organization could liquidate its unrestricted assets (net of fixed property and
#' related debt), how many days of operations could it fund? This broader measure
#' captures organizations that hold significant reserves in investment portfolios
#' rather than bank accounts.
#'
#' It is most relevant for endowed organizations, foundations, and mature nonprofits
#' that hold investment portfolios. For organizations with minimal investments,
#' \code{\link{get_days_cash_operations}} and this metric will be nearly identical.
#'
#' \strong{Formula variations and their sources}
#'
#' The numerator is unrestricted net assets plus investments held for sale, minus
#' net fixed assets (land/buildings/equipment), plus mortgage notes payable. This
#' construction approximates the liquid, unrestricted resource base by starting from
#' unrestricted net assets, adding back investment assets, and removing the illiquid
#' fixed asset component (net of its associated debt). The denominator uses the same
#' daily expense base as \code{\link{get_days_cash_operations}}: (total expenses -
#' depreciation) / 365.
#'
#' This formulation follows Zietlow et al. (2007) and is related to the LUNA measure
#' (\code{\link{get_liquid_assets_months}}).
#'
#' \strong{Why this formula was chosen}
#'
#' By netting out fixed assets and their associated mortgage debt, the formula isolates
#' resources that could realistically be accessed in a financial emergency — the
#' organization cannot liquidate its building overnight, but it can liquidate investment
#' securities. This is more operationally meaningful than simply summing all assets.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Zietlow, J., Hankin, J.A. & Seidner, A. (2007). \emph{Financial Management
#'     for Nonprofit Organizations}. Wiley.
#'   \item Calabrese, T.D. (2013). Running on empty: The operating reserves of U.S.
#'     nonprofit organizations. \emph{Nonprofit Management and Leadership}, 23(3),
#'     281-302.
#' }
#'
#' \strong{Definitional range}
#'
#' Unbounded in both directions. Negative values occur when unrestricted net assets are
#' negative (accumulated deficits exceed equity) or when fixed assets net of debt
#' exceed liquid resources. Values above 365 indicate more than one year of coverage.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item Because this measure is broader than days of cash alone, benchmarks are
#'     higher: 180-365 days is considered a solid reserve position for endowed
#'     organizations.
#'   \item For operating nonprofits without significant investment portfolios, results
#'     should be interpreted alongside \code{\link{get_days_cash_operations}}.
#'   \item A large gap between this metric and \code{\link{get_days_cash_operations}}
#'     indicates that much of the organization's liquidity is tied up in investments
#'     rather than immediately accessible cash.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{net_assets})
#'   \item \code{F9_10_ASSET_INV_SALE_EOY}: Investments held for sale, EOY (\code{investments})
#'   \item \code{F9_10_ASSET_LAND_BLDG_NET_EOY}: Net land, buildings, and equipment (\code{land_buildings})
#'   \item \code{F9_10_LIAB_MTG_NOTE_EOY}: Mortgages and notes payable (\code{mortgages_payable})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#'   \item \code{F9_09_EXP_DEPREC_TOT}: Depreciation and amortization (\code{depreciation})
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
#' d <- get_days_cash_investments( df = dat10k )
#' head( d[ , c( "days_cash_inv", "days_cash_inv_w", "days_cash_inv_z", "days_cash_inv_p" ) ] )
#'
#' @export
get_days_cash_investments <- function( df,
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
  DAYS_CASH_INV <- data.frame( days_cash_inv   = v$raw,
                      days_cash_inv_w = v$winsorized,
                      days_cash_inv_z = v$z,
                      days_cash_inv_p = v$pctile )

  if ( summarize ) {
    print( summary( DAYS_CASH_INV ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DAYS_CASH_INV$days_cash_inv, na.rm = TRUE ), main = "DAYS_CASH_INV (raw)" )
    plot( density( DAYS_CASH_INV$days_cash_inv_w, na.rm = TRUE ), main = "DAYS_CASH_INV Winsorized" )
    plot( density( DAYS_CASH_INV$days_cash_inv_z, na.rm = TRUE ), main = "DAYS_CASH_INV Standardized (Z)" )
    plot( density( DAYS_CASH_INV$days_cash_inv_p, na.rm = TRUE ), main = "DAYS_CASH_INV Percentile" )
  }

  return( cbind( df, DAYS_CASH_INV ) )
}
