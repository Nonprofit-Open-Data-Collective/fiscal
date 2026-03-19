###---------------------------------------------------
###   MONTHS OF LIQUID UNRESTRICTED NET ASSETS (LUNA)
###---------------------------------------------------

#' @title
#' Months of Liquid Unrestricted Net Assets (LUNA)
#'
#' @description
#' Number of months an organization can operate using its liquid unrestricted net assets.
#'
#' **Formula:**
#' ```
#' luna = liquid_unrestricted_net_assets / monthly_expenses
#'
#' liquid_unrestricted_net_assets = unrestricted_net_assets
#'                                  - ( net_fixed_assets - mortgages_payable )
#' monthly_expenses = total_expenses / 12
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY. (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param net_fixed_assets Net land, buildings, and equipment, EOY. (On 990: Part X, line 10C; \code{F9_10_ASSET_LAND_BLDG_NET_EOY})
#' @param mortgages_payable Mortgages and notes payable, EOY. (On 990: Part X, line 23B; \code{F9_10_LIAB_MTG_NOTE_EOY})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_liquid_assets_months( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   mortgages_payable       = "F9_10_LIAB_MTG_NOTE_EOY",
#'   total_expenses          = "F9_09_EXP_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{luna}   — months of liquid unrestricted net assets (raw)
#'     \item \code{luna_w} — winsorized version
#'     \item \code{luna_z} — standardized z-score (based on winsorized values)
#'     \item \code{luna_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' LUNA measures how many months an organization could sustain operations using its
#' liquid unrestricted equity. The numerator adjusts unrestricted net assets to remove
#' the illiquid portion tied up in property: net fixed assets are subtracted, but
#' the mortgages payable against them are added back (since that debt is already
#' reflected in the net asset balance).
#'
#' Cited by Calabrese (2013) and Zietlow (2012). Related to \code{\link{get_orr}},
#' which expresses the same concept as a ratio to annual (rather than monthly) expenses.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{unrestricted_net_assets})
#'   \item \code{F9_10_ASSET_LAND_BLDG_NET_EOY}: Net land, buildings, and equipment, EOY (\code{net_fixed_assets})
#'   \item \code{F9_10_LIAB_MTG_NOTE_EOY}: Mortgages and notes payable, EOY (\code{mortgages_payable})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
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
#' d <- get_liquid_assets_months( df = dat10k )
#' head( d[ , c( "liquid_assets_months", "liquid_assets_months_w", "liquid_assets_months_z", "liquid_assets_months_p" ) ] )
#'
#' @export
get_liquid_assets_months <- function( df,
                      unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                      net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
                      mortgages_payable       = "F9_10_LIAB_MTG_NOTE_EOY",
                      total_expenses          = "F9_09_EXP_TOT_TOT",
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( is.null( unrestricted_net_assets ) )
    stop( "`unrestricted_net_assets` cannot be NULL." )
  if ( is.null( net_fixed_assets ) )
    stop( "`net_fixed_assets` cannot be NULL." )
  if ( is.null( mortgages_payable ) )
    stop( "`mortgages_payable` cannot be NULL." )
  if ( is.null( total_expenses ) )
    stop( "`total_expenses` cannot be NULL." )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( net_fixed_assets ) > 2 )
    stop( "`net_fixed_assets` must be one or two column names." )
  if ( length( mortgages_payable ) > 2 )
    stop( "`mortgages_payable` must be one or two column names." )
  if ( length( total_expenses ) > 2 )
    stop( "`total_expenses` must be one or two column names." )

  all_cols <- c( unrestricted_net_assets, net_fixed_assets,
                 mortgages_payable, total_expenses )
  vars <- c( unrestricted_net_assets, net_fixed_assets, mortgages_payable, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  una <- resolve_col( dt, unrestricted_net_assets )
  nfa <- resolve_col( dt, net_fixed_assets )
  mnp <- resolve_col( dt, mortgages_payable )
  exp <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( exp == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  exp[ exp == 0 ] <- NA

  luna_val <- ( una - ( nfa - mnp ) ) / ( exp / 12 )

  v <- winsorize_var( luna_val, winsorize )
  LIQUID_ASSETS_MONTHS <- data.frame( liquid_assets_months   = v$raw,
                      liquid_assets_months_w = v$winsorized,
                      liquid_assets_months_z = v$z,
                      liquid_assets_months_p = v$pctile )

  if ( summarize ) {
    print( summary( LIQUID_ASSETS_MONTHS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS (raw)" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_w, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Winsorized" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_z, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Standardized (Z)" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_p, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Percentile" )
  }

  return( cbind( df, LIQUID_ASSETS_MONTHS ) )
}
