###---------------------------------------------------
###   OPERATING RESERVES RATIO
###---------------------------------------------------

#' @title
#' Operating Reserves Ratio
#'
#' @description
#' Liquid unrestricted net assets available to cover operating expenses, expressed
#' as a multiple of total annual expenses.
#'
#' **Formula:**
#' ```
#' orr = operating_reserves / total_expenses
#'
#' operating_reserves = unrestricted_net_assets - net_fixed_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY. (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param net_fixed_assets Net land, buildings, and equipment, EOY. (On 990: Part X, line 10C; \code{F9_10_ASSET_LAND_BLDG_NET_EOY})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_operating_reserve_ratio( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   total_expenses          = "F9_09_EXP_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{orr}   — operating reserves ratio (raw)
#'     \item \code{orr_w} — winsorized version
#'     \item \code{orr_z} — standardized z-score (based on winsorized values)
#'     \item \code{orr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The operating reserves ratio measures how many years' worth of expenses an organization
#' could sustain using only its liquid unrestricted net assets (i.e., net assets excluding
#' fixed property). Subtracting net fixed assets from unrestricted net assets isolates the
#' portion of equity that is not tied up in illiquid property. A ratio of 0.25 or higher
#' (three months of reserves) is commonly cited as a minimum benchmark.
#'
#' Cited by Keating et al. (2005) and Calabrese (2013).
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{unrestricted_net_assets})
#'   \item \code{F9_10_ASSET_LAND_BLDG_NET_EOY}: Net land, buildings, and equipment, EOY (\code{net_fixed_assets})
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
#' d <- get_operating_reserve_ratio( df = dat10k )
#' head( d[ , c( "op_reserve", "op_reserve_w", "op_reserve_z", "op_reserve_p" ) ] )
#'
#' @export
get_operating_reserve_ratio <- function( df,
                     unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                     net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
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
  if ( is.null( total_expenses ) )
    stop( "`total_expenses` cannot be NULL." )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( net_fixed_assets ) > 2 )
    stop( "`net_fixed_assets` must be one or two column names." )
  if ( length( total_expenses ) > 2 )
    stop( "`total_expenses` must be one or two column names." )

  all_cols <- c( unrestricted_net_assets, net_fixed_assets, total_expenses )
  vars <- c( unrestricted_net_assets, net_fixed_assets, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  una <- resolve_col( dt, unrestricted_net_assets )
  nfa <- resolve_col( dt, net_fixed_assets )
  exp <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( exp == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  exp[ exp == 0 ] <- NA

  orr <- ( una - nfa ) / exp

  v <- winsorize_var( orr, winsorize )
  OP_RESERVE <- data.frame( op_reserve   = v$raw,
                     op_reserve_w = v$winsorized,
                     op_reserve_z = v$z,
                     op_reserve_p = v$pctile )

  if ( summarize ) {
    print( summary( OP_RESERVE ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( OP_RESERVE$op_reserve, na.rm = TRUE ), main = "OP_RESERVE (raw)" )
    plot( density( OP_RESERVE$op_reserve_w, na.rm = TRUE ), main = "OP_RESERVE Winsorized" )
    plot( density( OP_RESERVE$op_reserve_z, na.rm = TRUE ), main = "OP_RESERVE Standardized (Z)" )
    plot( density( OP_RESERVE$op_reserve_p, na.rm = TRUE ), main = "OP_RESERVE Percentile" )
  }

  return( cbind( df, OP_RESERVE ) )
}
