###---------------------------------------------------
###   NET ASSETS COMPOSITION RATIO
###---------------------------------------------------

#' @title
#' Net Assets Composition Ratio
#'
#' @description
#' Share of total net assets that are unrestricted and available for general use.
#'
#' **Formula:**
#' ```
#' nacr = unrestricted_net_assets / total_net_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY. (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param total_net_assets Total net assets, EOY. (On 990: Part X, line 33B; \code{F9_10_NAFB_TOT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_nacr( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   total_net_assets        = "F9_10_NAFB_TOT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{nacr}   — net assets composition ratio (raw)
#'     \item \code{nacr_w} — winsorized version
#'     \item \code{nacr_z} — standardized z-score (based on winsorized values)
#'     \item \code{nacr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The net assets composition ratio measures the proportion of an organization's
#' total net assets that are unrestricted — i.e., available at management's
#' discretion rather than designated for a specific donor purpose. Higher values
#' indicate greater financial flexibility.
#'
#' Organizations with large permanently or temporarily restricted endowments may
#' show low ratios even while holding substantial total net assets. The ratio helps
#' distinguish between nominal and operational financial strength.
#'
#' Cited by the Urban Institute and IRS as a transparency and governance indicator.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{unrestricted_net_assets})
#'   \item \code{F9_10_NAFB_TOT_EOY}: Total net assets, EOY (\code{total_net_assets})
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
#' d <- get_nacr( df = dat10k )
#' head( d[ , c( "nacr", "nacr_w", "nacr_z", "nacr_p" ) ] )
#'
#' @export
get_nacr <- function( df,
                      unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                      total_net_assets        = "F9_10_NAFB_TOT_EOY",
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, unrestricted_net_assets, total_net_assets,
                   "unrestricted_net_assets", "total_net_assets" )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( total_net_assets ) > 2 )
    stop( "`total_net_assets` must be one or two column names." )

  vars <- c( unrestricted_net_assets, total_net_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  u <- resolve_col( dt, unrestricted_net_assets )
  t <- resolve_col( dt, total_net_assets )

  message( paste0( "Total net assets equal to zero: ", sum( t == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  t[ t == 0 ] <- NA

  nacr <- u / t

  v <- winsorize_var( nacr, winsorize )
  NACR <- data.frame( nacr   = v$raw,
                      nacr_w = v$winsorized,
                      nacr_z = v$z,
                      nacr_p = v$pctile )

  if ( summarize ) {
    print( summary( NACR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( NACR$nacr, na.rm = TRUE ), main = "NACR (raw)" )
    plot( density( NACR$nacr_w, na.rm = TRUE ), main = "NACR Winsorized" )
    plot( density( NACR$nacr_z, na.rm = TRUE ), main = "NACR Standardized (Z)" )
    plot( density( NACR$nacr_p, na.rm = TRUE ), main = "NACR Percentile" )
  }

  return( cbind( df, NACR ) )
}
