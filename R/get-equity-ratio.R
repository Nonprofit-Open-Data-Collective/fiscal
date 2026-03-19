###---------------------------------------------------
###   EQUITY RATIO
###---------------------------------------------------

#' @title
#' Equity Ratio
#'
#' @description
#' Share of total assets financed through net assets (equity).
#'
#' **Formula:**
#' ```
#' er = total_net_assets / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param net_assets Total net assets, EOY. (On 990: Part X, line 33B; \code{F9_10_NAFB_TOT_EOY})
#' @param total_assets Total assets, EOY. (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_equity_ratio( df,
#'   net_assets   = "F9_10_NAFB_TOT_EOY",
#'   total_assets = "F9_10_ASSET_TOT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{er}   — equity ratio (raw)
#'     \item \code{er_w} — winsorized version
#'     \item \code{er_z} — standardized z-score (based on winsorized values)
#'     \item \code{er_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The equity ratio measures the proportion of total assets that are financed by the
#' organization's own net assets rather than liabilities. Higher values indicate greater
#' financial stability and a lower dependence on debt financing.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_TOT_EOY}: Total net assets, EOY (\code{net_assets})
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
#' d <- get_equity_ratio( df = dat10k )
#' head( d[ , c( "equity", "equity_w", "equity_z", "equity_p" ) ] )
#'
#' @export
get_equity_ratio <- function( df,
                    net_assets   = "F9_10_NAFB_TOT_EOY",
                    total_assets = "F9_10_ASSET_TOT_EOY",
                    winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, net_assets, total_assets, "net_assets", "total_assets" )

  if ( length( net_assets )   > 2 ) stop( "`net_assets` must be one or two column names."   )
  if ( length( total_assets ) > 2 ) stop( "`total_assets` must be one or two column names." )

  vars <- c( net_assets, total_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  n <- resolve_col( dt, net_assets )
  a <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( a == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  a[ a == 0 ] <- NA

  er <- n / a

  v <- winsorize_var( er, winsorize )
  EQUITY <- data.frame( equity   = v$raw,
                    equity_w = v$winsorized,
                    equity_z = v$z,
                    equity_p = v$pctile )

  if ( summarize ) {
    print( summary( EQUITY ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EQUITY$equity, na.rm = TRUE ), main = "EQUITY (raw)" )
    plot( density( EQUITY$equity_w, na.rm = TRUE ), main = "EQUITY Winsorized" )
    plot( density( EQUITY$equity_z, na.rm = TRUE ), main = "EQUITY Standardized (Z)" )
    plot( density( EQUITY$equity_p, na.rm = TRUE ), main = "EQUITY Percentile" )
  }

  return( cbind( df, EQUITY ) )
}
