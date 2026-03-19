###---------------------------------------------------
###   CASH AND SAVINGS TO ASSETS RATIO
###---------------------------------------------------

#' @title
#' Cash and Savings to Assets Ratio
#'
#' @description
#' Cash and savings as a share of total assets; measures the liquid composition of the asset base.
#'
#' **Formula:**
#' ```
#' cash_assets = ( cash + savings ) / total_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#'   (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param total_assets Total assets, EOY.
#'   (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY})
#' @param savings Savings and temporary cash investments, EOY.
#'   (On 990: Part X, line 2B; \code{F9_10_ASSET_SAVING_EOY})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_cash_assets_ratio( df,
   cash                      = "F9_10_ASSET_CASH_EOY",
   total_assets              = "F9_10_ASSET_TOT_EOY",
   savings                   = "F9_10_ASSET_SAVING_EOY",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{cash_assets}, \code{cash_assets_w},
#'   \code{cash_assets_z}, \code{cash_assets_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_cash_assets_ratio( df = dat10k )
#' head( d[ , c( "cash_assets", "cash_assets_w", "cash_assets_z", "cash_assets_p" ) ] )
#'
#' @export
get_cash_assets_ratio <- function( df,
                     cash                      = "F9_10_ASSET_CASH_EOY",
                     total_assets              = "F9_10_ASSET_TOT_EOY",
                     savings                   = "F9_10_ASSET_SAVING_EOY",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, cash, total_assets,
                   "cash", "total_assets" )

  vars <- c( cash, total_assets, savings )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, cash )
  den <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  cash_assets <- num / den

  v <- winsorize_var( cash_assets, winsorize )
  CASH_ASSETS <- data.frame(
    cash_assets   = v$raw,
    cash_assets_w = v$winsorized,
    cash_assets_z = v$z,
    cash_assets_p = v$pctile )

  if ( summarize ) {
    print( summary( CASH_ASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( CASH_ASSETS$cash_assets,   na.rm = TRUE ), main = "CASH_ASSETS (raw)" )
    plot( density( CASH_ASSETS$cash_assets_w, na.rm = TRUE ), main = "CASH_ASSETS Winsorized" )
    plot( density( CASH_ASSETS$cash_assets_z, na.rm = TRUE ), main = "CASH_ASSETS Standardized (Z)" )
    plot( density( CASH_ASSETS$cash_assets_p, na.rm = TRUE ), main = "CASH_ASSETS Percentile" )
  }

  return( cbind( df, CASH_ASSETS ) )
}
