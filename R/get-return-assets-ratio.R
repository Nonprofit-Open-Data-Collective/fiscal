###---------------------------------------------------
###   RETURN ON ASSETS (ROA)
###---------------------------------------------------

#' @title
#' Return on Assets
#'
#' @description
#' Net surplus or deficit as a share of total assets.
#'
#' **Formula:**
#' ```
#' roa = revenues_less_expenses / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param revenues_less_expenses Revenues less expenses (net surplus or deficit) for the
#'   current year. Accepts one or two column names; if two are provided they are coalesced
#'   with the 990 value taking priority over 990EZ.
#'   (On 990: Part I, line 19 CY; \code{F9_01_EXP_REV_LESS_EXP_CY};
#'   If \code{F9_01_EXP_REV_LESS_EXP_CY} is unavailable, it is computed as
#'   \code{F9_01_REV_TOT_CY} minus \code{F9_01_EXP_TOT_CY}.)
#' @param total_assets Total assets, EOY. (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY};
#'   On EZ: Part II, line 25B; \code{F9_01_NAFB_ASSET_TOT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_return_assets_ratio( df,
#'   revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
#'   total_assets           = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{roa}   — return on assets (raw)
#'     \item \code{roa_w} — winsorized version
#'     \item \code{roa_z} — standardized z-score (based on winsorized values)
#'     \item \code{roa_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Return on assets measures how effectively an organization uses its asset base to
#' generate a surplus. In the nonprofit context, the numerator is revenues less expenses
#' (the Part I summary net income line) rather than profit in the commercial sense.
#' Positive values indicate that the organization generated a surplus relative to its
#' assets; negative values indicate a deficit.
#'
#' The primary numerator field \code{F9_01_EXP_REV_LESS_EXP_CY} is available on both
#' the full 990 (Part I, line 19) and 990EZ (Part I, line 18) forms (scope: PZ). The
#' secondary field \code{F9_11_RECO_REV_LESS_EXP} (Part XI, line 3) is a 990-only
#' fallback. Both total assets fields are available to PZ filers.
#'
#' Cited by Greenlee & Trussel (2000) and Keating et al. (2005).
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_01_EXP_REV_LESS_EXP_CY}: Revenues less expenses, current year (\code{revenues_less_expenses}, 990 + 990EZ)
#'   \item \code{F9_10_ASSET_TOT_EOY}: Total assets, EOY (\code{total_assets}, 990)
#'   \item \code{F9_01_NAFB_ASSET_TOT_EOY}: Total assets from Part I (\code{total_assets}, 990EZ)
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
#' d <- get_return_assets_ratio( df = dat10k )
#' head( d[ , c( "return_assets", "return_assets_w", "return_assets_z", "return_assets_p" ) ] )
#'
#' @export
get_return_assets_ratio <- function( df,
                     revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
                     total_assets           = c( "F9_10_ASSET_TOT_EOY",
                                                 "F9_01_NAFB_ASSET_TOT_EOY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, revenues_less_expenses, total_assets,
                   "revenues_less_expenses", "total_assets" )

  if ( length( revenues_less_expenses ) > 2 )
    stop( "`revenues_less_expenses` must be one or two column names." )
  if ( length( total_assets ) > 2 )
    stop( "`total_assets` must be one or two column names." )

  all_cols <- c( revenues_less_expenses, total_assets )
  vars <- c( revenues_less_expenses, total_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  s <- resolve_col( dt, revenues_less_expenses )
  a <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( a == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  a[ a == 0 ] <- NA

  roa <- s / a

  v <- winsorize_var( roa, winsorize )
  RETURN_ASSETS <- data.frame( return_assets   = v$raw,
                     return_assets_w = v$winsorized,
                     return_assets_z = v$z,
                     return_assets_p = v$pctile )

  if ( summarize ) {
    print( summary( RETURN_ASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( RETURN_ASSETS$return_assets, na.rm = TRUE ), main = "RETURN_ASSETS (raw)" )
    plot( density( RETURN_ASSETS$return_assets_w, na.rm = TRUE ), main = "RETURN_ASSETS Winsorized" )
    plot( density( RETURN_ASSETS$return_assets_z, na.rm = TRUE ), main = "RETURN_ASSETS Standardized (Z)" )
    plot( density( RETURN_ASSETS$return_assets_p, na.rm = TRUE ), main = "RETURN_ASSETS Percentile" )
  }

  return( cbind( df, RETURN_ASSETS ) )
}
