###---------------------------------------------------
###   POST-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Post-Depreciation Profitability Margin
#'
#' @description
#' Operating surplus or deficit as a share of total revenue, after depreciation.
#'
#' **Formula:**
#' ```
#' podpm = ( total_revenue - total_expenses ) / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_profit_margin_postdepr( df,
#'   expenses  = "F9_09_EXP_TOT_TOT",
#'   revenue   = "F9_08_REV_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{podpm}   — post-depreciation profitability margin (raw)
#'     \item \code{podpm_w} — winsorized version
#'     \item \code{podpm_z} — standardized z-score (based on winsorized values)
#'     \item \code{podpm_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The post-depreciation profitability margin measures the net surplus (or deficit) as a
#' proportion of total revenue after all expenses including non-cash depreciation charges
#' have been accounted for. Positive values indicate a surplus; negative values a deficit.
#' Compare with \code{\link{get_predpm}} which excludes depreciation.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{expenses})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{revenue})
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
#' d <- get_profit_margin_postdepr( df = dat10k )
#' head( d[ , c( "profit_postdepr", "profit_postdepr_w", "profit_postdepr_z", "profit_postdepr_p" ) ] )
#'
#' @export
get_profit_margin_postdepr <- function( df,
                       expenses  = "F9_09_EXP_TOT_TOT",
                       revenue   = "F9_08_REV_TOT_TOT",
                       winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, expenses, revenue, "expenses", "revenue" )

  if ( length( expenses ) > 2 ) stop( "`expenses` must be one or two column names." )
  if ( length( revenue )  > 2 ) stop( "`revenue` must be one or two column names."  )

  vars <- c( expenses, revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  e <- resolve_col( dt, expenses )
  r <- resolve_col( dt, revenue )

  message( paste0( "Revenue equal to zero: ", sum( r == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  r[ r == 0 ] <- NA

  podpm <- ( r - e ) / r

  v <- winsorize_var( podpm, winsorize )
  PROFIT_POSTDEPR <- data.frame( profit_postdepr   = v$raw,
                       profit_postdepr_w = v$winsorized,
                       profit_postdepr_z = v$z,
                       profit_postdepr_p = v$pctile )

  if ( summarize ) {
    print( summary( PROFIT_POSTDEPR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( PROFIT_POSTDEPR$profit_postdepr, na.rm = TRUE ), main = "PROFIT_POSTDEPR (raw)" )
    plot( density( PROFIT_POSTDEPR$profit_postdepr_w, na.rm = TRUE ), main = "PROFIT_POSTDEPR Winsorized" )
    plot( density( PROFIT_POSTDEPR$profit_postdepr_z, na.rm = TRUE ), main = "PROFIT_POSTDEPR Standardized (Z)" )
    plot( density( PROFIT_POSTDEPR$profit_postdepr_p, na.rm = TRUE ), main = "PROFIT_POSTDEPR Percentile" )
  }

  return( cbind( df, PROFIT_POSTDEPR ) )
}
