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
#' get_podpm( df,
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
#' d <- get_podpm( df = dat10k )
#' head( d[ , c( "podpm", "podpm_w", "podpm_z", "podpm_p" ) ] )
#'
#' @export
get_podpm <- function( df,
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
  PODPM <- data.frame( podpm   = v$raw,
                       podpm_w = v$winsorized,
                       podpm_z = v$z,
                       podpm_p = v$pctile )

  if ( summarize ) {
    print( summary( PODPM ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( PODPM$podpm, na.rm = TRUE ), main = "PODPM (raw)" )
    plot( density( PODPM$podpm_w, na.rm = TRUE ), main = "PODPM Winsorized" )
    plot( density( PODPM$podpm_z, na.rm = TRUE ), main = "PODPM Standardized (Z)" )
    plot( density( PODPM$podpm_p, na.rm = TRUE ), main = "PODPM Percentile" )
  }

  return( cbind( df, PODPM ) )
}
