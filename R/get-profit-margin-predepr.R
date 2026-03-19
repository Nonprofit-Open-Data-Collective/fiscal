###---------------------------------------------------
###   PRE-DEPRECIATION PROFITABILITY MARGIN
###---------------------------------------------------

#' @title
#' Pre-Depreciation Profitability Margin
#'
#' @description
#' Operating surplus or deficit as a share of total revenue, before depreciation.
#'
#' **Formula:**
#' ```
#' predpm = ( total_revenue - ( total_expenses - depreciation ) ) / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param depreciation Depreciation, depletion, and amortization. (On 990: Part IX, line 22A; \code{F9_09_EXP_DEPREC_TOT})
#' @param numerator Optional. A pre-calculated column for the numerator
#'   (revenue minus non-depreciation expenses). Cannot be combined with
#'   \code{revenue}, \code{expenses}, or \code{depreciation}.
#' @param denominator Optional. A pre-calculated column for the denominator. Cannot be
#'   combined with \code{revenue}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_profit_margin_predepr( df,
#'   revenue      = "F9_08_REV_TOT_TOT",
#'   expenses     = "F9_09_EXP_TOT_TOT",
#'   depreciation = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{predpm}   — pre-depreciation profitability margin (raw)
#'     \item \code{predpm_w} — winsorized version
#'     \item \code{predpm_z} — standardized z-score (based on winsorized values)
#'     \item \code{predpm_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The pre-depreciation profitability margin adds back depreciation to measure the
#' cash-based surplus as a share of revenue. Because depreciation is a non-cash expense,
#' this metric more closely approximates actual cash flow than \code{\link{get_podpm}}.
#' Higher values relative to the post-depreciation margin reflect the impact of large
#' non-cash depreciation charges.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{revenue})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{expenses})
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
#' d <- get_profit_margin_predepr( df = dat10k )
#' head( d[ , c( "profit_predepr", "profit_predepr_w", "profit_predepr_z", "profit_predepr_p" ) ] )
#'
#' @export
get_profit_margin_predepr <- function( df,
                        revenue      = "F9_08_REV_TOT_TOT",
                        expenses     = "F9_09_EXP_TOT_TOT",
                        depreciation = "F9_09_EXP_DEPREC_TOT",
                        numerator    = NULL,
                        denominator  = NULL,
                        winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( !is.null( numerator ) & ( !is.null( revenue ) | !is.null( expenses ) | !is.null( depreciation ) ) ) {
    stop( "Supply either `numerator` OR the individual component arguments (revenue, expenses, depreciation), not both." )
  }
  if ( !is.null( denominator ) & !is.null( revenue ) ) {
    stop( "Supply either `denominator` OR `revenue`, not both." )
  }

  all_cols <- c( revenue, expenses, depreciation, numerator, denominator )
  vars <- c( revenue, expenses, depreciation, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ revenue ]] - ( dt[[ expenses ]] - dt[[ depreciation ]] )
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ revenue ]]
  }

  message( paste0( "Revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  predpm <- num / den

  v <- winsorize_var( predpm, winsorize )
  PROFIT_PREDEPR <- data.frame( profit_predepr   = v$raw,
                        profit_predepr_w = v$winsorized,
                        profit_predepr_z = v$z,
                        profit_predepr_p = v$pctile )

  if ( summarize ) {
    print( summary( PROFIT_PREDEPR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( PROFIT_PREDEPR$profit_predepr, na.rm = TRUE ), main = "PROFIT_PREDEPR (raw)" )
    plot( density( PROFIT_PREDEPR$profit_predepr_w, na.rm = TRUE ), main = "PROFIT_PREDEPR Winsorized" )
    plot( density( PROFIT_PREDEPR$profit_predepr_z, na.rm = TRUE ), main = "PROFIT_PREDEPR Standardized (Z)" )
    plot( density( PROFIT_PREDEPR$profit_predepr_p, na.rm = TRUE ), main = "PROFIT_PREDEPR Percentile" )
  }

  return( cbind( df, PROFIT_PREDEPR ) )
}
