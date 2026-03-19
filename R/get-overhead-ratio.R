###---------------------------------------------------
###   OVERHEAD RATIO
###---------------------------------------------------

#' @title
#' Overhead Ratio
#'
#' @description
#' Combined management and fundraising expenses as a share of total expenses.
#'
#' **Formula:**
#' ```
#' overhead = ( mgmt_expenses + fundraising_expenses ) / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param mgmt_expenses Management and general expenses.
#'   (On 990: Part IX, line 25C; \code{F9_09_EXP_TOT_MGMT})
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param fundraising_expenses Fundraising expenses.
#'   (On 990: Part IX, line 25D; \code{F9_09_EXP_TOT_FUNDR})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_overhead_ratio( df,
   mgmt_expenses             = "F9_09_EXP_TOT_MGMT",
   total_expenses            = "F9_09_EXP_TOT_TOT",
   fundraising_expenses      = "F9_09_EXP_TOT_FUNDR",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{overhead}, \code{overhead_w},
#'   \code{overhead_z}, \code{overhead_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_overhead_ratio( df = dat10k )
#' head( d[ , c( "overhead", "overhead_w", "overhead_z", "overhead_p" ) ] )
#'
#' @export
get_overhead_ratio <- function( df,
                     mgmt_expenses             = "F9_09_EXP_TOT_MGMT",
                     total_expenses            = "F9_09_EXP_TOT_TOT",
                     fundraising_expenses      = "F9_09_EXP_TOT_FUNDR",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, mgmt_expenses, total_expenses,
                   "mgmt_expenses", "total_expenses" )

  vars <- c( mgmt_expenses, total_expenses, fundraising_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, mgmt_expenses )
  den <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  overhead <- num / den

  v <- winsorize_var( overhead, winsorize )
  OVERHEAD <- data.frame(
    overhead   = v$raw,
    overhead_w = v$winsorized,
    overhead_z = v$z,
    overhead_p = v$pctile )

  if ( summarize ) {
    print( summary( OVERHEAD ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( OVERHEAD$overhead,   na.rm = TRUE ), main = "OVERHEAD (raw)" )
    plot( density( OVERHEAD$overhead_w, na.rm = TRUE ), main = "OVERHEAD Winsorized" )
    plot( density( OVERHEAD$overhead_z, na.rm = TRUE ), main = "OVERHEAD Standardized (Z)" )
    plot( density( OVERHEAD$overhead_p, na.rm = TRUE ), main = "OVERHEAD Percentile" )
  }

  return( cbind( df, OVERHEAD ) )
}
