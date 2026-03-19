###---------------------------------------------------
###   PAYMENTS TO AFFILIATES RATIO
###---------------------------------------------------

#' @title
#' Payments to Affiliates Ratio
#'
#' @description
#' Payments to affiliates as a share of total expenses.
#'
#' **Formula:**
#' ```
#' expenses_affiliates = payments_to_affiliates / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param payments_to_affiliates Payments to affiliates (total).
#'   (On 990: Part IX, line 21 Column A; \code{F9_09_EXP_PAY_AFFIL_TOT})
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_expenses_affiliates_ratio( df,
   payments_to_affiliates    = "F9_09_EXP_PAY_AFFIL_TOT",
   total_expenses            = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{expenses_affiliates}, \code{expenses_affiliates_w},
#'   \code{expenses_affiliates_z}, \code{expenses_affiliates_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_expenses_affiliates_ratio( df = dat10k )
#' head( d[ , c( "expenses_affiliates", "expenses_affiliates_w", "expenses_affiliates_z", "expenses_affiliates_p" ) ] )
#'
#' @export
get_expenses_affiliates_ratio <- function( df,
                     payments_to_affiliates    = "F9_09_EXP_PAY_AFFIL_TOT",
                     total_expenses            = "F9_09_EXP_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, payments_to_affiliates, total_expenses,
                   "payments_to_affiliates", "total_expenses" )

  vars <- c( payments_to_affiliates, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, payments_to_affiliates )
  den <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  expenses_affiliates <- num / den

  v <- winsorize_var( expenses_affiliates, winsorize )
  EXPENSES_AFFILIATES <- data.frame(
    expenses_affiliates   = v$raw,
    expenses_affiliates_w = v$winsorized,
    expenses_affiliates_z = v$z,
    expenses_affiliates_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_AFFILIATES ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_AFFILIATES$expenses_affiliates,   na.rm = TRUE ), main = "EXPENSES_AFFILIATES (raw)" )
    plot( density( EXPENSES_AFFILIATES$expenses_affiliates_w, na.rm = TRUE ), main = "EXPENSES_AFFILIATES Winsorized" )
    plot( density( EXPENSES_AFFILIATES$expenses_affiliates_z, na.rm = TRUE ), main = "EXPENSES_AFFILIATES Standardized (Z)" )
    plot( density( EXPENSES_AFFILIATES$expenses_affiliates_p, na.rm = TRUE ), main = "EXPENSES_AFFILIATES Percentile" )
  }

  return( cbind( df, EXPENSES_AFFILIATES ) )
}
