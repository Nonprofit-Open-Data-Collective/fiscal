###---------------------------------------------------
###   MEMBER BENEFITS EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Member Benefits Expense Ratio
#'
#' @description
#' Benefits paid to or for members as a share of total expenses.
#'
#' **Formula:**
#' ```
#' expenses_membbenefits = member_benefits / total_expenses
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param member_benefits Benefits paid to or for members (total).
#'   (On 990: Part IX, line 4 Column A; \code{F9_09_EXP_BEN_PAID_MEMB_TOT} (scope: 990 + 990EZ))
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_expenses_membbenefits_ratio( df,
   member_benefits           = "F9_09_EXP_BEN_PAID_MEMB_TOT",
   total_expenses            = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{expenses_membbenefits}, \code{expenses_membbenefits_w},
#'   \code{expenses_membbenefits_z}, \code{expenses_membbenefits_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_expenses_membbenefits_ratio( df = dat10k )
#' head( d[ , c( "expenses_membbenefits", "expenses_membbenefits_w", "expenses_membbenefits_z", "expenses_membbenefits_p" ) ] )
#'
#' @export
get_expenses_membbenefits_ratio <- function( df,
                     member_benefits           = "F9_09_EXP_BEN_PAID_MEMB_TOT",
                     total_expenses            = "F9_09_EXP_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, member_benefits, total_expenses,
                   "member_benefits", "total_expenses" )

  vars <- c( member_benefits, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, member_benefits )
  den <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  expenses_membbenefits <- num / den

  v <- winsorize_var( expenses_membbenefits, winsorize )
  EXPENSES_MEMBBENEFITS <- data.frame(
    expenses_membbenefits   = v$raw,
    expenses_membbenefits_w = v$winsorized,
    expenses_membbenefits_z = v$z,
    expenses_membbenefits_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_MEMBBENEFITS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_MEMBBENEFITS$expenses_membbenefits,   na.rm = TRUE ), main = "EXPENSES_MEMBBENEFITS (raw)" )
    plot( density( EXPENSES_MEMBBENEFITS$expenses_membbenefits_w, na.rm = TRUE ), main = "EXPENSES_MEMBBENEFITS Winsorized" )
    plot( density( EXPENSES_MEMBBENEFITS$expenses_membbenefits_z, na.rm = TRUE ), main = "EXPENSES_MEMBBENEFITS Standardized (Z)" )
    plot( density( EXPENSES_MEMBBENEFITS$expenses_membbenefits_p, na.rm = TRUE ), main = "EXPENSES_MEMBBENEFITS Percentile" )
  }

  return( cbind( df, EXPENSES_MEMBBENEFITS ) )
}
