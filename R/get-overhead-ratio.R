###---------------------------------------------------
###   OVERHEAD RATIO
###---------------------------------------------------

#' @title Overhead Ratio
#'
#' @description
#' Combined management and fundraising expenses as a share of total expenses.
#'
#' **Formula**
#'
#' `overhead = (mgmt_expenses + fundraising_expenses) / total_expenses`
#'
#' **Calculated for:** 990 filers only.
#'
#' @param df A data.frame containing the fields required for computing the metric.
#' @param mgmt_expenses Management and general expenses
#'   (Form 990, Part IX, line 25C; `F9_09_EXP_TOT_MGMT`).
#' @param total_expenses Total functional expenses
#'   (Form 990, Part IX, line 25A; `F9_09_EXP_TOT_TOT`).
#' @param fundraising_expenses Fundraising expenses
#'   (Form 990, Part IX, line 25D; `F9_09_EXP_TOT_FUNDR`).
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for missing
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @details
#' ## Primary uses and key insights
#'
#' The overhead ratio measures the combined share of total expenses devoted to
#' management and fundraising. It is a standard definition used in the nonprofit
#' accountability literature and by charity watchdog ratings. The overhead ratio
#' complements [get_program_expenses_ratio()]: the two sum to 1.0. However, its
#' use as a primary quality indicator has been widely criticized.
#'
#' ## Formula variations and sources
#'
#' The standard version is:
#'
#' `management and general + fundraising) / total expenses`
#'
#' using Form 990 Part IX, lines 25C + 25D divided by line 25A.
#'
#' The administrative-only version, [get_expenses_admin_ratio()], excludes
#' fundraising.
#'
#' ## Canonical citations
#'
#' - Charity Navigator, *Financial Health Methodology*. Uses overhead ratio as a
#'   primary rating factor.
#' - Lecy, J.D. & Searing, E.A. (2015). Anatomy of the nonprofit starvation cycle.
#'   *Nonprofit and Voluntary Sector Quarterly*, 44(3), 539-563.
#' - Hager, M.A. & Flack, T. (2004). The pros and cons of financial efficiency
#'   standards. *Nonprofit Overhead Cost Project Brief 5*. Urban Institute.
#'
#' ## Definitional range
#'
#' Bounded between 0 and 1. Empirical range is approximately 0.05 to 0.50 for
#' most operating nonprofits.
#'
#' ## Benchmarks and rules of thumb
#'
#' - `<= 25%`: Charity Navigator threshold for a full efficiency score.
#' - `<= 35%`: BBB Wise Giving Alliance standard.
#' - Very low overhead can indicate underinvestment in organizational capacity
#'   rather than genuine efficiency.
#'
#' ## Variables used
#'
#' - `F9_09_EXP_TOT_MGMT`: Management and general expenses (`mgmt_expenses`)
#' - `F9_09_EXP_TOT_FUNDR`: Fundraising expenses (`fundraising_expenses`)
#' - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_overhead_ratio( df,
#'   mgmt_expenses             = "F9_09_EXP_TOT_MGMT",
#'   total_expenses            = "F9_09_EXP_TOT_TOT",
#'   fundraising_expenses      = "F9_09_EXP_TOT_FUNDR",
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
