###---------------------------------------------------
###   ADMINISTRATIVE OVERHEAD RATIO
###---------------------------------------------------

#' @title
#' Administrative Overhead Ratio
#'
#' @description
#' Share of total expenses devoted to management and general administration.
#'
#' **Formula:**
#' ```
#' aer = management_expenses / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param mgmt_expenses Management and general expenses. (On 990: Part IX, line 25C; \code{F9_09_EXP_TOT_MGMT})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_expenses_admin_ratio( df,
#'   mgmt_expenses  = "F9_09_EXP_TOT_MGMT",
#'   total_expenses = "F9_09_EXP_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{aer}   — administrative overhead ratio (raw)
#'     \item \code{aer_w} — winsorized version
#'     \item \code{aer_z} — standardized z-score (based on winsorized values)
#'     \item \code{aer_p} — percentile rank (1–100)
#'   }
#'
#' @details
#' The administrative overhead ratio measures what share of total spending goes to management
#' and general expenses rather than programs or fundraising. Lower values indicate more
#' mission-focused spending. Charity Navigator gives its highest ratings to organizations
#' spending less than 15% on overhead; the Better Business Bureau recommends below 35%.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_TOT_MGMT}: Management and general expenses (\code{mgmt_expenses})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
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
#' d <- get_expenses_admin_ratio( df = dat10k )
#' head( d[ , c( "expenses_admin", "expenses_admin_w", "expenses_admin_z", "expenses_admin_p" ) ] )
#'
#' @export
get_expenses_admin_ratio <- function( df,
                     mgmt_expenses  = "F9_09_EXP_TOT_MGMT",
                     total_expenses = "F9_09_EXP_TOT_TOT",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, mgmt_expenses, total_expenses, "mgmt_expenses", "total_expenses" )

  if ( length( mgmt_expenses )  > 2 ) stop( "`mgmt_expenses` must be one or two column names."  )
  if ( length( total_expenses ) > 2 ) stop( "`total_expenses` must be one or two column names." )

  vars <- c( mgmt_expenses, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  d <- resolve_col( dt, mgmt_expenses )
  e <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( e == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  e[ e == 0 ] <- NA

  aer <- d / e

  v <- winsorize_var( aer, winsorize )
  EXPENSES_ADMIN <- data.frame( expenses_admin   = v$raw,
                     expenses_admin_w = v$winsorized,
                     expenses_admin_z = v$z,
                     expenses_admin_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_ADMIN ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_ADMIN$expenses_admin, na.rm = TRUE ), main = "EXPENSES_ADMIN (raw)" )
    plot( density( EXPENSES_ADMIN$expenses_admin_w, na.rm = TRUE ), main = "EXPENSES_ADMIN Winsorized" )
    plot( density( EXPENSES_ADMIN$expenses_admin_z, na.rm = TRUE ), main = "EXPENSES_ADMIN Standardized (Z)" )
    plot( density( EXPENSES_ADMIN$expenses_admin_p, na.rm = TRUE ), main = "EXPENSES_ADMIN Percentile" )
  }

  return( cbind( df, EXPENSES_ADMIN ) )
}
