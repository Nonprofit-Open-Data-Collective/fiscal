###---------------------------------------------------
###   GRANTS-TO-OTHERS EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Grants-to-Others Expense Ratio
#'
#' @description
#' Total grants paid to domestic organizations, domestic individuals, and foreign
#' entities as a share of total functional expenses.
#'
#' **Formula:**
#' ```
#' expenses_grants = ( us_org_grants + us_indiv_grants + foreign_grants )
#'                   / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param us_org_grants Grants to domestic organizations and governments (total).
#'   (On 990: Part IX, line 1 Column A; \code{F9_09_EXP_GRANT_US_ORG_TOT})
#' @param us_indiv_grants Grants and assistance to domestic individuals (total).
#'   (On 990: Part IX, line 2 Column A; \code{F9_09_EXP_GRANT_US_INDIV_TOT})
#' @param foreign_grants Grants to foreign organizations and governments (total).
#'   (On 990: Part IX, line 3 Column A; \code{F9_09_EXP_GRANT_FRGN_TOT})
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}).
#' @param summarize Logical (default \code{FALSE}).
#'
#' @usage
#' get_expenses_grants_ratio( df,
#'   us_org_grants    = "F9_09_EXP_GRANT_US_ORG_TOT",
#'   us_indiv_grants  = "F9_09_EXP_GRANT_US_INDIV_TOT",
#'   foreign_grants   = "F9_09_EXP_GRANT_FRGN_TOT",
#'   total_expenses   = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{expenses_grants}, \code{expenses_grants_w},
#'   \code{expenses_grants_z}, \code{expenses_grants_p}.
#'
#' @details
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_GRANT_US_ORG_TOT}: Grants to domestic orgs (\code{us_org_grants})
#'   \item \code{F9_09_EXP_GRANT_US_INDIV_TOT}: Grants to domestic individuals (\code{us_indiv_grants})
#'   \item \code{F9_09_EXP_GRANT_FRGN_TOT}: Grants to foreign entities (\code{foreign_grants})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#' }
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_expenses_grants_ratio( df = dat10k )
#' head( d[ , c( "expenses_grants", "expenses_grants_w", "expenses_grants_z", "expenses_grants_p" ) ] )
#'
#' @export
get_expenses_grants_ratio <- function( df,
                     us_org_grants   = "F9_09_EXP_GRANT_US_ORG_TOT",
                     us_indiv_grants = "F9_09_EXP_GRANT_US_INDIV_TOT",
                     foreign_grants  = "F9_09_EXP_GRANT_FRGN_TOT",
                     total_expenses  = "F9_09_EXP_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )
  for ( arg in c("us_org_grants","us_indiv_grants","foreign_grants","total_expenses") ) {
    if ( is.null( get(arg) ) ) stop( paste0("`", arg, "` cannot be NULL.") )
  }

  vars <- c( us_org_grants, us_indiv_grants, foreign_grants, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  g1 <- resolve_col( dt, us_org_grants )
  g2 <- resolve_col( dt, us_indiv_grants )
  g3 <- resolve_col( dt, foreign_grants )
  te <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( te == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  te[ te == 0 ] <- NA

  expenses_grants <- ( g1 + g2 + g3 ) / te

  v <- winsorize_var( expenses_grants, winsorize )
  EXPENSES_GRANTS <- data.frame(
    expenses_grants   = v$raw,
    expenses_grants_w = v$winsorized,
    expenses_grants_z = v$z,
    expenses_grants_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_GRANTS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_GRANTS$expenses_grants,   na.rm = TRUE ), main = "EXPENSES_GRANTS (raw)" )
    plot( density( EXPENSES_GRANTS$expenses_grants_w, na.rm = TRUE ), main = "EXPENSES_GRANTS Winsorized" )
    plot( density( EXPENSES_GRANTS$expenses_grants_z, na.rm = TRUE ), main = "EXPENSES_GRANTS Standardized (Z)" )
    plot( density( EXPENSES_GRANTS$expenses_grants_p, na.rm = TRUE ), main = "EXPENSES_GRANTS Percentile" )
  }

  return( cbind( df, EXPENSES_GRANTS ) )
}
