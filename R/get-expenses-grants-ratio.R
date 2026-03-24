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
#' **Definitional Range**
#'
#' Bounded \[0, 1\]. Near zero for direct-service organizations; near 1.0 for
#' pure pass-through grant-makers.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - Subsector comparisons are essential: a 0.80 ratio is expected for a community
#'     foundation; the same value would be anomalous for a hospital.
#'   - Rising ratios over time may indicate a strategic shift toward a more
#'     redistributive model.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param us_org_grants Grants to domestic organizations and governments (total).
#'
#' @param us_indiv_grants Grants and assistance to domestic individuals (total).
#'
#' @param foreign_grants Grants to foreign organizations and governments (total).
#'
#' @param total_expenses Total functional expenses.
#'
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#' @details
#' ## Primary uses and key insights
#'
#' The grants-to-others ratio measures the share of total expenses paid out as grants
#' to other organizations, individuals, or foreign entities. It is the defining
#' characteristic of grant-making and pass-through organizations - foundations,
#' federated funders, and community chests - where the primary activity is resource
#' redistribution rather than direct service delivery.
#'
#' ## Formula
#'
#' (Domestic org grants + domestic individual grants + foreign grants) / total expenses.
#' Note: US organization grants (line 1) are PZ scope; other components are PC scope.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
#'     and Society*, 20(4), 94-112.
#'
#'
#' ## Variables used:
#'
#'   - `F9_09_EXP_GRANT_US_ORG_TOT`: 
#'     Grants to domestic organizations (`us_org_grants`)
#'   - `F9_09_EXP_GRANT_US_INDIV_TOT`: 
#'     Grants to domestic individuals (`us_indiv_grants`)
#'   - `F9_09_EXP_GRANT_FRGN_TOT`: Grants to foreign entities (`foreign_grants`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'
#'
#' @param sanitize Logical (default `TRUE`).
#' @param summarize Logical (default `FALSE`).
#'
#' @usage
#' get_expenses_grants_ratio( df,
#'   us_org_grants    = "F9_09_EXP_GRANT_US_ORG_TOT",
#'   us_indiv_grants  = "F9_09_EXP_GRANT_US_INDIV_TOT",
#'   foreign_grants   = "F9_09_EXP_GRANT_FRGN_TOT",
#'   total_expenses   = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four columns appended:
#'   `expenses_grants`, `expenses_grants_w`,
#'   `expenses_grants_z`, `expenses_grants_p`.
#'
#' @details
#' **Variables used:**
#'
#'   - `F9_09_EXP_GRANT_US_ORG_TOT`: 
#'     Grants to domestic orgs (`us_org_grants`)
#'   - `F9_09_EXP_GRANT_US_INDIV_TOT`: 
#'     Grants to domestic individuals (`us_indiv_grants`)
#'   - `F9_09_EXP_GRANT_FRGN_TOT`: Grants to foreign entities (`foreign_grants`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'
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
                     winsorize  = 0.98 ,
                     range     = "zo" ,
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

  nan.count <- sum( te == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total expenses equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  te[ te == 0 ] <- NaN

  expenses_grants <- ( g1 + g2 + g3 ) / te

  v <- apply_transformations( expenses_grants, winsorize, range )
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
