###---------------------------------------------------
###   RELATED ORGANIZATIONS REVENUE RATIO
###---------------------------------------------------

#' @title
#' Related Organizations Revenue Ratio
#'
#' @description
#' Revenue from related organizations as a share of total revenue.
#'
#' **Formula:**
#' ```
#' revenue_reltdorgs = related_org_revenue / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param related_org_revenue Contributions from related organizations.
#'
#' @param total_revenue Total revenue.
#'
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @details
#' ## Revenue Reltdorgs Ratio - Revenue composition measure
#'
#' Formula: related organization revenue / total revenue. Bounded \[0, 1\].
#'
#' Revenue from related organizations (subsidiaries, affiliates, supporting organizations) may indicate resource sharing within a nonprofit family rather than arm's-length fundraising. High values warrant review of organizational structure and transfer pricing.
#'
#' ## Canonical citations
#'
#'
#'   - Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     *VOLUNTAS*, 5(3), 273-290.
#'   - Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in nonprofit
#'     organizations. *Journal of Public Administration Research and Theory*,
#'     19(4), 947-966.
#'
#'
#' ## Variables used:
#'
#'   - `F9_08_REV_CONTR_RLTD_ORG`: Numerator (`related_org_revenue`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_reltdorgs_ratio( df,
#'   related_org_revenue       = "F9_08_REV_CONTR_RLTD_ORG",
#'   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four new columns:
#'
#'   - `revenue_reltdorgs` (raw ratio)
#'   - `revenue_reltdorgs_w` (winsorized)
#'   - `revenue_reltdorgs_z` (z-score)
#'   - `revenue_reltdorgs_p` (percentile rank, 1-100)
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_revenue_reltdorgs_ratio( df = dat10k )
#' head( d[ , c( "revenue_reltdorgs", "revenue_reltdorgs_w", "revenue_reltdorgs_z", "revenue_reltdorgs_p" ) ] )
#'
#' @export
get_revenue_reltdorgs_ratio <- function( df,
                     related_org_revenue       = "F9_08_REV_CONTR_RLTD_ORG",
                     total_revenue             = "F9_08_REV_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, related_org_revenue, total_revenue,
                   "related_org_revenue", "total_revenue" )

  vars <- c( related_org_revenue, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, related_org_revenue )
  den <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  revenue_reltdorgs <- num / den

  v <- winsorize_var( revenue_reltdorgs, winsorize )
  REVENUE_RELTDORGS <- data.frame(
    revenue_reltdorgs   = v$raw,
    revenue_reltdorgs_w = v$winsorized,
    revenue_reltdorgs_z = v$z,
    revenue_reltdorgs_p = v$pctile )

  if ( summarize ) {
    print( summary( REVENUE_RELTDORGS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( REVENUE_RELTDORGS$revenue_reltdorgs,   na.rm = TRUE ), main = "REVENUE_RELTDORGS (raw)" )
    plot( density( REVENUE_RELTDORGS$revenue_reltdorgs_w, na.rm = TRUE ), main = "REVENUE_RELTDORGS Winsorized" )
    plot( density( REVENUE_RELTDORGS$revenue_reltdorgs_z, na.rm = TRUE ), main = "REVENUE_RELTDORGS Standardized (Z)" )
    plot( density( REVENUE_RELTDORGS$revenue_reltdorgs_p, na.rm = TRUE ), main = "REVENUE_RELTDORGS Percentile" )
  }

  return( cbind( df, REVENUE_RELTDORGS ) )
}
