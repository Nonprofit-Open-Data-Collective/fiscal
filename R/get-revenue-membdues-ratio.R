###---------------------------------------------------
###   MEMBERSHIP DUES REVENUE RATIO
###---------------------------------------------------

#' @title
#' Membership Dues Revenue Ratio
#'
#' @description
#' Membership dues as a share of total revenue.
#'
#' **Formula:**
#' ```
#' revenue_membdues = membership_dues / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param membership_dues Membership dues received.
#'
#' @param total_revenue Total revenue.
#'
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @details
#' ## Revenue Membdues Ratio - Revenue composition measure
#'
#' Formula: membership dues / total revenue. Bounded \[0, 1\].
#'
#' Membership dues provide relatively predictable recurring revenue tied to member retention. A high ratio indicates a membership-model organization whose financial health depends on maintaining and growing the member base.
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
#'   - `F9_08_REV_CONTR_MEMBSHIP_DUE`: Numerator (`membership_dues`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_membdues_ratio( df,
#'   membership_dues           = "F9_08_REV_CONTR_MEMBSHIP_DUE",
#'   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four new columns:
#'
#'   - `revenue_membdues` (raw ratio)
#'   - `revenue_membdues_w` (winsorized)
#'   - `revenue_membdues_z` (z-score)
#'   - `revenue_membdues_p` (percentile rank, 1-100)
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_revenue_membdues_ratio( df = dat10k )
#' head( d[ , c( "revenue_membdues", "revenue_membdues_w", "revenue_membdues_z", "revenue_membdues_p" ) ] )
#'
#' @export
get_revenue_membdues_ratio <- function( df,
                     membership_dues           = "F9_08_REV_CONTR_MEMBSHIP_DUE",
                     total_revenue             = "F9_08_REV_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, membership_dues, total_revenue,
                   "membership_dues", "total_revenue" )

  vars <- c( membership_dues, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, membership_dues )
  den <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  revenue_membdues <- num / den

  v <- winsorize_var( revenue_membdues, winsorize )
  REVENUE_MEMBDUES <- data.frame(
    revenue_membdues   = v$raw,
    revenue_membdues_w = v$winsorized,
    revenue_membdues_z = v$z,
    revenue_membdues_p = v$pctile )

  if ( summarize ) {
    print( summary( REVENUE_MEMBDUES ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( REVENUE_MEMBDUES$revenue_membdues,   na.rm = TRUE ), main = "REVENUE_MEMBDUES (raw)" )
    plot( density( REVENUE_MEMBDUES$revenue_membdues_w, na.rm = TRUE ), main = "REVENUE_MEMBDUES Winsorized" )
    plot( density( REVENUE_MEMBDUES$revenue_membdues_z, na.rm = TRUE ), main = "REVENUE_MEMBDUES Standardized (Z)" )
    plot( density( REVENUE_MEMBDUES$revenue_membdues_p, na.rm = TRUE ), main = "REVENUE_MEMBDUES Percentile" )
  }

  return( cbind( df, REVENUE_MEMBDUES ) )
}
