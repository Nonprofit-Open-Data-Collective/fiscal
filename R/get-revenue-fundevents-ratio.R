###---------------------------------------------------
###   FUNDRAISING EVENTS REVENUE RATIO
###---------------------------------------------------

#' @title
#' Fundraising Events Revenue Ratio
#'
#' @description
#' Fundraising event contributions as a share of total revenue.
#'
#' **Formula:**
#' ```
#' revenue_fundevents = fundraising_event_revenue / total_revenue
#' ```
#'
#'
#' **Definitional Range**
#'
#' Bounded \[0, 1\]. Zero means none of this revenue type was received;
#' one means this channel accounted for all total revenue.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param fundraising_event_revenue Gross revenue from fundraising events.
#'
#' @param total_revenue Total revenue.
#'
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#' @details
#' ## Revenue Fundevents Ratio - Revenue composition measure
#'
#' Formula: fundraising event revenue / total revenue. Bounded \[0, 1\].
#'
#' Fundraising events (galas, walks, auctions) generate contributed revenue but typically carry high direct expenses. This ratio measures gross event revenue as a share of total revenue; net event income after direct expenses is reported elsewhere.
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
#'   - `F9_08_REV_CONTR_FUNDR_EVNT`: 
#'     Numerator (`fundraising_event_revenue`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_fundevents_ratio( df,
#'   fundraising_event_revenue = "F9_08_REV_CONTR_FUNDR_EVNT",
#'   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four new columns:
#'
#'   - `revenue_fundevents` (raw ratio)
#'   - `revenue_fundevents_w` (winsorized)
#'   - `revenue_fundevents_z` (z-score)
#'   - `revenue_fundevents_p` (percentile rank, 1-100)
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_revenue_fundevents_ratio( df = dat10k )
#' head( d[ , c( "revenue_fundevents", "revenue_fundevents_w", "revenue_fundevents_z", "revenue_fundevents_p" ) ] )
#'
#' @export
get_revenue_fundevents_ratio <- function( df,
                     fundraising_event_revenue = "F9_08_REV_CONTR_FUNDR_EVNT",
                     total_revenue             = "F9_08_REV_TOT_TOT",
                     winsorize  = 0.98 ,
                     range     = "zo" ,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, fundraising_event_revenue, total_revenue,
                   "fundraising_event_revenue", "total_revenue" )

  vars <- c( fundraising_event_revenue, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, fundraising_event_revenue )
  den <- resolve_col( dt, total_revenue )

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total revenue equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  revenue_fundevents <- num / den

  v <- apply_transformations( revenue_fundevents, winsorize, range )
  REVENUE_FUNDEVENTS <- data.frame(
    revenue_fundevents   = v$raw,
    revenue_fundevents_w = v$winsorized,
    revenue_fundevents_z = v$z,
    revenue_fundevents_p = v$pctile )

  if ( summarize ) {
    print( summary( REVENUE_FUNDEVENTS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( REVENUE_FUNDEVENTS$revenue_fundevents,   na.rm = TRUE ), main = "REVENUE_FUNDEVENTS (raw)" )
    plot( density( REVENUE_FUNDEVENTS$revenue_fundevents_w, na.rm = TRUE ), main = "REVENUE_FUNDEVENTS Winsorized" )
    plot( density( REVENUE_FUNDEVENTS$revenue_fundevents_z, na.rm = TRUE ), main = "REVENUE_FUNDEVENTS Standardized (Z)" )
    plot( density( REVENUE_FUNDEVENTS$revenue_fundevents_p, na.rm = TRUE ), main = "REVENUE_FUNDEVENTS Percentile" )
  }

  return( cbind( df, REVENUE_FUNDEVENTS ) )
}
