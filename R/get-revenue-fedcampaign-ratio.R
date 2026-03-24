###---------------------------------------------------
###   FEDERATED CAMPAIGN REVENUE RATIO
###---------------------------------------------------

#' @title Federated Campaign Revenue Ratio
#'
#' @description
#' Federated campaign contributions as a share of total revenue.
#'
#' **Formula:**
#' ```
#' revenue_fedcampaign = federated_campaigns / total_revenue
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
#' @param federated_campaigns Federated campaign contributions.
#'
#' @param total_revenue Total revenue.
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#' @details
#' ## Revenue Fedcampaign Ratio - Revenue composition measure
#'
#' Formula: federated campaign contributions / total revenue. Bounded \[0, 1\].
#'
#' Federated campaigns (United Way, Combined Federal Campaign) are a specific channel of collective giving. Organizations heavily funded through federated campaigns face dependency on a single intermediary.
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
#'   - `F9_08_REV_CONTR_FED_CAMP`: Numerator (`federated_campaigns`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_fedcampaign_ratio( df,
#'   federated_campaigns = "F9_08_REV_CONTR_FED_CAMP",
#'   total_revenue       = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four new columns:
#'
#'   - `revenue_fedcampaign` (raw ratio)
#'   - `revenue_fedcampaign_w` (winsorized)
#'   - `revenue_fedcampaign_z` (z-score)
#'   - `revenue_fedcampaign_p` (percentile rank, 1-100)
#'
#' @details
#' **Variables used:**
#'
#'   - `F9_08_REV_CONTR_FED_CAMP`: Federated campaign contributions (`federated_campaigns`, 990)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`, 990)
#'
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_revenue_fedcampaign_ratio( df = dat10k )
#' head( d[ , c( "revenue_fedcampaign", "revenue_fedcampaign_w", "revenue_fedcampaign_z", "revenue_fedcampaign_p" ) ] )
#'
#' @export
get_revenue_fedcampaign_ratio <- function( df,
                     federated_campaigns = "F9_08_REV_CONTR_FED_CAMP",
                     total_revenue       = "F9_08_REV_TOT_TOT",
                     winsorize  = 0.98 ,
                     range     = "zo" ,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, federated_campaigns, total_revenue,
                   "federated_campaigns", "total_revenue" )

  vars <- c( federated_campaigns, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  fc <- resolve_col( dt, federated_campaigns )
  r  <- resolve_col( dt, total_revenue )

  nan.count <- sum( r == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total revenue equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  r[ r == 0 ] <- NaN

  revenue_fedcampaign <- fc / r

  v <- apply_transformations( revenue_fedcampaign, winsorize, range )
  REVENUE_FEDCAMPAIGN <- data.frame(
    revenue_fedcampaign   = v$raw,
    revenue_fedcampaign_w = v$winsorized,
    revenue_fedcampaign_z = v$z,
    revenue_fedcampaign_p = v$pctile )

  if ( summarize ) {
    print( summary( REVENUE_FEDCAMPAIGN ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( REVENUE_FEDCAMPAIGN$revenue_fedcampaign,   na.rm = TRUE ), main = "REVENUE_FEDCAMPAIGN (raw)" )
    plot( density( REVENUE_FEDCAMPAIGN$revenue_fedcampaign_w, na.rm = TRUE ), main = "REVENUE_FEDCAMPAIGN Winsorized" )
    plot( density( REVENUE_FEDCAMPAIGN$revenue_fedcampaign_z, na.rm = TRUE ), main = "REVENUE_FEDCAMPAIGN Standardized (Z)" )
    plot( density( REVENUE_FEDCAMPAIGN$revenue_fedcampaign_p, na.rm = TRUE ), main = "REVENUE_FEDCAMPAIGN Percentile" )
  }

  return( cbind( df, REVENUE_FEDCAMPAIGN ) )
}
