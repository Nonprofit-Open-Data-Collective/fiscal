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
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param federated_campaigns Federated campaign contributions.
#'   (On 990: Part VIII, line 1a; \code{F9_08_REV_CONTR_FED_CAMP})
#' @param total_revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @details
#' \strong{Revenue Fedcampaign Ratio — Revenue composition measure}
#'
#' Formula: federated campaign contributions / total revenue. Bounded \[0, 1\].
#'
#' Federated campaigns (United Way, Combined Federal Campaign) are a specific channel of collective giving. Organizations heavily funded through federated campaigns face dependency on a single intermediary.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     \emph{VOLUNTAS}, 5(3), 273-290.
#'   \item Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in nonprofit
#'     organizations. \emph{Journal of Public Administration Research and Theory},
#'     19(4), 947-966.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_08_REV_CONTR_FED_CAMP}: Numerator (\code{federated_campaigns})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{total_revenue})
#' }
#'
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_fedcampaign_ratio( df,
#'   federated_campaigns = "F9_08_REV_CONTR_FED_CAMP",
#'   total_revenue       = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{revenue_fedcampaign}, \code{revenue_fedcampaign_w},
#'   \code{revenue_fedcampaign_z}, \code{revenue_fedcampaign_p}.
#'
#' @details
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_CONTR_FED_CAMP}: Federated campaign contributions (\code{federated_campaigns}, 990)
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{total_revenue}, 990)
#' }
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
                     winsorize  = 0.98,
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

  message( paste0( "Total revenue equal to zero: ", sum( r == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  r[ r == 0 ] <- NA

  revenue_fedcampaign <- fc / r

  v <- winsorize_var( revenue_fedcampaign, winsorize )
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
