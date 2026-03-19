###---------------------------------------------------
###   DONATION/GRANT DEPENDENCE RATIO
###---------------------------------------------------

#' @title
#' Donation/Grant Dependence Ratio
#'
#' @description
#' Measures reliance on contributions and fundraising as a share of total revenue.
#'
#' **Formula:**
#' ```
#' dgdr = donation_revenue / total_revenue
#'
#' donation_revenue = contributions + fundraising_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param contributions Total contributions, EOY. (On 990: Part VIII, line 1h; \code{F9_08_REV_CONTR_TOT})
#' @param fundraising_revenue Net fundraising event revenue. (On 990: Part VIII, line 8c; \code{F9_08_REV_OTH_FUNDR_NET_TOT})
#' @param total_revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param numerator Optional. A pre-aggregated column name for donation revenue, bypassing
#'   \code{contributions} and \code{fundraising_revenue}. Cannot be combined with those arguments.
#' @param denominator Optional. A pre-aggregated column name for the denominator. Cannot be
#'   combined with \code{total_revenue}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_donations_revenue_ratio( df,
#'   contributions        = "F9_08_REV_CONTR_TOT",
#'   fundraising_revenue  = "F9_08_REV_OTH_FUNDR_NET_TOT",
#'   total_revenue        = "F9_08_REV_TOT_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{donations_rev}   — donation/grant dependence ratio (raw)
#'     \item \code{donations_rev_w} — winsorized version
#'     \item \code{donations_rev_z} — standardized z-score (based on winsorized values)
#'     \item \code{donations_rev_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' The donations and grant dependence ratio measures the combined share of total
#' revenue from contributions (individual donations, foundation grants, corporate
#' giving, federated campaigns) and net fundraising event income. It captures the
#' overall dependence on philanthropic and voluntary support.
#'
#' A high ratio indicates the organization relies heavily on the goodwill of donors
#' and the fundraising environment rather than earned or contractual income. This
#' creates vulnerability to donor fatigue, economic downturns (when charitable giving
#' declines), and changes in donor priorities.
#'
#' \strong{Formula variations and their sources}
#'
#' (Total contributions + net fundraising event revenue) / total revenue (Part VIII
#' lines 1h + 8c / line 12A). Government grants are included in total contributions
#' (line 1h) in this formulation. For a pure private philanthropy measure that excludes
#' government grants, combine this ratio with \code{\link{get_grants_govt_ratio}}.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     \emph{VOLUNTAS}, 5(3), 273-290.
#'   \item Froelich, K.A. (1999). Diversification of revenue strategies. \emph{Nonprofit
#'     and Voluntary Sector Quarterly}, 28(3), 246-268.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded \[0, 1\]. Values near 1.0 characterize traditional charitable organizations
#' with minimal earned income; values near 0 characterize fee-based service providers.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item No universal threshold. Revenue diversification theory suggests values
#'     above 0.70-0.80 warrant monitoring of donor concentration and retention.
#'   \item Organizations above 0.90 have almost no earned or contractual income buffer
#'     if philanthropic support declines.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_08_REV_CONTR_TOT}: Total contributions (\code{contributions})
#'   \item \code{F9_08_REV_OTH_FUNDR_NET_TOT}: Net fundraising event revenue (\code{fundraising_revenue})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{total_revenue})
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
#' d <- get_donations_revenue_ratio( df = dat10k )
#' head( d[ , c( "donations_rev", "donations_rev_w", "donations_rev_z", "donations_rev_p" ) ] )
#'
#' @export
get_donations_revenue_ratio <- function( df,
                      contributions       = "F9_08_REV_CONTR_TOT",
                      fundraising_revenue = "F9_08_REV_OTH_FUNDR_NET_TOT",
                      total_revenue       = "F9_08_REV_TOT_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( contributions ) | !is.null( fundraising_revenue )
  using_component_den <- !is.null( total_revenue )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual component arguments (contributions, fundraising_revenue), not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR `total_revenue`, not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or (contributions + fundraising_revenue)." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or `total_revenue`." )
  }

  all_cols <- c( contributions, fundraising_revenue, total_revenue, numerator, denominator )
  vars <- c( contributions, fundraising_revenue, total_revenue, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ contributions ]] + dt[[ fundraising_revenue ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ total_revenue ]]
  }

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  dgdr <- num / den

  v <- winsorize_var( dgdr, winsorize )
  DONATIONS_REV <- data.frame( donations_rev   = v$raw,
                      donations_rev_w = v$winsorized,
                      donations_rev_z = v$z,
                      donations_rev_p = v$pctile )

  if ( summarize ) {
    print( summary( DONATIONS_REV ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DONATIONS_REV$donations_rev, na.rm = TRUE ), main = "DONATIONS_REV (raw)" )
    plot( density( DONATIONS_REV$donations_rev_w, na.rm = TRUE ), main = "DONATIONS_REV Winsorized" )
    plot( density( DONATIONS_REV$donations_rev_z, na.rm = TRUE ), main = "DONATIONS_REV Standardized (Z)" )
    plot( density( DONATIONS_REV$donations_rev_p, na.rm = TRUE ), main = "DONATIONS_REV Percentile" )
  }

  return( cbind( df, DONATIONS_REV ) )
}
