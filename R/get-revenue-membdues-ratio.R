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
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param membership_dues Membership dues received.
#'   (On 990: Part VIII, line 1b; \code{F9_08_REV_CONTR_MEMBSHIP_DUE})
#' @param total_revenue Total revenue.
#'   (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_membdues_ratio( df,
   membership_dues           = "F9_08_REV_CONTR_MEMBSHIP_DUE",
   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{revenue_membdues}, \code{revenue_membdues_w},
#'   \code{revenue_membdues_z}, \code{revenue_membdues_p}.
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
