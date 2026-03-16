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
#' get_dgdr( df,
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
#'     \item \code{dgdr}   — donation/grant dependence ratio (raw)
#'     \item \code{dgdr_w} — winsorized version
#'     \item \code{dgdr_z} — standardized z-score (based on winsorized values)
#'     \item \code{dgdr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The donation/grant dependence ratio measures the share of total revenue derived from
#' contributions and fundraising. Higher values indicate greater reliance on external
#' philanthropic support, which may create revenue volatility risk.
#'
#' **Variables used:**
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
#' d <- get_dgdr( df = dat10k )
#' head( d[ , c( "dgdr", "dgdr_w", "dgdr_z", "dgdr_p" ) ] )
#'
#' @export
get_dgdr <- function( df,
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
  DGDR <- data.frame( dgdr   = v$raw,
                      dgdr_w = v$winsorized,
                      dgdr_z = v$z,
                      dgdr_p = v$pctile )

  if ( summarize ) {
    print( summary( DGDR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DGDR$dgdr, na.rm = TRUE ), main = "DGDR (raw)" )
    plot( density( DGDR$dgdr_w, na.rm = TRUE ), main = "DGDR Winsorized" )
    plot( density( DGDR$dgdr_z, na.rm = TRUE ), main = "DGDR Standardized (Z)" )
    plot( density( DGDR$dgdr_p, na.rm = TRUE ), main = "DGDR Percentile" )
  }

  return( cbind( df, DGDR ) )
}
