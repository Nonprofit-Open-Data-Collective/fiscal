###---------------------------------------------------
###   SURPLUS MARGIN
###---------------------------------------------------

#' @title
#' Surplus Margin
#'
#' @description
#' Net surplus or deficit as a share of total revenue.
#'
#' **Formula:**
#' ```
#' sm = revenues_less_expenses / total_revenue
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param revenues_less_expenses Revenues less expenses (net surplus or deficit) for the
#'   current year. Accepts one or two column names; if two are provided they are coalesced
#'   with the 990 value taking priority over 990EZ.
#'   (On 990: Part I, line 19 CY; \code{F9_01_EXP_REV_LESS_EXP_CY};
#'   If \code{F9_01_EXP_REV_LESS_EXP_CY} is unavailable, it is computed as
#'   \code{F9_01_REV_TOT_CY} minus \code{F9_01_EXP_TOT_CY}.)
#' @param total_revenue Total revenue. Accepts one or two column names.
#'   (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT};
#'   On EZ: Part I, line 9; \code{F9_01_REV_TOT_CY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_surplus_margin_ratio( df,
#'   revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
#'   total_revenue          = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{sm}   — surplus margin (raw)
#'     \item \code{sm_w} — winsorized version
#'     \item \code{sm_z} — standardized z-score (based on winsorized values)
#'     \item \code{sm_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The surplus margin measures what fraction of total revenue remains after all expenses.
#' Positive values indicate a financial surplus; negative values indicate a deficit.
#' Unlike \code{\link{get_podpm}}, which computes (revenue - expenses) / revenue using
#' Part VIII and Part IX directly, the surplus margin uses the Part I summary line, which
#' may reflect accounting adjustments not captured in the Part VIII/IX subtraction alone.
#'
#' The primary field \code{F9_01_EXP_REV_LESS_EXP_CY} is available on both the full 990
#' (Part I, line 19) and 990EZ (Part I, line 18) forms (scope: PZ). The secondary field
#' \code{F9_11_RECO_REV_LESS_EXP} is the Part XI reconciliation value, available only to
#' full 990 filers, and is used as a fallback when the Part I field is missing.
#'
#' Cited by the National Center for Charitable Statistics (NCCS).
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_01_EXP_REV_LESS_EXP_CY}: Revenues less expenses, current year (\code{revenues_less_expenses}, 990 + 990EZ)
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue from Part VIII (\code{total_revenue}, 990)
#'   \item \code{F9_01_REV_TOT_CY}: Total revenue from Part I (\code{total_revenue}, 990EZ)
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
#' d <- get_surplus_margin_ratio( df = dat10k )
#' head( d[ , c( "surplus_margin", "surplus_margin_w", "surplus_margin_z", "surplus_margin_p" ) ] )
#'
#' @export
get_surplus_margin_ratio <- function( df,
                    revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
                    total_revenue          = c( "F9_08_REV_TOT_TOT",
                                                "F9_01_REV_TOT_CY" ),
                    winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, revenues_less_expenses, total_revenue,
                   "revenues_less_expenses", "total_revenue" )

  if ( length( revenues_less_expenses ) > 2 )
    stop( "`revenues_less_expenses` must be one or two column names." )
  if ( length( total_revenue ) > 2 )
    stop( "`total_revenue` must be one or two column names." )

  all_cols <- c( revenues_less_expenses, total_revenue )
  vars <- c( revenues_less_expenses, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  s <- resolve_col( dt, revenues_less_expenses )
  r <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( r == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  r[ r == 0 ] <- NA

  sm <- s / r

  v <- winsorize_var( sm, winsorize )
  SURPLUS_MARGIN <- data.frame( surplus_margin   = v$raw,
                    surplus_margin_w = v$winsorized,
                    surplus_margin_z = v$z,
                    surplus_margin_p = v$pctile )

  if ( summarize ) {
    print( summary( SURPLUS_MARGIN ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( SURPLUS_MARGIN$surplus_margin, na.rm = TRUE ), main = "SURPLUS_MARGIN (raw)" )
    plot( density( SURPLUS_MARGIN$surplus_margin_w, na.rm = TRUE ), main = "SURPLUS_MARGIN Winsorized" )
    plot( density( SURPLUS_MARGIN$surplus_margin_z, na.rm = TRUE ), main = "SURPLUS_MARGIN Standardized (Z)" )
    plot( density( SURPLUS_MARGIN$surplus_margin_p, na.rm = TRUE ), main = "SURPLUS_MARGIN Percentile" )
  }

  return( cbind( df, SURPLUS_MARGIN ) )
}
