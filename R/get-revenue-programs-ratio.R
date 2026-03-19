###---------------------------------------------------
###   PROGRAM SERVICE REVENUE RATIO
###---------------------------------------------------

#' @title
#' Program Service Revenue Ratio
#'
#' @description
#' Program service revenue as a share of total revenue.
#'
#' **Formula:**
#' ```
#' revenue_programs = program_service_revenue / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param program_service_revenue Total program service revenue.
#'   (On 990: Part VIII, line 2g Column A; \code{F9_08_REV_PROG_TOT_TOT})
#' @param total_revenue Total revenue.
#'   (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_programs_ratio( df,
   program_service_revenue   = "F9_08_REV_PROG_TOT_TOT",
   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{revenue_programs}, \code{revenue_programs_w},
#'   \code{revenue_programs_z}, \code{revenue_programs_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_revenue_programs_ratio( df = dat10k )
#' head( d[ , c( "revenue_programs", "revenue_programs_w", "revenue_programs_z", "revenue_programs_p" ) ] )
#'
#' @export
get_revenue_programs_ratio <- function( df,
                     program_service_revenue   = "F9_08_REV_PROG_TOT_TOT",
                     total_revenue             = "F9_08_REV_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, program_service_revenue, total_revenue,
                   "program_service_revenue", "total_revenue" )

  vars <- c( program_service_revenue, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, program_service_revenue )
  den <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  revenue_programs <- num / den

  v <- winsorize_var( revenue_programs, winsorize )
  REVENUE_PROGRAMS <- data.frame(
    revenue_programs   = v$raw,
    revenue_programs_w = v$winsorized,
    revenue_programs_z = v$z,
    revenue_programs_p = v$pctile )

  if ( summarize ) {
    print( summary( REVENUE_PROGRAMS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( REVENUE_PROGRAMS$revenue_programs,   na.rm = TRUE ), main = "REVENUE_PROGRAMS (raw)" )
    plot( density( REVENUE_PROGRAMS$revenue_programs_w, na.rm = TRUE ), main = "REVENUE_PROGRAMS Winsorized" )
    plot( density( REVENUE_PROGRAMS$revenue_programs_z, na.rm = TRUE ), main = "REVENUE_PROGRAMS Standardized (Z)" )
    plot( density( REVENUE_PROGRAMS$revenue_programs_p, na.rm = TRUE ), main = "REVENUE_PROGRAMS Percentile" )
  }

  return( cbind( df, REVENUE_PROGRAMS ) )
}
