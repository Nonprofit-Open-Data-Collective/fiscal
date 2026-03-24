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
#'
#' **Definitional Range**
#'
#' Bounded \[0, 1\]. Zero means none of this revenue type was received;
#' one means this channel accounted for all total revenue.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param program_service_revenue Total program service revenue.
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
#' ## Revenue Programs Ratio - Revenue composition measure
#'
#' Formula: program service revenue / total revenue. Bounded \[0, 1\].
#'
#' Pure program service share, distinct from get_earned_income_ratio which also includes dues, royalties, and misc.
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
#'   - `F9_08_REV_PROG_TOT_TOT`: Numerator (`program_service_revenue`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_revenue_programs_ratio( df,
#'   program_service_revenue   = "F9_08_REV_PROG_TOT_TOT",
#'   total_revenue             = "F9_08_REV_TOT_TOT",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four new columns:
#'
#'   - `revenue_programs` (raw ratio)
#'   - `revenue_programs_w` (winsorized)
#'   - `revenue_programs_z` (z-score)
#'   - `revenue_programs_p` (percentile rank, 1-100)
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
                     winsorize  = 0.98 ,
                     range     = "zo" ,
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

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total revenue equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  revenue_programs <- num / den

  v <- apply_transformations( revenue_programs, winsorize, range )
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
