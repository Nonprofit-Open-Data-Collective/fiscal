###---------------------------------------------------
###   EARNED INCOME DEPENDENCY RATIO
###---------------------------------------------------

#' @title
#' Earned Income Dependency Ratio
#'
#' @description
#' Share of total revenue derived from earned (non-donation) income sources.
#'
#' **Formula:**
#' ```
#' eidr = earned_revenue / total_revenue
#'
#' earned_revenue = program_service_revenue + membership_dues
#'                  + royalties + other_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param program_service_rev Program service revenue. (On 990: Part VIII, line 2g; \code{F9_08_REV_PROG_TOT_TOT})
#' @param membership_dues Membership dues and assessments. (On 990: Part VIII, line 4; \code{F9_08_REV_CONTR_MEMBSHIP_DUE})
#' @param royalties Royalties. (On 990: Part VIII, line 7d; \code{F9_08_REV_OTH_ROY_TOT})
#' @param other_revenue Other miscellaneous revenue. (On 990: Part VIII, line 11e; \code{F9_08_REV_MISC_OTH_TOT})
#' @param total_revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param numerator Optional. A pre-aggregated column for earned revenue. Cannot be combined
#'   with the individual component arguments.
#' @param denominator Optional. A pre-aggregated column for total revenue. Cannot be combined
#'   with \code{total_revenue}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_earned_income_ratio( df,
#'   program_service_rev = "F9_08_REV_PROG_TOT_TOT",
#'   membership_dues     = "F9_08_REV_CONTR_MEMBSHIP_DUE",
#'   royalties           = "F9_08_REV_OTH_ROY_TOT",
#'   other_revenue       = "F9_08_REV_MISC_OTH_TOT",
#'   total_revenue       = "F9_08_REV_TOT_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{eidr}   — earned income dependency ratio (raw)
#'     \item \code{eidr_w} — winsorized version
#'     \item \code{eidr_z} — standardized z-score (based on winsorized values)
#'     \item \code{eidr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The earned income dependency ratio measures the share of total revenue from self-generated
#' sources, excluding donations and government grants. Higher values indicate greater financial
#' self-sufficiency.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_PROG_TOT_TOT}: Program service revenue (\code{program_service_rev})
#'   \item \code{F9_08_REV_CONTR_MEMBSHIP_DUE}: Membership dues (\code{membership_dues})
#'   \item \code{F9_08_REV_OTH_ROY_TOT}: Royalties (\code{royalties})
#'   \item \code{F9_08_REV_MISC_OTH_TOT}: Other miscellaneous revenue (\code{other_revenue})
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
#' d <- get_earned_income_ratio( df = dat10k )
#' head( d[ , c( "earned_income", "earned_income_w", "earned_income_z", "earned_income_p" ) ] )
#'
#' @export
get_earned_income_ratio <- function( df,
                      program_service_rev = "F9_08_REV_PROG_TOT_TOT",
                      membership_dues     = "F9_08_REV_CONTR_MEMBSHIP_DUE",
                      royalties           = "F9_08_REV_OTH_ROY_TOT",
                      other_revenue       = "F9_08_REV_MISC_OTH_TOT",
                      total_revenue       = "F9_08_REV_TOT_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( program_service_rev ) | !is.null( membership_dues ) |
                         !is.null( royalties ) | !is.null( other_revenue )
  using_component_den <- !is.null( total_revenue )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual component arguments, not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR `total_revenue`, not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual revenue columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or `total_revenue`." )
  }

  all_cols <- c( program_service_rev, membership_dues, royalties, other_revenue,
                 total_revenue, numerator, denominator )
  vars <- c( program_service_rev, membership_dues, royalties, other_revenue, total_revenue, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ program_service_rev ]] + dt[[ membership_dues ]] +
           dt[[ royalties ]] + dt[[ other_revenue ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ total_revenue ]]
  }

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  eidr <- num / den

  v <- winsorize_var( eidr, winsorize )
  EARNED_INCOME <- data.frame( earned_income   = v$raw,
                      earned_income_w = v$winsorized,
                      earned_income_z = v$z,
                      earned_income_p = v$pctile )

  if ( summarize ) {
    print( summary( EARNED_INCOME ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EARNED_INCOME$earned_income, na.rm = TRUE ), main = "EARNED_INCOME (raw)" )
    plot( density( EARNED_INCOME$earned_income_w, na.rm = TRUE ), main = "EARNED_INCOME Winsorized" )
    plot( density( EARNED_INCOME$earned_income_z, na.rm = TRUE ), main = "EARNED_INCOME Standardized (Z)" )
    plot( density( EARNED_INCOME$earned_income_p, na.rm = TRUE ), main = "EARNED_INCOME Percentile" )
  }

  return( cbind( df, EARNED_INCOME ) )
}
