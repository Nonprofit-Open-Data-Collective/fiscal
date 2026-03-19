###---------------------------------------------------
###   SELF SUFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio
#'
#' @description
#' Measures whether program service revenue is sufficient to cover total expenses.
#'
#' **Formula:**
#' ```
#' ssr = program_service_revenue / total_expenses
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param program_service_rev Program service revenue. Accepts one or two column names;
#'   if two are provided they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part VIII, line 2g; \code{F9_08_REV_PROG_TOT_TOT};
#'   On EZ: Part I, line 2; \code{F9_01_REV_PROG_TOT_CY})
#' @param total_expenses Total functional expenses. Accepts one or two column names.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT};
#'   On EZ: Part I, line 17; \code{F9_01_EXP_TOT_CY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_self_sufficiency_ratio( df,
#'   program_service_rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY" ),
#'   total_expenses      = c( "F9_09_EXP_TOT_TOT",      "F9_01_EXP_TOT_CY"      ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{ssr}   — self sufficiency ratio (raw)
#'     \item \code{ssr_w} — winsorized version
#'     \item \code{ssr_z} — standardized z-score (based on winsorized values)
#'     \item \code{ssr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The self sufficiency ratio measures the degree to which an organization can cover its
#' total expenses through earned program service revenue alone, without relying on donations,
#' grants, or investment income. A ratio at or above 1.0 indicates full self-sufficiency.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_PROG_TOT_TOT}: Program service revenue (\code{program_service_rev}, 990)
#'   \item \code{F9_01_REV_PROG_TOT_CY}: Program service revenue from Part I (\code{program_service_rev}, 990EZ)
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses}, 990)
#'   \item \code{F9_01_EXP_TOT_CY}: Total expenses from Part I (\code{total_expenses}, 990EZ)
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
#' d <- get_self_sufficiency_ratio( df = dat10k )
#' head( d[ , c( "self_suff", "self_suff_w", "self_suff_z", "self_suff_p" ) ] )
#'
#' @export
get_self_sufficiency_ratio <- function( df,
                     program_service_rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY" ),
                     total_expenses      = c( "F9_09_EXP_TOT_TOT",      "F9_01_EXP_TOT_CY"      ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, program_service_rev, total_expenses,
                   "program_service_rev", "total_expenses" )

  if ( length( program_service_rev ) > 2 ) stop( "`program_service_rev` must be one or two column names." )
  if ( length( total_expenses )      > 2 ) stop( "`total_expenses` must be one or two column names."      )

  vars <- c( program_service_rev, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  p <- resolve_col( dt, program_service_rev )
  e <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( e == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  e[ e == 0 ] <- NA

  ssr <- p / e

  v <- winsorize_var( ssr, winsorize )
  SELF_SUFF <- data.frame( self_suff   = v$raw,
                     self_suff_w = v$winsorized,
                     self_suff_z = v$z,
                     self_suff_p = v$pctile )

  if ( summarize ) {
    print( summary( SELF_SUFF ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( SELF_SUFF$self_suff, na.rm = TRUE ), main = "SELF_SUFF (raw)" )
    plot( density( SELF_SUFF$self_suff_w, na.rm = TRUE ), main = "SELF_SUFF Winsorized" )
    plot( density( SELF_SUFF$self_suff_z, na.rm = TRUE ), main = "SELF_SUFF Standardized (Z)" )
    plot( density( SELF_SUFF$self_suff_p, na.rm = TRUE ), main = "SELF_SUFF Percentile" )
  }

  return( cbind( df, SELF_SUFF ) )
}
