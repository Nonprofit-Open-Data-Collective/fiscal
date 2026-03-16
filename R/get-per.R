###---------------------------------------------------
###   PROGRAM EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Program Expense Ratio
#'
#' @description
#' Share of total expenses devoted to program services.
#'
#' **Formula:**
#' ```
#' per = program_expenses / total_expenses
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param program_expenses Program service expenses. (On 990: Part IX, line 25B; \code{F9_09_EXP_TOT_PROG})
#' @param total_expenses Total functional expenses. Accepts one or two column names; if two
#'   are provided they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT};
#'   On EZ: Part I, line 17; \code{F9_01_EXP_TOT_CY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_per( df,
#'   program_expenses = "F9_09_EXP_TOT_PROG",
#'   total_expenses   = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{per}   — program expense ratio (raw)
#'     \item \code{per_w} — winsorized version
#'     \item \code{per_z} — standardized z-score (based on winsorized values)
#'     \item \code{per_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The program expense ratio measures the share of total spending directed toward mission-related
#' program services. Higher values indicate more mission-focused resource allocation.
#' Charity Navigator and similar watchdogs use this ratio as a key quality indicator.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_TOT_PROG}: Program service expenses (\code{program_expenses})
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
#' d <- get_per( df = dat10k )
#' head( d[ , c( "per", "per_w", "per_z", "per_p" ) ] )
#'
#' @export
get_per <- function( df,
                     program_expenses = "F9_09_EXP_TOT_PROG",
                     total_expenses   = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, program_expenses, total_expenses, "program_expenses", "total_expenses" )

  if ( length( program_expenses ) > 2 ) stop( "`program_expenses` must be one or two column names." )
  if ( length( total_expenses )   > 2 ) stop( "`total_expenses` must be one or two column names."   )

  vars <- c( program_expenses, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  p <- resolve_col( dt, program_expenses )
  e <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( e == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  e[ e == 0 ] <- NA

  per <- p / e

  v <- winsorize_var( per, winsorize )
  PER <- data.frame( per   = v$raw,
                     per_w = v$winsorized,
                     per_z = v$z,
                     per_p = v$pctile )

  if ( summarize ) {
    print( summary( PER ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( PER$per, na.rm = TRUE ), main = "PER (raw)" )
    plot( density( PER$per_w, na.rm = TRUE ), main = "PER Winsorized" )
    plot( density( PER$per_z, na.rm = TRUE ), main = "PER Standardized (Z)" )
    plot( density( PER$per_p, na.rm = TRUE ), main = "PER Percentile" )
  }

  return( cbind( df, PER ) )
}
