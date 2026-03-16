###---------------------------------------------------
###   FUNDRAISING EFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Fundraising Efficiency Ratio
#'
#' @description
#' Cost of raising one dollar of contributions; measures how efficiently fundraising
#' expenses generate donated revenue.
#'
#' **Formula:**
#' ```
#' fer = fundraising_expenses / total_contributions
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param fundraising_expenses Fundraising expenses. (On 990: Part IX, line 25D; \code{F9_09_EXP_TOT_FUNDR})
#' @param total_contributions Total contributions received. (On 990: Part VIII, line 1h; \code{F9_08_REV_CONTR_TOT})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_fer( df,
#'   fundraising_expenses = "F9_09_EXP_TOT_FUNDR",
#'   total_contributions  = "F9_08_REV_CONTR_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{fer}   — fundraising efficiency ratio (raw)
#'     \item \code{fer_w} — winsorized version
#'     \item \code{fer_z} — standardized z-score (based on winsorized values)
#'     \item \code{fer_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The fundraising efficiency ratio expresses the cost of raising each dollar of
#' contributions. A value of 0.20 means the organization spent 20 cents to raise
#' each dollar. Lower values indicate more efficient fundraising.
#'
#' Note that organizations with no fundraising expenses (e.g., those relying entirely
#' on unsolicited donations or government grants) will return a ratio of zero, which
#' should be interpreted with caution rather than as evidence of exceptional efficiency.
#' Organizations that receive no contributions will produce NA values (denominator zero).
#'
#' Cited by GuideStar and IRS as a standard accountability metric.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_TOT_FUNDR}: Fundraising expenses (\code{fundraising_expenses})
#'   \item \code{F9_08_REV_CONTR_TOT}: Total contributions received (\code{total_contributions})
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
#' d <- get_fer( df = dat10k )
#' head( d[ , c( "fer", "fer_w", "fer_z", "fer_p" ) ] )
#'
#' @export
get_fer <- function( df,
                     fundraising_expenses = "F9_09_EXP_TOT_FUNDR",
                     total_contributions  = "F9_08_REV_CONTR_TOT",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, fundraising_expenses, total_contributions,
                   "fundraising_expenses", "total_contributions" )

  if ( length( fundraising_expenses ) > 2 )
    stop( "`fundraising_expenses` must be one or two column names." )
  if ( length( total_contributions ) > 2 )
    stop( "`total_contributions` must be one or two column names." )

  vars <- c( fundraising_expenses, total_contributions )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  f <- resolve_col( dt, fundraising_expenses )
  c <- resolve_col( dt, total_contributions )

  message( paste0( "Total contributions equal to zero: ", sum( c == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  c[ c == 0 ] <- NA

  fer <- f / c

  v <- winsorize_var( fer, winsorize )
  FER <- data.frame( fer   = v$raw,
                     fer_w = v$winsorized,
                     fer_z = v$z,
                     fer_p = v$pctile )

  if ( summarize ) {
    print( summary( FER ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( FER$fer, na.rm = TRUE ), main = "FER (raw)" )
    plot( density( FER$fer_w, na.rm = TRUE ), main = "FER Winsorized" )
    plot( density( FER$fer_z, na.rm = TRUE ), main = "FER Standardized (Z)" )
    plot( density( FER$fer_p, na.rm = TRUE ), main = "FER Percentile" )
  }

  return( cbind( df, FER ) )
}
