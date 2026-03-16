###---------------------------------------------------
###   BURN RATE
###---------------------------------------------------

#' @title
#' Burn Rate
#'
#' @description
#' Monthly rate at which an organization draws down its cash reserves.
#'
#' **Formula:**
#' ```
#' brr = ( cash_boy - cash_eoy ) / months_in_period
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash_eoy Cash on hand, end of year. (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param cash_boy Cash on hand, beginning of year. (On 990: Part X, line 1A; \code{F9_10_ASSET_CASH_BOY})
#' @param months_in_period Number of months in the reporting period. Defaults to 12 for
#'   a standard annual filing. Adjust for short-year filers.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_brr( df,
#'   cash_eoy         = "F9_10_ASSET_CASH_EOY",
#'   cash_boy         = "F9_10_ASSET_CASH_BOY",
#'   months_in_period = 12,
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{brr}   — monthly burn rate in dollars (raw)
#'     \item \code{brr_w} — winsorized version
#'     \item \code{brr_z} — standardized z-score (based on winsorized values)
#'     \item \code{brr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Burn rate measures the average monthly decrease in cash over the reporting period.
#' Positive values indicate net cash consumption (spending down reserves); negative
#' values indicate net cash accumulation. Unlike ratio-based measures, burn rate is
#' expressed in dollar terms and is sensitive to organizational size.
#'
#' Note that \code{F9_10_ASSET_CASH_BOY} (beginning-of-year cash, Part X line 1A)
#' may not be present in all efile datasets. Researchers working with multi-year
#' panels can compute this by lagging the prior year's \code{F9_10_ASSET_CASH_EOY}.
#'
#' Cited by Ritchie et al. (2007) and Calabrese (2013).
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_CASH_EOY}: Cash on hand, end of year (\code{cash_eoy})
#'   \item \code{F9_10_ASSET_CASH_BOY}: Cash on hand, beginning of year (\code{cash_boy})
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
#' d <- get_brr( df = dat10k )
#' head( d[ , c( "brr", "brr_w", "brr_z", "brr_p" ) ] )
#'
#' @export
get_brr <- function( df,
                     cash_eoy         = "F9_10_ASSET_CASH_EOY",
                     cash_boy         = "F9_10_ASSET_CASH_BOY",
                     months_in_period = 12,
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( is.null( cash_eoy ) ) stop( "`cash_eoy` cannot be NULL." )
  if ( is.null( cash_boy ) ) stop( "`cash_boy` cannot be NULL." )

  if ( !is.numeric( months_in_period ) || months_in_period <= 0 )
    stop( "`months_in_period` must be a positive number." )

  if ( length( cash_eoy ) > 2 ) stop( "`cash_eoy` must be one or two column names." )
  if ( length( cash_boy ) > 2 ) stop( "`cash_boy` must be one or two column names." )

  vars <- c( cash_eoy, cash_boy )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  eoy <- resolve_col( dt, cash_eoy )
  boy <- resolve_col( dt, cash_boy )

  brr <- ( boy - eoy ) / months_in_period

  v <- winsorize_var( brr, winsorize )
  BRR <- data.frame( brr   = v$raw,
                     brr_w = v$winsorized,
                     brr_z = v$z,
                     brr_p = v$pctile )

  if ( summarize ) {
    print( summary( BRR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( BRR$brr, na.rm = TRUE ), main = "BRR (raw)" )
    plot( density( BRR$brr_w, na.rm = TRUE ), main = "BRR Winsorized" )
    plot( density( BRR$brr_z, na.rm = TRUE ), main = "BRR Standardized (Z)" )
    plot( density( BRR$brr_p, na.rm = TRUE ), main = "BRR Percentile" )
  }

  return( cbind( df, BRR ) )
}
