###---------------------------------------------------
###   CASH ON HAND
###---------------------------------------------------

#' @title
#' Cash on Hand
#'
#' @description
#' Absolute dollar amount of cash and liquid savings held at end of year.
#'
#' **Formula:**
#' ```
#' coh = cash + savings
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY. (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param savings Short-term investments (savings), EOY. (On 990: Part X, line 2B; \code{F9_10_ASSET_SAVING_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_coh( df,
#'   cash    = "F9_10_ASSET_CASH_EOY",
#'   savings = "F9_10_ASSET_SAVING_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{coh}   — cash on hand in dollars (raw)
#'     \item \code{coh_w} — winsorized version
#'     \item \code{coh_z} — standardized z-score (based on winsorized values)
#'     \item \code{coh_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Cash on hand is an absolute dollar measure rather than a ratio. It captures the
#' total liquid reserves available to an organization at year end, combining cash
#' and short-term savings. Unlike ratio-based liquidity measures, it is sensitive to
#' organizational size, so comparisons across organizations of different scales should
#' use the ratio variants (\code{\link{get_doch}}, \code{\link{get_moch}}) instead.
#'
#' Cited by Tuckman & Chang (1991) and Bowman (2011) as a core indicator of
#' financial vulnerability.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_CASH_EOY}: Cash on hand, EOY (\code{cash})
#'   \item \code{F9_10_ASSET_SAVING_EOY}: Savings and short-term investments, EOY (\code{savings})
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
#' d <- get_coh( df = dat10k )
#' head( d[ , c( "coh", "coh_w", "coh_z", "coh_p" ) ] )
#'
#' @export
get_coh <- function( df,
                     cash    = "F9_10_ASSET_CASH_EOY",
                     savings = "F9_10_ASSET_SAVING_EOY",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, cash, savings, "cash", "savings" )

  if ( length( cash )    > 2 ) stop( "`cash` must be one or two column names."    )
  if ( length( savings ) > 2 ) stop( "`savings` must be one or two column names." )

  vars <- c( cash, savings )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  c_val <- resolve_col( dt, cash )
  s_val <- resolve_col( dt, savings )

  coh <- c_val + s_val

  v <- winsorize_var( coh, winsorize )
  COH <- data.frame( coh   = v$raw,
                     coh_w = v$winsorized,
                     coh_z = v$z,
                     coh_p = v$pctile )

  if ( summarize ) {
    print( summary( COH ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( COH$coh, na.rm = TRUE ), main = "COH (raw)" )
    plot( density( COH$coh_w, na.rm = TRUE ), main = "COH Winsorized" )
    plot( density( COH$coh_z, na.rm = TRUE ), main = "COH Standardized (Z)" )
    plot( density( COH$coh_p, na.rm = TRUE ), main = "COH Percentile" )
  }

  return( cbind( df, COH ) )
}
