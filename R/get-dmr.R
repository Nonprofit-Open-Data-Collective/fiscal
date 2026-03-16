###---------------------------------------------------
###   DEBT TO NET ASSETS RATIO
###---------------------------------------------------

#' @title
#' Debt to Net Assets Ratio
#'
#' @description
#' Compares total liabilities to unrestricted net assets.
#'
#' **Formula:**
#' ```
#' dmr = total_liabilities / unrestricted_net_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param liabilities Total liabilities, EOY. Accepts one or two column names; if two are
#'   provided they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part X, line 26B; \code{F9_10_LIAB_TOT_EOY};
#'   On EZ: Part II, line 26B; \code{F9_01_NAFB_LIAB_TOT_EOY})
#' @param net_assets Unrestricted net assets, EOY. (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_dmr( df,
#'   liabilities = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
#'   net_assets  = "F9_10_NAFB_UNRESTRICT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{dmr}   — debt to net assets ratio (raw)
#'     \item \code{dmr_w} — winsorized version
#'     \item \code{dmr_z} — standardized z-score (based on winsorized values)
#'     \item \code{dmr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The debt to net assets ratio measures the relationship between an organization's total
#' obligations and its unrestricted equity base. Lower values indicate a stronger capacity
#' to absorb debt.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_LIAB_TOT_EOY}: Total liabilities, EOY (\code{liabilities}, 990)
#'   \item \code{F9_01_NAFB_LIAB_TOT_EOY}: Total liabilities from Part I (\code{liabilities}, 990EZ)
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{net_assets})
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
#' d <- get_dmr( df = dat10k )
#' head( d[ , c( "dmr", "dmr_w", "dmr_z", "dmr_p" ) ] )
#'
#' @export
get_dmr <- function( df,
                     liabilities = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
                     net_assets  = "F9_10_NAFB_UNRESTRICT_EOY",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, liabilities, net_assets, "liabilities", "net_assets" )

  if ( length( liabilities ) > 2 ) stop( "`liabilities` must be one or two column names." )
  if ( length( net_assets )  > 2 ) stop( "`net_assets` must be one or two column names."  )

  vars <- c( liabilities, net_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  l <- resolve_col( dt, liabilities )
  n <- resolve_col( dt, net_assets )

  message( paste0( "Net assets equal to zero: ", sum( n == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  n[ n == 0 ] <- NA

  dmr <- l / n

  v <- winsorize_var( dmr, winsorize )
  DMR <- data.frame( dmr   = v$raw,
                     dmr_w = v$winsorized,
                     dmr_z = v$z,
                     dmr_p = v$pctile )

  if ( summarize ) {
    print( summary( DMR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DMR$dmr, na.rm = TRUE ), main = "DMR (raw)" )
    plot( density( DMR$dmr_w, na.rm = TRUE ), main = "DMR Winsorized" )
    plot( density( DMR$dmr_z, na.rm = TRUE ), main = "DMR Standardized (Z)" )
    plot( density( DMR$dmr_p, na.rm = TRUE ), main = "DMR Percentile" )
  }

  return( cbind( df, DMR ) )
}
