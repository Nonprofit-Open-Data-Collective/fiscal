###---------------------------------------------------
###   DEBT TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio
#'
#' @description
#' Proportion of total assets financed through liabilities.
#'
#' **Formula:**
#' ```
#' dar = total_liabilities / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param debt Total liabilities, EOY. Accepts one or two column names; if two are provided
#'   they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part X, line 26B; \code{F9_10_LIAB_TOT_EOY};
#'   On EZ: Part II, line 26B; \code{F9_01_NAFB_LIAB_TOT_EOY})
#' @param assets Total assets, EOY. Accepts one or two column names.
#'   (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY};
#'   On EZ: Part II, line 25B; \code{F9_01_NAFB_ASSET_TOT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_dar( df,
#'   debt   = c( "F9_10_LIAB_TOT_EOY",  "F9_01_NAFB_LIAB_TOT_EOY"  ),
#'   assets = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{dar}   — debt to asset ratio (raw)
#'     \item \code{dar_w} — winsorized version
#'     \item \code{dar_z} — standardized z-score (based on winsorized values)
#'     \item \code{dar_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The debt to asset ratio measures the degree of leverage — how much of the organization's
#' asset base is financed by debt rather than equity. Higher values indicate greater
#' financial risk.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_LIAB_TOT_EOY}: Total liabilities, EOY (\code{debt}, 990)
#'   \item \code{F9_01_NAFB_LIAB_TOT_EOY}: Total liabilities from Part I (\code{debt}, 990EZ)
#'   \item \code{F9_10_ASSET_TOT_EOY}: Total assets, EOY (\code{assets}, 990)
#'   \item \code{F9_01_NAFB_ASSET_TOT_EOY}: Total assets from Part I (\code{assets}, 990EZ)
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
#' d <- get_dar( df = dat10k )
#' head( d[ , c( "dar", "dar_w", "dar_z", "dar_p" ) ] )
#'
#' @export
get_dar <- function( df,
                     debt   = c( "F9_10_LIAB_TOT_EOY",  "F9_01_NAFB_LIAB_TOT_EOY"  ),
                     assets = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, debt, assets, "debt", "assets" )

  if ( length( debt )   > 2 ) stop( "`debt` must be one or two column names."   )
  if ( length( assets ) > 2 ) stop( "`assets` must be one or two column names." )

  vars <- c( debt, assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  d <- resolve_col( dt, debt )
  a <- resolve_col( dt, assets )

  message( paste0( "Assets equal to zero: ", sum( a == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  a[ a == 0 ] <- NA

  dar <- d / a

  v <- winsorize_var( dar, winsorize )
  DAR <- data.frame( dar   = v$raw,
                     dar_w = v$winsorized,
                     dar_z = v$z,
                     dar_p = v$pctile )

  if ( summarize ) {
    print( summary( DAR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DAR$dar, na.rm = TRUE ), main = "DAR (raw)" )
    plot( density( DAR$dar_w, na.rm = TRUE ), main = "DAR Winsorized" )
    plot( density( DAR$dar_z, na.rm = TRUE ), main = "DAR Standardized (Z)" )
    plot( density( DAR$dar_p, na.rm = TRUE ), main = "DAR Percentile" )
  }

  return( cbind( df, DAR ) )
}
