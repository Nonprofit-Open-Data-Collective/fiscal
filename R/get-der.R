###---------------------------------------------------
###   DEBT TO EQUITY RATIO
###---------------------------------------------------

#' @title
#' Debt to Equity Ratio
#'
#' @description
#' Compares total liabilities to unrestricted net assets.
#'
#' **Formula:**
#' ```
#' der = total_liabilities / unrestricted_net_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param debt Total liabilities, EOY. Accepts one or two column names; if two are provided
#'   they are coalesced with the 990 value taking priority.
#'   (On 990: Part X, line 26B; \code{F9_10_LIAB_TOT_EOY};
#'   On EZ: Part II, line 26B; \code{F9_01_NAFB_LIAB_TOT_EOY})
#' @param equity Unrestricted net assets, EOY. Accepts one or two column names.
#'   (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY};
#'   On EZ: \code{F9_01_NAFB_UNRESTRICT_EOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_der( df,
#'   debt   = c( "F9_10_LIAB_TOT_EOY",       "F9_01_NAFB_LIAB_TOT_EOY"    ),
#'   equity = c( "F9_10_NAFB_UNRESTRICT_EOY", "F9_01_NAFB_UNRESTRICT_EOY"  ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{der}   — debt to equity ratio (raw)
#'     \item \code{der_w} — winsorized version
#'     \item \code{der_z} — standardized z-score (based on winsorized values)
#'     \item \code{der_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The debt to equity ratio indicates the degree to which an organization relies on
#' debt financing relative to its own equity (unrestricted net assets). Higher values
#' suggest greater financial leverage and risk.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_LIAB_TOT_EOY}: Total liabilities, EOY (\code{debt}, 990)
#'   \item \code{F9_01_NAFB_LIAB_TOT_EOY}: Total liabilities from Part I (\code{debt}, 990EZ)
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{equity}, 990)
#'   \item \code{F9_01_NAFB_UNRESTRICT_EOY}: Unrestricted net assets from Part I (\code{equity}, 990EZ)
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
#' d <- get_der( df = dat10k )
#' head( d[ , c( "der", "der_w", "der_z", "der_p" ) ] )
#'
#' @export
get_der <- function( df,
                     debt   = c( "F9_10_LIAB_TOT_EOY",       "F9_01_NAFB_LIAB_TOT_EOY"   ),
                     equity = c( "F9_10_NAFB_UNRESTRICT_EOY", "F9_01_NAFB_UNRESTRICT_EOY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, debt, equity, "debt", "equity" )

  if ( length( debt )   > 2 ) stop( "`debt` must be one or two column names."   )
  if ( length( equity ) > 2 ) stop( "`equity` must be one or two column names." )

  vars <- c( debt, equity )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  d <- resolve_col( dt, debt )
  e <- resolve_col( dt, equity )

  message( paste0( "Equity equal to zero: ", sum( e == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  e[ e == 0 ] <- NA

  der <- d / e

  v <- winsorize_var( der, winsorize )
  DER <- data.frame( der   = v$raw,
                     der_w = v$winsorized,
                     der_z = v$z,
                     der_p = v$pctile )

  if ( summarize ) {
    print( summary( DER ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DER$der, na.rm = TRUE ), main = "DER (raw)" )
    plot( density( DER$der_w, na.rm = TRUE ), main = "DER Winsorized" )
    plot( density( DER$der_z, na.rm = TRUE ), main = "DER Standardized (Z)" )
    plot( density( DER$der_p, na.rm = TRUE ), main = "DER Percentile" )
  }

  return( cbind( df, DER ) )
}
