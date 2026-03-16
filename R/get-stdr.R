###---------------------------------------------------
###   SHORT TERM DEBT RATIO
###---------------------------------------------------

#' @title
#' Short Term Debt Ratio
#'
#' @description
#' Measures short-term obligations as a share of total net assets.
#'
#' **Formula:**
#' ```
#' stdr = short_term_liabilities / net_assets
#'
#' short_term_liabilities = accounts_payable + grants_payable
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param accounts_payable Accounts payable and accrued expenses, EOY. (On 990: Part X, line 17B; \code{F9_10_LIAB_ACC_PAYABLE_EOY})
#' @param grants_payable Grants and similar amounts payable, EOY. (On 990: Part X, line 18B; \code{F9_10_LIAB_GRANT_PAYABLE_EOY})
#' @param net_assets Total net assets, EOY. (On 990: Part X, line 33B; \code{F9_10_NAFB_TOT_EOY})
#' @param numerator Optional. A pre-aggregated column for short-term liabilities. Cannot be
#'   combined with \code{accounts_payable} or \code{grants_payable}.
#' @param denominator Optional. A pre-aggregated column for the denominator. Cannot be
#'   combined with \code{net_assets}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_stdr( df,
#'   accounts_payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
#'   grants_payable   = "F9_10_LIAB_GRANT_PAYABLE_EOY",
#'   net_assets       = "F9_10_NAFB_TOT_EOY",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{stdr}   — short term debt ratio (raw)
#'     \item \code{stdr_w} — winsorized version
#'     \item \code{stdr_z} — standardized z-score (based on winsorized values)
#'     \item \code{stdr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The short term debt ratio measures the size of near-term financial obligations relative
#' to the organization's total equity base. Higher values indicate greater near-term
#' financial pressure and reduced capacity to absorb unexpected costs.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_LIAB_ACC_PAYABLE_EOY}: Accounts payable and accrued expenses, EOY (\code{accounts_payable})
#'   \item \code{F9_10_LIAB_GRANT_PAYABLE_EOY}: Grants and similar amounts payable, EOY (\code{grants_payable})
#'   \item \code{F9_10_NAFB_TOT_EOY}: Total net assets, EOY (\code{net_assets})
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
#' d <- get_stdr( df = dat10k )
#' head( d[ , c( "stdr", "stdr_w", "stdr_z", "stdr_p" ) ] )
#'
#' @export
get_stdr <- function( df,
                      accounts_payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
                      grants_payable   = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                      net_assets       = "F9_10_NAFB_TOT_EOY",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( accounts_payable ) | !is.null( grants_payable )
  using_component_den <- !is.null( net_assets )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the payable arguments (accounts_payable, grants_payable), not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR `net_assets`, not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the payable columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or `net_assets`." )
  }

  all_cols <- c( accounts_payable, grants_payable, net_assets, numerator, denominator )
  vars <- c( accounts_payable, grants_payable, net_assets, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ accounts_payable ]] + dt[[ grants_payable ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ net_assets ]]
  }

  message( paste0( "Net assets equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  stdr <- num / den

  v <- winsorize_var( stdr, winsorize )
  STDR <- data.frame( stdr   = v$raw,
                      stdr_w = v$winsorized,
                      stdr_z = v$z,
                      stdr_p = v$pctile )

  if ( summarize ) {
    print( summary( STDR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( STDR$stdr, na.rm = TRUE ), main = "STDR (raw)" )
    plot( density( STDR$stdr_w, na.rm = TRUE ), main = "STDR Winsorized" )
    plot( density( STDR$stdr_z, na.rm = TRUE ), main = "STDR Standardized (Z)" )
    plot( density( STDR$stdr_p, na.rm = TRUE ), main = "STDR Percentile" )
  }

  return( cbind( df, STDR ) )
}
