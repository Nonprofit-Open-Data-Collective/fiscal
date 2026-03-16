###---------------------------------------------------
###   QUICK RATIO
###---------------------------------------------------

#' @title
#' Quick Ratio
#'
#' @description
#' Measures liquidity using only the most liquid assets, excluding inventory and prepaid expenses.
#'
#' **Formula:**
#' ```
#' qr = quick_assets / current_liabilities
#'
#' quick_assets        = cash + savings + pledges_receivable + accounts_receivable
#' current_liabilities = accounts_payable + grants_payable
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY. (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param savings Short-term investments (savings), EOY. (On 990: Part X, line 2B; \code{F9_10_ASSET_SAVING_EOY})
#' @param pledges_receivable Net pledges and grants receivable, EOY. (On 990: Part X, line 3B; \code{F9_10_ASSET_PLEDGE_NET_EOY})
#' @param accounts_receivable Accounts receivable, net, EOY. (On 990: Part X, line 4B; \code{F9_10_ASSET_ACC_NET_EOY})
#' @param accounts_payable Accounts payable and accrued expenses, EOY. (On 990: Part X, line 17B; \code{F9_10_LIAB_ACC_PAYABLE_EOY})
#' @param grants_payable Grants and similar amounts payable, EOY. (On 990: Part X, line 18B; \code{F9_10_LIAB_GRANT_PAYABLE_EOY})
#' @param numerator Optional. A pre-aggregated column for quick assets. Cannot be combined
#'   with the individual asset arguments.
#' @param denominator Optional. A pre-aggregated column for current liabilities. Cannot be
#'   combined with \code{accounts_payable} or \code{grants_payable}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_qr( df,
#'   cash                = "F9_10_ASSET_CASH_EOY",
#'   savings             = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
#'   accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
#'   grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{qr}   — quick ratio (raw)
#'     \item \code{qr_w} — winsorized version
#'     \item \code{qr_z} — standardized z-score (based on winsorized values)
#'     \item \code{qr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The quick ratio is a stricter liquidity measure than the current ratio (\code{\link{get_cr}}).
#' It excludes inventory and prepaid expenses from current assets, retaining only the most
#' immediately convertible resources. A ratio above 1.0 indicates that highly liquid assets
#' are sufficient to cover current liabilities.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_CASH_EOY}: Cash on hand, EOY (\code{cash})
#'   \item \code{F9_10_ASSET_SAVING_EOY}: Savings and short-term investments, EOY (\code{savings})
#'   \item \code{F9_10_ASSET_PLEDGE_NET_EOY}: Net pledges receivable, EOY (\code{pledges_receivable})
#'   \item \code{F9_10_ASSET_ACC_NET_EOY}: Accounts receivable, net, EOY (\code{accounts_receivable})
#'   \item \code{F9_10_LIAB_ACC_PAYABLE_EOY}: Accounts payable and accrued expenses, EOY (\code{accounts_payable})
#'   \item \code{F9_10_LIAB_GRANT_PAYABLE_EOY}: Grants and similar amounts payable, EOY (\code{grants_payable})
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
#' d <- get_qr( df = dat10k )
#' head( d[ , c( "qr", "qr_w", "qr_z", "qr_p" ) ] )
#'
#' @export
get_qr <- function( df,
                    cash                = "F9_10_ASSET_CASH_EOY",
                    savings             = "F9_10_ASSET_SAVING_EOY",
                    pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                    accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                    accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
                    grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
                    numerator   = NULL,
                    denominator = NULL,
                    winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( cash ) | !is.null( savings ) |
                         !is.null( pledges_receivable ) | !is.null( accounts_receivable )
  using_component_den <- !is.null( accounts_payable ) | !is.null( grants_payable )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual asset arguments (cash, savings, pledges_receivable, accounts_receivable), not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR the payable arguments (accounts_payable, grants_payable), not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual asset columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or the payable columns." )
  }

  all_cols <- c( cash, savings, pledges_receivable, accounts_receivable,
                 accounts_payable, grants_payable, numerator, denominator )
  vars <- c( cash, savings, pledges_receivable, accounts_receivable, accounts_payable, grants_payable, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ cash ]] + dt[[ savings ]] +
           dt[[ pledges_receivable ]] + dt[[ accounts_receivable ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ accounts_payable ]] + dt[[ grants_payable ]]
  }

  message( paste0( "Current liabilities equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  qr <- num / den

  v <- winsorize_var( qr, winsorize )
  QR <- data.frame( qr   = v$raw,
                    qr_w = v$winsorized,
                    qr_z = v$z,
                    qr_p = v$pctile )

  if ( summarize ) {
    print( summary( QR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( QR$qr, na.rm = TRUE ), main = "QR (raw)" )
    plot( density( QR$qr_w, na.rm = TRUE ), main = "QR Winsorized" )
    plot( density( QR$qr_z, na.rm = TRUE ), main = "QR Standardized (Z)" )
    plot( density( QR$qr_p, na.rm = TRUE ), main = "QR Percentile" )
  }

  return( cbind( df, QR ) )
}
