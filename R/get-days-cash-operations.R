###---------------------------------------------------
###   DAYS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Days of Operating Cash on Hand
#'
#' @description
#' Measures how many days an organization can operate using available liquid assets.
#'
#' **Formula:**
#' ```
#' doch = liquid_assets / daily_expenses
#'
#' liquid_assets = cash + savings + pledges_receivable + accounts_receivable
#' daily_expenses = ( total_expenses - depreciation ) / 365
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY. (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param savings Short-term investments (savings), EOY. (On 990: Part X, line 2B; \code{F9_10_ASSET_SAVING_EOY})
#' @param pledges_receivable Net pledges and grants receivable, EOY. (On 990: Part X, line 3B; \code{F9_10_ASSET_PLEDGE_NET_EOY})
#' @param accounts_receivable Accounts receivable, net, EOY. (On 990: Part X, line 4B; \code{F9_10_ASSET_ACC_NET_EOY})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param depreciation Depreciation, depletion, and amortization. (On 990: Part IX, line 22A; \code{F9_09_EXP_DEPREC_TOT})
#' @param numerator Optional. A pre-aggregated column name for liquid assets. Cannot be
#'   combined with the individual asset arguments.
#' @param denominator Optional. A pre-aggregated column name for annual non-depreciation
#'   expenses (the function divides this by 365 internally). Cannot be combined with
#'   \code{total_expenses} or \code{depreciation}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_days_cash_operations( df,
#'   cash                = "F9_10_ASSET_CASH_EOY",
#'   savings             = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
#'   total_expenses      = "F9_09_EXP_TOT_TOT",
#'   depreciation        = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{doch}   — days of operating cash on hand (raw)
#'     \item \code{doch_w} — winsorized version
#'     \item \code{doch_z} — standardized z-score (based on winsorized values)
#'     \item \code{doch_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Days of operating cash on hand estimates how many days an organization could sustain
#' operations using only its liquid assets if all other revenue ceased. Daily expenses
#' exclude non-cash depreciation to approximate actual cash outflows. The monthly
#' equivalent is \code{\link{get_moch}}.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_CASH_EOY}: Cash on hand, EOY (\code{cash})
#'   \item \code{F9_10_ASSET_SAVING_EOY}: Savings and short-term investments, EOY (\code{savings})
#'   \item \code{F9_10_ASSET_PLEDGE_NET_EOY}: Net pledges receivable, EOY (\code{pledges_receivable})
#'   \item \code{F9_10_ASSET_ACC_NET_EOY}: Accounts receivable, net, EOY (\code{accounts_receivable})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#'   \item \code{F9_09_EXP_DEPREC_TOT}: Depreciation, depletion, and amortization (\code{depreciation})
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
#' d <- get_days_cash_operations( df = dat10k )
#' head( d[ , c( "days_cash_ops", "days_cash_ops_w", "days_cash_ops_z", "days_cash_ops_p" ) ] )
#'
#' @export
get_days_cash_operations <- function( df,
                      cash                = "F9_10_ASSET_CASH_EOY",
                      savings             = "F9_10_ASSET_SAVING_EOY",
                      pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                      accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                      total_expenses      = "F9_09_EXP_TOT_TOT",
                      depreciation        = "F9_09_EXP_DEPREC_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( cash ) | !is.null( savings ) |
                         !is.null( pledges_receivable ) | !is.null( accounts_receivable )
  using_component_den <- !is.null( total_expenses ) | !is.null( depreciation )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual asset arguments, not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR (total_expenses + depreciation), not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual asset columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or (total_expenses + depreciation)." )
  }

  all_cols <- c( cash, savings, pledges_receivable, accounts_receivable,
                 total_expenses, depreciation, numerator, denominator )
  vars <- c( cash, savings, pledges_receivable, accounts_receivable, total_expenses, depreciation, numerator, denominator )
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
    den <- dt[[ denominator ]] / 365
  } else {
    den <- ( dt[[ total_expenses ]] - dt[[ depreciation ]] ) / 365
  }

  message( paste0( "Daily operating expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  doch <- num / den

  v <- winsorize_var( doch, winsorize )
  DAYS_CASH_OPS <- data.frame( days_cash_ops   = v$raw,
                      days_cash_ops_w = v$winsorized,
                      days_cash_ops_z = v$z,
                      days_cash_ops_p = v$pctile )

  if ( summarize ) {
    print( summary( DAYS_CASH_OPS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DAYS_CASH_OPS$days_cash_ops, na.rm = TRUE ), main = "DAYS_CASH_OPS (raw)" )
    plot( density( DAYS_CASH_OPS$days_cash_ops_w, na.rm = TRUE ), main = "DAYS_CASH_OPS Winsorized" )
    plot( density( DAYS_CASH_OPS$days_cash_ops_z, na.rm = TRUE ), main = "DAYS_CASH_OPS Standardized (Z)" )
    plot( density( DAYS_CASH_OPS$days_cash_ops_p, na.rm = TRUE ), main = "DAYS_CASH_OPS Percentile" )
  }

  return( cbind( df, DAYS_CASH_OPS ) )
}
