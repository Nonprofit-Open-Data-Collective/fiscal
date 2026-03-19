###---------------------------------------------------
###   MONTHS OF OPERATING CASH ON HAND
###---------------------------------------------------

#' @title
#' Months of Operating Cash on Hand
#'
#' @description
#' Number of months the organization can operate using available liquid assets.
#'
#' **Formula:**
#' ```
#' moch = liquid_assets / monthly_expenses
#'
#' liquid_assets    = cash + savings + pledges_receivable + accounts_receivable
#' monthly_expenses = ( total_expenses - depreciation ) / 12
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY. (On 990: Part X, line 1B; \code{F9_10_ASSET_CASH_EOY})
#' @param savings Short-term investments (savings), EOY. (On 990: Part X, line 2B; \code{F9_10_ASSET_SAVING_EOY})
#' @param pledges_receivable Pledges and grants receivable, EOY. (On 990: Part X, line 3B; \code{F9_10_ASSET_PLEDGE_NET_EOY})
#' @param accounts_receivable Accounts receivable, EOY. (On 990: Part X, line 4B; \code{F9_10_ASSET_ACC_NET_EOY})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param depreciation Depreciation, depletion, and amortization. (On 990: Part IX, line 22A; \code{F9_09_EXP_DEPREC_TOT})
#' @param numerator Optional. A pre-aggregated column name for liquid assets, bypassing the
#'   individual \code{cash}, \code{savings}, \code{pledges_receivable}, and
#'   \code{accounts_receivable} arguments. Cannot be combined with those arguments.
#' @param denominator Optional. A pre-aggregated column name for the denominator (annual
#'   non-depreciation expenses). The function divides this by 12 internally. Cannot be
#'   combined with \code{total_expenses} or \code{depreciation}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_months_cash_operations( df,
#'   cash                 = "F9_10_ASSET_CASH_EOY",
#'   savings              = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable   = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable  = "F9_10_ASSET_ACC_NET_EOY",
#'   total_expenses       = "F9_09_EXP_TOT_TOT",
#'   depreciation         = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{moch}   — months of operating cash on hand (raw)
#'     \item \code{moch_w} — winsorized version
#'     \item \code{moch_z} — standardized z-score (based on winsorized values)
#'     \item \code{moch_p} — percentile rank (1–100)
#'   }
#'
#' @details
#' Months of operating cash on hand measures how long an organization could sustain
#' operations using only its liquid assets if all other revenue ceased. It is the
#' monthly equivalent of \code{\link{get_doch}}.
#'
#' Liquid assets are defined as the sum of cash, short-term savings, pledges receivable,
#' and accounts receivable. Monthly expenses exclude non-cash depreciation charges to
#' better approximate actual cash outflows.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_ASSET_CASH_EOY}: Cash (\code{cash})
#'   \item \code{F9_10_ASSET_SAVING_EOY}: Savings (\code{savings})
#'   \item \code{F9_10_ASSET_PLEDGE_NET_EOY}: Pledges receivable (\code{pledges_receivable})
#'   \item \code{F9_10_ASSET_ACC_NET_EOY}: Accounts receivable (\code{accounts_receivable})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total expenses (\code{total_expenses})
#'   \item \code{F9_09_EXP_DEPREC_TOT}: Depreciation (\code{depreciation})
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
#' d <- get_months_cash_operations( df = dat10k )
#' head( d[ , c( "months_cash_ops", "months_cash_ops_w", "months_cash_ops_z", "months_cash_ops_p" ) ] )
#'
#' @export
get_months_cash_operations <- function( df,
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
    stop( "Supply either `numerator` OR the individual asset arguments (cash, savings, pledges_receivable, accounts_receivable), not both." )
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
    den <- dt[[ denominator ]] / 12
  } else {
    den <- ( dt[[ total_expenses ]] - dt[[ depreciation ]] ) / 12
  }

  message( paste0( "Monthly operating expenses equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  moch <- num / den

  v <- winsorize_var( moch, winsorize )
  MONTHS_CASH_OPS <- data.frame( months_cash_ops   = v$raw,
                      months_cash_ops_w = v$winsorized,
                      months_cash_ops_z = v$z,
                      months_cash_ops_p = v$pctile )

  if ( summarize ) {
    print( summary( MONTHS_CASH_OPS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( MONTHS_CASH_OPS$months_cash_ops, na.rm = TRUE ), main = "MONTHS_CASH_OPS (raw)" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_w, na.rm = TRUE ), main = "MONTHS_CASH_OPS Winsorized" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_z, na.rm = TRUE ), main = "MONTHS_CASH_OPS Standardized (Z)" )
    plot( density( MONTHS_CASH_OPS$months_cash_ops_p, na.rm = TRUE ), main = "MONTHS_CASH_OPS Percentile" )
  }

  return( cbind( df, MONTHS_CASH_OPS ) )
}
