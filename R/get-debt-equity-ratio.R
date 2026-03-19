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
#' get_debt_equity_ratio( df,
#'   debt   = c( "F9_10_LIAB_TOT_EOY",       "F9_01_NAFB_LIAB_TOT_EOY"    ),
#'   equity = c( "F9_10_NAFB_UNRESTRICT_EOY", "F9_01_NAFB_UNRESTRICT_EOY"  ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{debt_equity}   — debt to equity ratio (raw)
#'     \item \code{debt_equity_w} — winsorized version
#'     \item \code{debt_equity_z} — standardized z-score (based on winsorized values)
#'     \item \code{debt_equity_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' The debt to equity ratio compares total obligations to the organization's equity
#' cushion — its unrestricted net assets. In the nonprofit context, unrestricted net
#' assets represent the accumulated surplus that the organization controls without
#' donor restriction: the purest measure of its financial equity. A high ratio signals
#' that liabilities substantially exceed the equity base, meaning a relatively small
#' revenue shortfall could impair the ability to meet obligations.
#'
#' This ratio is closely related to \code{\link{get_debt_assets_ratio}} (DAR), but
#' uses unrestricted net assets as the denominator instead of total assets. It is a
#' stricter solvency measure because unrestricted net assets are typically much smaller
#' than total assets, and because restricted assets cannot be used to pay general
#' obligations.
#'
#' \strong{Formula variations and their sources}
#'
#' The commercial debt/equity ratio uses total debt / shareholders' equity. The
#' nonprofit adaptation substitutes unrestricted net assets for equity, following
#' Tuckman & Chang (1991) and the argument that only unrestricted resources represent
#' true organizational equity available to cover obligations.
#'
#' An alternative uses total net assets in the denominator (restricted + unrestricted),
#' which is more generous and less analytically precise. A third variant uses only
#' long-term debt in the numerator. This implementation uses total liabilities /
#' unrestricted net assets as the most commonly cited nonprofit version.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. \emph{Nonprofit and Voluntary
#'     Sector Quarterly}, 20(4), 445-460.
#'   \item Bowman, W. (2011). Financial capacity and sustainability of ordinary
#'     nonprofits. \emph{Nonprofit Management and Leadership}, 22(1), 37-51.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded below at zero when liabilities are non-negative and unrestricted net assets
#' are positive. Unbounded above when unrestricted net assets are very small. Values
#' below zero occur when unrestricted net assets are negative (accumulated deficits
#' exceed unrestricted equity), an acute financial distress signal. The ratio is
#' undefined (NA) when unrestricted net assets equal zero.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item Values below 1.0 mean total liabilities are less than the unrestricted
#'     equity base — generally considered strong.
#'   \item Values between 1.0 and 3.0 indicate moderate leverage.
#'   \item Values above 5.0 indicate very high leverage relative to the equity base
#'     and are a common vulnerability threshold.
#'   \item Negative values indicate negative unrestricted net assets, which almost
#'     always signals significant financial stress.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_10_LIAB_TOT_EOY}: Total liabilities, EOY (\code{debt})
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{equity})
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
#' d <- get_debt_equity_ratio( df = dat10k )
#' head( d[ , c( "debt_equity", "debt_equity_w", "debt_equity_z", "debt_equity_p" ) ] )
#'
#' @export
get_debt_equity_ratio <- function( df,
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
  DEBT_EQUITY <- data.frame( debt_equity   = v$raw,
                     debt_equity_w = v$winsorized,
                     debt_equity_z = v$z,
                     debt_equity_p = v$pctile )

  if ( summarize ) {
    print( summary( DEBT_EQUITY ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DEBT_EQUITY$debt_equity, na.rm = TRUE ), main = "DEBT_EQUITY (raw)" )
    plot( density( DEBT_EQUITY$debt_equity_w, na.rm = TRUE ), main = "DEBT_EQUITY Winsorized" )
    plot( density( DEBT_EQUITY$debt_equity_z, na.rm = TRUE ), main = "DEBT_EQUITY Standardized (Z)" )
    plot( density( DEBT_EQUITY$debt_equity_p, na.rm = TRUE ), main = "DEBT_EQUITY Percentile" )
  }

  return( cbind( df, DEBT_EQUITY ) )
}
