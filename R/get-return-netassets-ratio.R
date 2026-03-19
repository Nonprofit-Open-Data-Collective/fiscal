###---------------------------------------------------
###   RETURN ON NET ASSETS (RONA)
###---------------------------------------------------

#' @title
#' Return on Net Assets
#'
#' @description
#' Net surplus or deficit as a share of beginning-of-year net assets.
#'
#' **Formula:**
#' ```
#' rona = revenues_less_expenses / net_assets_boy
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param revenues_less_expenses Revenues less expenses (net surplus or deficit) for the
#'   current year. Accepts one or two column names; if two are provided they are coalesced
#'   with the 990 value taking priority over 990EZ.
#'   (On 990: Part I, line 19 CY; \code{F9_01_EXP_REV_LESS_EXP_CY};
#'   If \code{F9_01_EXP_REV_LESS_EXP_CY} is unavailable, it is computed as
#'   \code{F9_01_REV_TOT_CY} minus \code{F9_01_EXP_TOT_CY}.)
#' @param net_assets_boy Total net assets, beginning of year. Accepts one or two column names.
#'   (On 990: Part X, line 33A; \code{F9_10_NAFB_TOT_BOY};
#'   On EZ: Part I, line 22 BOY; \code{F9_01_NAFB_TOT_BOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_return_netassets_ratio( df,
#'   revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
#'   net_assets_boy         = c( "F9_10_NAFB_TOT_BOY", "F9_01_NAFB_TOT_BOY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{return_netassets}   — return on net assets (raw)
#'     \item \code{return_netassets_w} — winsorized version
#'     \item \code{return_netassets_z} — standardized z-score (based on winsorized values)
#'     \item \code{return_netassets_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' Return on net assets (RONA) measures how effectively the organization used its equity
#' base to generate a surplus. By using beginning-of-year net assets as the denominator,
#' it expresses the annual surplus as a percentage return on the organizational equity
#' in place at the start of the year — analogous to return on equity (ROE) in commercial
#' finance.
#'
#' RONA is closely related to \code{\link{get_netassets_growth_ratio}}: both measure
#' year-over-year equity change, but RONA uses the reported revenues-less-expenses figure
#' while the growth ratio uses the direct balance sheet comparison (EOY - BOY). The two
#' can differ when there are other net asset adjustments (line 20 on Form 990 Part I).
#'
#' \strong{Formula variations and their sources}
#'
#' Revenues less expenses / net assets BOY. Using beginning-of-year net assets in the
#' denominator (rather than ending or average) avoids circular dependency: the ending
#' value is determined partly by the surplus being measured. This is the standard
#' approach in the nonprofit literature (Greenlee & Trussel 2000).
#'
#' An alternative uses average net assets ((BOY + EOY)/2), which smooths distortions
#' from large mid-year transactions, but requires two balance sheet fields and is
#' not standard in the 990-based literature.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. \emph{Nonprofit Management and Leadership}, 11(2),
#'     199-210.
#'   \item Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005). Assessing
#'     financial vulnerability in the nonprofit sector. \emph{Harvard Business School
#'     Working Paper 04-016}.
#'   \item Nonprofit Finance Fund. \emph{State of the Nonprofit Sector Survey} (annual).
#' }
#'
#' \strong{Definitional range}
#'
#' Unbounded in both directions. A value of 0 means break-even. The typical range for
#' nonprofits is approximately \[-0.30, 0.30\]. The ratio is undefined (NA) when
#' beginning net assets equal zero. Extreme values occur when BOY net assets are near
#' zero (small denominator) rather than when the surplus itself is large.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item Small positive values (0.02 to 0.10) are considered healthy.
#'   \item Sustained negative RONA over multiple years is a financial vulnerability
#'     indicator (Greenlee & Trussel 2000).
#'   \item Like all ratio measures, RONA is most useful in trend analysis and
#'     within-subsector comparisons rather than as an absolute benchmark.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_01_EXP_REV_LESS_EXP_CY}: Revenues less expenses, current year (\code{revenues_less_expenses})
#'   \item \code{F9_10_NAFB_TOT_BOY}: Total net assets, BOY (\code{net_assets_boy}, 990)
#'   \item \code{F9_01_NAFB_TOT_BOY}: Net assets from Part I, BOY (\code{net_assets_boy}, 990EZ fallback)
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
#' d <- get_return_netassets_ratio( df = dat10k )
#' head( d[ , c( "return_netassets", "return_netassets_w", "return_netassets_z", "return_netassets_p" ) ] )
#'
#' @export
get_return_netassets_ratio <- function( df,
                      revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
                      net_assets_boy         = c( "F9_10_NAFB_TOT_BOY",
                                                   "F9_01_NAFB_TOT_BOY" ),
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, revenues_less_expenses, net_assets_boy,
                   "revenues_less_expenses", "net_assets_boy" )

  if ( length( revenues_less_expenses ) > 2 )
    stop( "`revenues_less_expenses` must be one or two column names." )
  if ( length( net_assets_boy ) > 2 )
    stop( "`net_assets_boy` must be one or two column names." )

  all_cols <- c( revenues_less_expenses, net_assets_boy )
  vars <- c( revenues_less_expenses, net_assets_boy )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  s <- resolve_col( dt, revenues_less_expenses )
  n <- resolve_col( dt, net_assets_boy )

  message( paste0( "Beginning net assets equal to zero: ", sum( n == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  n[ n == 0 ] <- NA

  rona <- s / n

  v <- winsorize_var( rona, winsorize )
  RETURN_NETASSETS <- data.frame( return_netassets   = v$raw,
                      return_netassets_w = v$winsorized,
                      return_netassets_z = v$z,
                      return_netassets_p = v$pctile )

  if ( summarize ) {
    print( summary( RETURN_NETASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( RETURN_NETASSETS$return_netassets, na.rm = TRUE ), main = "RETURN_NETASSETS (raw)" )
    plot( density( RETURN_NETASSETS$return_netassets_w, na.rm = TRUE ), main = "RETURN_NETASSETS Winsorized" )
    plot( density( RETURN_NETASSETS$return_netassets_z, na.rm = TRUE ), main = "RETURN_NETASSETS Standardized (Z)" )
    plot( density( RETURN_NETASSETS$return_netassets_p, na.rm = TRUE ), main = "RETURN_NETASSETS Percentile" )
  }

  return( cbind( df, RETURN_NETASSETS ) )
}
