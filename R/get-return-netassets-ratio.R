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
#'     \item \code{rona}   — return on net assets (raw)
#'     \item \code{rona_w} — winsorized version
#'     \item \code{rona_z} — standardized z-score (based on winsorized values)
#'     \item \code{rona_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' Return on net assets expresses the annual surplus or deficit relative to the equity
#' base at the start of the year. Using beginning-of-year net assets as the denominator
#' (rather than end-of-year) avoids circular dependency — the surplus itself affects
#' ending net assets. A positive RONA indicates the organization grew its equity base
#' during the year.
#'
#' RONA is closely related to \code{\link{get_or}} (Operating Ratio). Both measure
#' year-over-year net asset growth, but use different numerators: \code{get_or} computes
#' (EOY - BOY) / BOY using net asset balances directly, while \code{get_return_netassets_ratio} uses
#' the reported revenues-less-expenses figure, which may differ due to other balance
#' sheet adjustments (line 20 on Form 990 Part I).
#'
#' Cited by NCCS and Nonprofit Finance Fund.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_01_EXP_REV_LESS_EXP_CY}: Revenues less expenses, current year (\code{revenues_less_expenses}, 990 + 990EZ)
#'   \item \code{F9_10_NAFB_TOT_BOY}: Total net assets, BOY (\code{net_assets_boy}, 990)
#'   \item \code{F9_01_NAFB_TOT_BOY}: Net assets or fund balances, BOY from Part I (\code{net_assets_boy}, 990EZ)
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
