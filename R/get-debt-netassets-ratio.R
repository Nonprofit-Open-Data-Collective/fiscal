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
#' get_debt_netassets_ratio( df,
#'   liabilities = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
#'   net_assets  = "F9_10_NAFB_UNRESTRICT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{debt_netassets}   — debt to net assets ratio (raw)
#'     \item \code{debt_netassets_w} — winsorized version
#'     \item \code{debt_netassets_z} — standardized z-score (based on winsorized values)
#'     \item \code{debt_netassets_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' The debt to net assets ratio compares total liabilities to unrestricted net assets,
#' similar to \code{\link{get_debt_equity_ratio}}. The key distinction is in scope:
#' this function accepts both 990 and 990EZ filers (PZ scope) because it can use the
#' Part I liabilities field as a fallback for 990EZ filers, whereas \code{\link{get_debt_equity_ratio}}
#' requires the Part X unrestricted net assets breakdown which is only on the full 990.
#'
#' The practical use case for this function is cross-sectional analyses that include
#' both 990 and 990EZ filers. For full-990-only datasets, \code{\link{get_debt_equity_ratio}}
#' provides a more precise measure.
#'
#' \strong{Formula variations and their sources}
#'
#' See \code{\link{get_debt_equity_ratio}} for a full discussion of formula variations.
#' This implementation uses \code{F9_10_NAFB_UNRESTRICT_EOY} as the denominator
#' (unrestricted net assets, Part X line 27B) and falls back to the Part I summary
#' liabilities field for the numerator when needed. The Part X unrestricted net assets
#' field is not available on 990EZ, so 990EZ filers will have NA for this ratio.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. \emph{Nonprofit and Voluntary
#'     Sector Quarterly}, 20(4), 445-460.
#' }
#'
#' \strong{Definitional range}
#'
#' Same as \code{\link{get_debt_equity_ratio}}: bounded below at zero when both
#' numerator and denominator are positive; unbounded above; negative when unrestricted
#' net assets are negative.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' See \code{\link{get_debt_equity_ratio}} for benchmarks. As a rule of thumb, values
#' above 3.0-5.0 are commonly flagged as high leverage in the nonprofit vulnerability
#' literature.
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_10_LIAB_TOT_EOY}: Total liabilities, EOY (\code{liabilities}, 990)
#'   \item \code{F9_01_NAFB_LIAB_TOT_EOY}: Total liabilities from Part I (\code{liabilities}, 990EZ fallback)
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
#' d <- get_debt_netassets_ratio( df = dat10k )
#' head( d[ , c( "debt_netassets", "debt_netassets_w", "debt_netassets_z", "debt_netassets_p" ) ] )
#'
#' @export
get_debt_netassets_ratio <- function( df,
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
  DEBT_NETASSETS <- data.frame( debt_netassets   = v$raw,
                     debt_netassets_w = v$winsorized,
                     debt_netassets_z = v$z,
                     debt_netassets_p = v$pctile )

  if ( summarize ) {
    print( summary( DEBT_NETASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DEBT_NETASSETS$debt_netassets, na.rm = TRUE ), main = "DEBT_NETASSETS (raw)" )
    plot( density( DEBT_NETASSETS$debt_netassets_w, na.rm = TRUE ), main = "DEBT_NETASSETS Winsorized" )
    plot( density( DEBT_NETASSETS$debt_netassets_z, na.rm = TRUE ), main = "DEBT_NETASSETS Standardized (Z)" )
    plot( density( DEBT_NETASSETS$debt_netassets_p, na.rm = TRUE ), main = "DEBT_NETASSETS Percentile" )
  }

  return( cbind( df, DEBT_NETASSETS ) )
}
