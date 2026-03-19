###---------------------------------------------------
###   INVESTMENT ASSETS RATIO
###---------------------------------------------------

#' @title
#' Investment Assets Ratio
#'
#' @description
#' Investment securities as a share of total assets.
#'
#' **Formula:**
#' ```
#' investments_assets = ( pub_traded_securities + other_securities ) / total_assets
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param pub_traded_securities Investments in publicly traded securities, EOY.
#'   (On 990: Part X, line 11B; \code{F9_10_ASSET_INVEST_SEC_EOY} (scope: 990 + 990EZ))
#' @param total_assets Total assets, EOY.
#'   (On 990: Part X, line 16B; \code{F9_10_ASSET_TOT_EOY} (scope: 990 + 990EZ))
#' @param other_securities Investments in other securities, EOY.
#'   (On 990: Part X, line 12B; \code{F9_10_ASSET_INVEST_SEC_OTH_EOY} (scope: 990 + 990EZ))
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @details
#' \strong{Primary uses and key insights}
#'
#' The investment assets ratio measures what share of total assets is held in financial
#' securities (publicly traded and other investments). It distinguishes financially
#' oriented organizations (endowments, foundations) from operationally oriented ones.
#' A high ratio indicates significant financial reserves; a low ratio indicates assets
#' are primarily operational. Most informative for endowed organizations and foundations.
#'
#' \strong{Formula variations and their sources}
#'
#' (Publicly traded securities + other securities) / total assets. Both investment
#' fields (Part X lines 11B and 12B) are PZ scope. A more comprehensive version
#' would include program-related investments (line 13B).
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Bowman, W. (2011). Financial capacity and sustainability of ordinary
#'     nonprofits. \emph{Nonprofit Management and Leadership}, 22(1), 37-51.
#'   \item Calabrese, T.D. (2013). Running on empty. \emph{Nonprofit Management and
#'     Leadership}, 23(3), 281-302.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded \[0, 1\]. Most operating nonprofits show values near zero; foundations
#' and endowed institutions may show values above 0.80.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item No universal benchmark. Operating nonprofits with values above 0.30 are
#'     typically holding significant endowment or reserve portfolios.
#'   \item A high investment ratio combined with a high debt ratio may indicate
#'     borrowing to fund operations while holding investments — worth scrutinizing.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_10_ASSET_INVEST_SEC_EOY}: Publicly traded securities, EOY (\code{pub_traded_securities})
#'   \item \code{F9_10_ASSET_INVEST_SEC_OTH_EOY}: Other securities, EOY (\code{other_securities})
#'   \item \code{F9_10_ASSET_TOT_EOY}: Total assets, EOY (\code{total_assets})
#' }
#'
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_investments_assets_ratio( df,
#'   pub_traded_securities     = "F9_10_ASSET_INVEST_SEC_EOY",
#'   total_assets              = "F9_10_ASSET_TOT_EOY",
#'   other_securities          = "F9_10_ASSET_INVEST_SEC_OTH_EOY",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{investments_assets}, \code{investments_assets_w},
#'   \code{investments_assets_z}, \code{investments_assets_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_investments_assets_ratio( df = dat10k )
#' head( d[ , c( "investments_assets", "investments_assets_w", "investments_assets_z", "investments_assets_p" ) ] )
#'
#' @export
get_investments_assets_ratio <- function( df,
                     pub_traded_securities     = "F9_10_ASSET_INVEST_SEC_EOY",
                     total_assets              = "F9_10_ASSET_TOT_EOY",
                     other_securities          = "F9_10_ASSET_INVEST_SEC_OTH_EOY",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, pub_traded_securities, total_assets,
                   "pub_traded_securities", "total_assets" )

  vars <- c( pub_traded_securities, total_assets, other_securities )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, pub_traded_securities )
  den <- resolve_col( dt, total_assets )

  message( paste0( "Total assets equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  investments_assets <- num / den

  v <- winsorize_var( investments_assets, winsorize )
  INVESTMENTS_ASSETS <- data.frame(
    investments_assets   = v$raw,
    investments_assets_w = v$winsorized,
    investments_assets_z = v$z,
    investments_assets_p = v$pctile )

  if ( summarize ) {
    print( summary( INVESTMENTS_ASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( INVESTMENTS_ASSETS$investments_assets,   na.rm = TRUE ), main = "INVESTMENTS_ASSETS (raw)" )
    plot( density( INVESTMENTS_ASSETS$investments_assets_w, na.rm = TRUE ), main = "INVESTMENTS_ASSETS Winsorized" )
    plot( density( INVESTMENTS_ASSETS$investments_assets_z, na.rm = TRUE ), main = "INVESTMENTS_ASSETS Standardized (Z)" )
    plot( density( INVESTMENTS_ASSETS$investments_assets_p, na.rm = TRUE ), main = "INVESTMENTS_ASSETS Percentile" )
  }

  return( cbind( df, INVESTMENTS_ASSETS ) )
}
