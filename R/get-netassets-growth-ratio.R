###---------------------------------------------------
###   OPERATING RATIO
###---------------------------------------------------

#' @title
#' Operating Ratio
#'
#' @description
#' Year-over-year change in total net assets, expressed as a proportion of beginning net assets.
#'
#' **Formula:**
#' ```
#' or = ( net_assets_eoy - net_assets_boy ) / net_assets_boy
#' ```
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param net_assets_eoy Total net assets, end of year. Accepts one or two column names;
#'   if two are provided they are coalesced with the 990 value taking priority over 990EZ.
#'   (On 990: Part X, line 33B; \code{F9_10_NAFB_TOT_EOY};
#'   On EZ: Part II; \code{F9_01_NAFB_TOT_EOY})
#' @param net_assets_boy Total net assets, beginning of year. Accepts one or two column names.
#'   (On 990: Part X, line 33A; \code{F9_10_NAFB_TOT_BOY};
#'   On EZ: Part II; \code{F9_01_NAFB_TOT_BOY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_netassets_growth_ratio( df,
#'   net_assets_eoy = c( "F9_10_NAFB_TOT_EOY", "F9_01_NAFB_TOT_EOY" ),
#'   net_assets_boy = c( "F9_10_NAFB_TOT_BOY", "F9_01_NAFB_TOT_BOY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{or}   — operating ratio (raw)
#'     \item \code{or_w} — winsorized version
#'     \item \code{or_z} — standardized z-score (based on winsorized values)
#'     \item \code{or_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The operating ratio measures financial performance as the percentage change in total net
#' assets from the beginning to the end of the fiscal year. Positive values indicate a
#' financial surplus; negative values indicate a deficit.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_TOT_EOY}: Total net assets, EOY (\code{net_assets_eoy}, 990)
#'   \item \code{F9_01_NAFB_TOT_EOY}: Total net assets, EOY from Part I (\code{net_assets_eoy}, 990EZ)
#'   \item \code{F9_10_NAFB_TOT_BOY}: Total net assets, BOY (\code{net_assets_boy}, 990)
#'   \item \code{F9_01_NAFB_TOT_BOY}: Total net assets, BOY from Part I (\code{net_assets_boy}, 990EZ)
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
#' d <- get_netassets_growth_ratio( df = dat10k )
#' head( d[ , c( "netassets_growth", "netassets_growth_w", "netassets_growth_z", "netassets_growth_p" ) ] )
#'
#' @export
get_netassets_growth_ratio <- function( df,
                    net_assets_eoy = c( "F9_10_NAFB_TOT_EOY", "F9_01_NAFB_TOT_EOY" ),
                    net_assets_boy = c( "F9_10_NAFB_TOT_BOY", "F9_01_NAFB_TOT_BOY" ),
                    winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, net_assets_eoy, net_assets_boy, "net_assets_eoy", "net_assets_boy" )

  if ( length( net_assets_eoy ) > 2 ) stop( "`net_assets_eoy` must be one or two column names." )
  if ( length( net_assets_boy ) > 2 ) stop( "`net_assets_boy` must be one or two column names." )

  vars <- c( net_assets_eoy, net_assets_boy )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  eoy <- resolve_col( dt, net_assets_eoy )
  boy <- resolve_col( dt, net_assets_boy )

  message( paste0( "Beginning net assets equal to zero: ", sum( boy == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  boy[ boy == 0 ] <- NA

  or <- ( eoy - boy ) / boy

  v <- winsorize_var( or, winsorize )
  NETASSETS_GROWTH <- data.frame( netassets_growth   = v$raw,
                    netassets_growth_w = v$winsorized,
                    netassets_growth_z = v$z,
                    netassets_growth_p = v$pctile )

  if ( summarize ) {
    print( summary( NETASSETS_GROWTH ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( NETASSETS_GROWTH$netassets_growth, na.rm = TRUE ), main = "NETASSETS_GROWTH (raw)" )
    plot( density( NETASSETS_GROWTH$netassets_growth_w, na.rm = TRUE ), main = "NETASSETS_GROWTH Winsorized" )
    plot( density( NETASSETS_GROWTH$netassets_growth_z, na.rm = TRUE ), main = "NETASSETS_GROWTH Standardized (Z)" )
    plot( density( NETASSETS_GROWTH$netassets_growth_p, na.rm = TRUE ), main = "NETASSETS_GROWTH Percentile" )
  }

  return( cbind( df, NETASSETS_GROWTH ) )
}
