###---------------------------------------------------
###   NET ASSETS COMPOSITION RATIO
###---------------------------------------------------

#' @title
#' Net Assets Composition Ratio
#'
#' @description
#' Share of total net assets that are unrestricted and available for general use.
#'
#' **Formula:**
#' ```
#' nacr = unrestricted_net_assets / total_net_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY.
#' @param total_net_assets Total net assets, EOY.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_netassets_composition_ratio( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   total_net_assets        = "F9_10_NAFB_TOT_EOY",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `netassets_comp`   - net assets composition ratio (raw)
#'     - `netassets_comp_w` - winsorized version
#'     - `netassets_comp_z` - standardized z-score (based on winsorized values)
#'     - `netassets_comp_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The net assets composition ratio measures what fraction of total net assets is
#' unrestricted - available for general operations without donor-imposed constraints.
#' A high ratio means the organization has substantial financial flexibility; a low
#' ratio means most net assets are restricted and cannot be redirected to cover
#' operating shortfalls or unexpected needs.
#'
#' This metric is specific to the nonprofit sector: commercial firms have no equivalent
#' concept because all equity is by definition "unrestricted." It is particularly
#' relevant for organizations that receive large restricted gifts or endowments, where
#' total net assets may look strong but the unrestricted portion available for operations
#' may be minimal.
#'
#' ## Formula variations and their sources
#'
#' Unrestricted net assets (Part X line 27B) / total net assets (Part X line 33B).
#' An alternative formulation divides unrestricted net assets by total assets, which
#' is more conservative and is sometimes used in rating agency analyses. The net assets
#' denominator version is more common in the academic literature.
#'
#' ## Canonical citations
#'
#'
#'   - Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     *VOLUNTAS: International Journal of Voluntary and Nonprofit Organizations*,
#'     5(3), 273-290. - Discusses the role of unrestricted resources in financial
#'     flexibility.
#'   - Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
#'     Leadership*, 23(3), 281-302. - Examines unrestricted reserve composition.
#'
#'
#' ## Definitional range
#'
#' Bounded \[0, 1\] when both numerator and denominator are positive: zero means all
#' net assets are restricted; one means all net assets are unrestricted. Values outside
#' this range occur when either unrestricted or total net assets are negative.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - Values above 0.50 indicate that at least half of net assets are
#'     unrestricted - generally considered healthy.
#'   - Values below 0.20 suggest heavy donor-restricted balance sheets and
#'     limited financial flexibility.
#'   - Organizations with large endowments will show low ratios if the endowment
#'     is permanently restricted, which may not indicate financial weakness.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_NAFB_UNRESTRICT_EOY`: 
#'     Unrestricted net assets, EOY (`unrestricted_net_assets`)
#'   - `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (`total_net_assets`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, NA values in
#'   the financial input columns are imputed to zero before the ratio is computed,
#'   respecting form scope: Part X and VIII/IX fields (990 only) are imputed only
#'   for 990 filers; Part I summary fields (990 + 990EZ) are imputed for all filers.
#'   The returned dataframe always contains the original unmodified input columns.
#'
#' @param summarize Logical. If `TRUE`, prints a `summary()` of
#'   the results and plots density curves for all four output columns
#'   (raw, winsorized, z-score, percentile). Defaults to `FALSE`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' d <- get_netassets_composition_ratio( df = dat10k )
#' head( d[ , c( "netassets_comp", "netassets_comp_w", "netassets_comp_z", "netassets_comp_p" ) ] )
#'
#' @export
get_netassets_composition_ratio <- function( df,
                      unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                      total_net_assets        = "F9_10_NAFB_TOT_EOY",
                      winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, unrestricted_net_assets, total_net_assets,
                   "unrestricted_net_assets", "total_net_assets" )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( total_net_assets ) > 2 )
    stop( "`total_net_assets` must be one or two column names." )

  vars <- c( unrestricted_net_assets, total_net_assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  u <- resolve_col( dt, unrestricted_net_assets )
  t <- resolve_col( dt, total_net_assets )

  message( paste0( "Total net assets equal to zero: ", sum( t == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  t[ t == 0 ] <- NA

  nacr <- u / t

  v <- winsorize_var( nacr, winsorize )
  NETASSETS_COMP <- data.frame( netassets_comp   = v$raw,
                      netassets_comp_w = v$winsorized,
                      netassets_comp_z = v$z,
                      netassets_comp_p = v$pctile )

  if ( summarize ) {
    print( summary( NETASSETS_COMP ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( NETASSETS_COMP$netassets_comp, na.rm = TRUE ), main = "NETASSETS_COMP (raw)" )
    plot( density( NETASSETS_COMP$netassets_comp_w, na.rm = TRUE ), main = "NETASSETS_COMP Winsorized" )
    plot( density( NETASSETS_COMP$netassets_comp_z, na.rm = TRUE ), main = "NETASSETS_COMP Standardized (Z)" )
    plot( density( NETASSETS_COMP$netassets_comp_p, na.rm = TRUE ), main = "NETASSETS_COMP Percentile" )
  }

  return( cbind( df, NETASSETS_COMP ) )
}
