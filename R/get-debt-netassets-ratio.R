###---------------------------------------------------
###   DEBT TO NET ASSETS RATIO
###---------------------------------------------------

#' @title
#' Debt to Net Assets Ratio
#'
#' @description
#' Compares total liabilities to total net assets.
#'
#' **Formula:**
#' ```
#' dnar = total_liabilities / total_net_assets
#' ```
#'
#' **Definitional Range**
#'
#' Bounded below at zero when total net assets are positive; unbounded above as net
#' assets approach zero. Negative values occur when total net assets are negative
#' (liabilities exceed assets), an insolvency signal, so the ratio is not monotone in
#' financial health across zero. The ratio is undefined (NaN) when total net assets
#' equal zero.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - Values below 1.0 mean liabilities are smaller than the net asset base.
#'   - Values above 3.0-5.0 are commonly flagged as high leverage in the
#'     nonprofit vulnerability literature.
#'   - Always read alongside [get_debt_equity_ratio()], which uses only
#'     unrestricted net assets: a large gap between the two means much of the
#'     equity cushion is donor restricted.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param liabilities Total liabilities, EOY. Accepts one or two column names; if two are
#'   provided they are coalesced with the first taking priority.
#'
#' @param net_assets Total net assets, EOY. Accepts one or two column names; if two are
#'   provided they are coalesced with the first taking priority.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"np"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_debt_netassets_ratio( df,
#'   liabilities = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
#'   net_assets  = c( "F9_10_NAFB_TOT_EOY", "F9_01_NAFB_TOT_EOY" ),
#'   winsorize = 0.98 ,
#'   range     = "np",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `debt_netassets`   - debt to net assets ratio (raw)
#'     - `debt_netassets_w` - winsorized version
#'     - `debt_netassets_z` - standardized z-score (based on winsorized values)
#'     - `debt_netassets_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The debt to net assets ratio compares total obligations with the organization's
#' entire equity base, restricted and unrestricted. It is the nonprofit analogue of
#' the commercial debt-to-equity ratio. Because total net assets are reported on both
#' the Form 990 (Part X line 33) and the 990-EZ (Part II line 27), this ratio is
#' available for both filer types.
#'
#' ## Formula variations and their sources
#'
#' Total liabilities (Part X line 26) / total net assets (Part X line 33). A stricter
#' variant, [get_debt_equity_ratio()], divides by unrestricted net assets only, on the
#' argument that donor-restricted resources cannot be used to meet general
#' obligations. The two ratios are identical for organizations with no restricted
#' net assets. Earlier versions of this function used unrestricted net assets, which
#' made it a duplicate of [get_debt_equity_ratio()].
#'
#' ## Canonical citations
#'
#'
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (Part X line 26; 990-EZ Part II
#'     line 26) (`liabilities`)
#'   - `F9_01_NAFB_LIAB_TOT_EOY`: Total liabilities from Part I line 21 (`liabilities`,
#'     fallback)
#'   - `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (Part X line 33; 990-EZ Part II
#'     line 27) (`net_assets`)
#'   - `F9_01_NAFB_TOT_EOY`: Net assets from Part I line 22 (`net_assets`, fallback)
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
#' d <- get_debt_netassets_ratio( df = dat10k )
#' head( d[ , c( "debt_netassets", "debt_netassets_w", "debt_netassets_z", "debt_netassets_p" ) ] )
#'
#' @export
get_debt_netassets_ratio <- function( df,
                     liabilities = c( "F9_10_LIAB_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY" ),
                     net_assets  = c( "F9_10_NAFB_TOT_EOY", "F9_01_NAFB_TOT_EOY" ),
                     winsorize = 0.98  ,
                     range     = "np" ,
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

  nan.count <- sum( n == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Net assets equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  n[ n == 0 ] <- NaN

  dnar <- l / n

  v <- apply_transformations( dnar, winsorize, range )
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
