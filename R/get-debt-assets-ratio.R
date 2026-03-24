###---------------------------------------------------
###   DEBT TO ASSET RATIO
###---------------------------------------------------

#' @title
#' Debt to Asset Ratio
#'
#' @description
#' Proportion of total assets financed through liabilities.
#'
#' **Formula:**
#' ```
#' dar = total_liabilities / total_assets
#' ```
#'
#' **Definitional Range**
#'
#' Theoretically bounded \[0, 1\] when net assets are positive: zero means no debt;
#' one means liabilities equal total assets (zero net assets). Values above 1.0 occur
#' when total liabilities exceed total assets, i.e., the organization has negative net
#' assets (accumulated deficits exceed equity). This is a distress indicator but not
#' uncommon in capital-intensive nonprofits (hospitals, housing) carrying large
#' long-term debt.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **Below 0.50**: Generally considered financially stable.
#'   - **0.50-0.70**: Moderate leverage; manageable but warrants monitoring.
#'   - **Above 0.70**: High leverage; common vulnerability threshold (Tuckman
#'     & Chang 1991).
#'   - **Above 1.0**: Negative net assets; acute financial risk.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param debt Total liabilities, EOY. Accepts one or two column names; if two are provided
#'   they are coalesced with the 990 value taking priority over 990EZ.
#'
#' @param assets Total assets, EOY. Accepts one or two column names.
#'
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_debt_assets_ratio( df,
#'   debt   = c( "F9_10_LIAB_TOT_EOY",  "F9_01_NAFB_LIAB_TOT_EOY"  ),
#'   assets = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
#'   winsorize = 0.98 ,
#'   range     = "zo",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `debt_assets`   - debt to asset ratio (raw)
#'     - `debt_assets_w` - winsorized version
#'     - `debt_assets_z` - standardized z-score (based on winsorized values)
#'     - `debt_assets_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The debt to asset ratio (also called the leverage ratio or debt ratio) measures
#' what fraction of total assets is financed by liabilities rather than equity (net
#' assets). It is the most fundamental solvency indicator in both commercial and
#' nonprofit finance: a ratio approaching 1.0 means the organization is almost entirely
#' debt-financed and has minimal equity cushion; a ratio near 0 means assets are
#' almost entirely owned free of debt.
#'
#' For nonprofits this ratio is particularly important because: (1) they cannot raise
#' equity capital by issuing stock, so debt is the primary external financing mechanism;
#' and (2) funders, lenders, and rating agencies commonly use this ratio to assess
#' creditworthiness and organizational stability.
#'
#' ## Formula variations and their sources
#'
#' The commercial formula is identical: total liabilities / total assets. Nonprofit
#' applications use the same ratio but may define "total liabilities" differently.
#' This implementation uses the 990 Part X total liabilities (line 26B) or the Part I
#' summary equivalent for 990EZ filers (line 26B), which includes all short- and
#' long-term obligations reported on the balance sheet.
#'
#' Some studies use only long-term debt in the numerator to focus on structural
#' leverage rather than near-term obligations. The full liabilities version is used
#' here as the most comprehensive and commonly cited measure.
#'
#' ## Why this formula was chosen
#'
#' Total liabilities / total assets is the most universally understood and reported
#' leverage measure. Using total liabilities rather than a subset avoids definitional
#' ambiguity about which obligations to include. The PZ scope (both 990 and 990EZ)
#' maximizes coverage across all filing types.
#'
#' ## Canonical citations
#'
#'
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460.
#'   - Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. *Nonprofit Management and Leadership*, 11(2),
#'     199-210.
#'   - Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005). Assessing
#'     financial vulnerability in the nonprofit sector. *Harvard Business School
#'     Working Paper 04-016*.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`debt`, 990)
#'   - `F9_01_NAFB_LIAB_TOT_EOY`: Total liabilities from Part I (`debt`, 990EZ fallback)
#'   - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`assets`, 990)
#'   - `F9_01_NAFB_ASSET_TOT_EOY`: Total assets from Part I (`assets`, 990EZ fallback)
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
#' d <- get_debt_assets_ratio( df = dat10k )
#' head( d[ , c( "debt_assets", "debt_assets_w", "debt_assets_z", "debt_assets_p" ) ] )
#'
#' @export
get_debt_assets_ratio <- function( df,
                     debt   = c( "F9_10_LIAB_TOT_EOY",  "F9_01_NAFB_LIAB_TOT_EOY"  ),
                     assets = c( "F9_10_ASSET_TOT_EOY", "F9_01_NAFB_ASSET_TOT_EOY" ),
                     winsorize = 0.98  ,
                     range     = "zo" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, debt, assets, "debt", "assets" )

  if ( length( debt )   > 2 ) stop( "`debt` must be one or two column names."   )
  if ( length( assets ) > 2 ) stop( "`assets` must be one or two column names." )

  vars <- c( debt, assets )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  d <- resolve_col( dt, debt )
  a <- resolve_col( dt, assets )

  nan.count <- sum( a == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Assets equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  a[ a == 0 ] <- NaN

  dar <- d / a

  v <- apply_transformations( dar, winsorize, range )
  DEBT_ASSETS <- data.frame( debt_assets   = v$raw,
                     debt_assets_w = v$winsorized,
                     debt_assets_z = v$z,
                     debt_assets_p = v$pctile )

  if ( summarize ) {
    print( summary( DEBT_ASSETS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DEBT_ASSETS$debt_assets, na.rm = TRUE ), main = "DEBT_ASSETS (raw)" )
    plot( density( DEBT_ASSETS$debt_assets_w, na.rm = TRUE ), main = "DEBT_ASSETS Winsorized" )
    plot( density( DEBT_ASSETS$debt_assets_z, na.rm = TRUE ), main = "DEBT_ASSETS Standardized (Z)" )
    plot( density( DEBT_ASSETS$debt_assets_p, na.rm = TRUE ), main = "DEBT_ASSETS Percentile" )
  }

  return( cbind( df, DEBT_ASSETS ) )
}
