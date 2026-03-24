###---------------------------------------------------
###   MONTHS OF LIQUID UNRESTRICTED NET ASSETS (LUNA)
###---------------------------------------------------

#' @title
#' Months of Liquid Unrestricted Net Assets (LUNA)
#'
#' @description
#' Number of months an organization can operate using its liquid unrestricted net assets.
#'
#' **Formula:**
#' ```
#' luna = liquid_unrestricted_net_assets / monthly_expenses
#'
#' liquid_unrestricted_net_assets = unrestricted_net_assets
#'                                  - ( net_fixed_assets - mortgages_payable )
#' monthly_expenses = total_expenses / 12
#' ```
#'
#' **Definitional Range**
#'
#' Unbounded in both directions, expressed in months. Negative values indicate that
#' fixed property (net of mortgage debt) exceeds unrestricted net assets. The practical
#' range for operating nonprofits is approximately \[-12, 36\] months.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - Same general benchmarks as [get_operating_reserve_ratio()]: 3-6 months
#'     is considered adequate for most operating nonprofits.
#'   - Negative LUNA values are particularly informative: they indicate
#'     unrestricted equity is entirely absorbed by illiquid fixed assets.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY.
#' @param net_fixed_assets Net land, buildings, and equipment, EOY.
#' @param mortgages_payable Mortgages and notes payable, EOY.
#' @param total_expenses Total functional expenses.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"np"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_liquid_assets_months( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   mortgages_payable       = "F9_10_LIAB_MTG_NOTE_EOY",
#'   total_expenses          = "F9_09_EXP_TOT_TOT",
#'   winsorize = 0.98 ,
#'   range     = "np",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `liquid_assets_months`   - months of liquid unrestricted net assets (raw)
#'     - `liquid_assets_months_w` - winsorized version
#'     - `liquid_assets_months_z` - standardized z-score (based on winsorized values)
#'     - `liquid_assets_months_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' Liquid unrestricted net assets (LUNA) measures how many months of operating expenses
#' are covered by unrestricted net assets that are not tied up in illiquid fixed property
#' (net of associated mortgage debt). It is a refinement of the operating reserve ratio
#' ([get_operating_reserve_ratio()]) that more precisely isolates liquid
#' resources by netting out the value of fixed assets but adding back the portion
#' financed by mortgage debt (since that debt reduces the net equity encumbrance of
#' the fixed asset).
#'
#' ## Formula variations and their sources
#'
#' (Unrestricted net assets - net fixed assets + mortgages payable) / (total expenses / 12).
#' The addition of mortgage debt back into the numerator reflects the logic that if the
#' organization sold its building, it would receive the net proceeds (market value minus
#' mortgage balance) - and the mortgage liability already reduces unrestricted net assets.
#' Adding it back avoids double-counting the encumbrance.
#'
#' This formulation follows Zietlow et al. (2007) and is cited by Calabrese (2013).
#' It differs from [get_operating_reserve_ratio()] only in the addition of
#' mortgages_payable to the numerator.
#'
#' ## Canonical citations
#'
#'
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley. - Primary source for the LUNA definition.
#'   - Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
#'     Leadership*, 23(3), 281-302. - Empirical application and validation.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_NAFB_UNRESTRICT_EOY`: 
#'     Unrestricted net assets, EOY (`unrestricted_net_assets`)
#'   - `F9_10_ASSET_LAND_BLDG_NET_EOY`: 
#'     Net land, buildings, and equipment (`net_fixed_assets`)
#'   - `F9_10_LIAB_MTG_NOTE_EOY`: Mortgages and notes payable (`mortgages_payable`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
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
#' d <- get_liquid_assets_months( df = dat10k )
#' head( d[ , c( "liquid_assets_months", "liquid_assets_months_w", "liquid_assets_months_z", "liquid_assets_months_p" ) ] )
#'
#' @export
get_liquid_assets_months <- function( df,
                      unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                      net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
                      mortgages_payable       = "F9_10_LIAB_MTG_NOTE_EOY",
                      total_expenses          = "F9_09_EXP_TOT_TOT",
                      winsorize = 0.98  ,
                     range     = "np" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( is.null( unrestricted_net_assets ) )
    stop( "`unrestricted_net_assets` cannot be NULL." )
  if ( is.null( net_fixed_assets ) )
    stop( "`net_fixed_assets` cannot be NULL." )
  if ( is.null( mortgages_payable ) )
    stop( "`mortgages_payable` cannot be NULL." )
  if ( is.null( total_expenses ) )
    stop( "`total_expenses` cannot be NULL." )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( net_fixed_assets ) > 2 )
    stop( "`net_fixed_assets` must be one or two column names." )
  if ( length( mortgages_payable ) > 2 )
    stop( "`mortgages_payable` must be one or two column names." )
  if ( length( total_expenses ) > 2 )
    stop( "`total_expenses` must be one or two column names." )

  all_cols <- c( unrestricted_net_assets, net_fixed_assets,
                 mortgages_payable, total_expenses )
  vars <- c( unrestricted_net_assets, net_fixed_assets, mortgages_payable, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  una <- resolve_col( dt, unrestricted_net_assets )
  nfa <- resolve_col( dt, net_fixed_assets )
  mnp <- resolve_col( dt, mortgages_payable )
  exp <- resolve_col( dt, total_expenses )

  nan.count <- sum( exp == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total expenses equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  exp[ exp == 0 ] <- NaN

  luna_val <- ( una - ( nfa - mnp ) ) / ( exp / 12 )

  v <- apply_transformations( luna_val, winsorize, range )
  LIQUID_ASSETS_MONTHS <- data.frame( liquid_assets_months   = v$raw,
                      liquid_assets_months_w = v$winsorized,
                      liquid_assets_months_z = v$z,
                      liquid_assets_months_p = v$pctile )

  if ( summarize ) {
    print( summary( LIQUID_ASSETS_MONTHS ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS (raw)" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_w, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Winsorized" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_z, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Standardized (Z)" )
    plot( density( LIQUID_ASSETS_MONTHS$liquid_assets_months_p, na.rm = TRUE ), main = "LIQUID_ASSETS_MONTHS Percentile" )
  }

  return( cbind( df, LIQUID_ASSETS_MONTHS ) )
}
