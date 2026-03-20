###---------------------------------------------------
###   OPERATING RESERVES RATIO
###---------------------------------------------------

#' @title
#' Operating Reserves Ratio
#'
#' @description
#' Liquid unrestricted net assets available to cover operating expenses, expressed
#' as a multiple of total annual expenses.
#'
#' **Formula:**
#' ```
#' orr = operating_reserves / total_expenses
#'
#' operating_reserves = unrestricted_net_assets - net_fixed_assets
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param unrestricted_net_assets Unrestricted net assets, EOY.
#' @param net_fixed_assets Net land, buildings, and equipment, EOY.
#' @param total_expenses Total functional expenses.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_operating_reserve_ratio( df,
#'   unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
#'   net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
#'   total_expenses          = "F9_09_EXP_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `op_reserve`   - operating reserves ratio (raw)
#'     - `op_reserve_w` - winsorized version
#'     - `op_reserve_z` - standardized z-score (based on winsorized values)
#'     - `op_reserve_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The operating reserve ratio answers a key sustainability question: how many months
#' of operations can the organization fund from its unrestricted liquid assets, excluding
#' illiquid fixed property? It is the recommended primary reserve adequacy metric of the
#' Nonprofit Finance Fund and is widely used in financial health assessments, lender due
#' diligence, and board reporting.
#'
#' Unlike days/months of cash measures ([get_days_cash_operations()],
#' [get_months_cash_operations()]), which count only cash and receivables,
#' the operating reserve ratio starts from the full unrestricted equity base and removes
#' fixed assets - giving a broader picture of the liquid reserves available to sustain
#' operations.
#'
#' ## Formula variations and their sources
#'
#' (Unrestricted net assets - net fixed assets) / (total expenses / 12). This
#' implementation follows the Nonprofit Finance Fund and Zietlow et al. (2007)
#' definition. An alternative adds back mortgage debt to fixed assets
#' ([get_liquid_assets_months()]) to more precisely isolate the unencumbered
#' value of fixed property. Some formulations use unrestricted net assets alone
#' (without subtracting fixed assets), which produces higher values.
#'
#' ## Canonical citations
#'
#'
#'   - Nonprofit Finance Fund. *Operating Reserve Policy Toolkit for Nonprofits*.
#'     - Primary source for the operating reserve ratio definition and benchmarks.
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley.
#'   - Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
#'     Leadership*, 23(3), 281-302. - Empirical analysis of operating reserve levels
#'     across the U.S. nonprofit sector.
#'
#'
#' ## Definitional range
#'
#' Unbounded in both directions when expressed in months. Negative values occur when
#' unrestricted net assets are negative or when net fixed assets exceed unrestricted net
#' assets. Values above 24 months are uncommon for operating organizations and may
#' indicate accumulation beyond what is needed for operating risk management.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - **Less than 1 month**: Acute vulnerability.
#'   - **1-3 months**: Below the commonly recommended minimum.
#'   - **3-6 months**: Generally adequate for most operating nonprofits.
#'   - **6-12 months**: Strong; appropriate for organizations with volatile
#'     revenue, major capital needs, or large fixed cost bases.
#'   - The Nonprofit Finance Fund recommends a minimum of 3 months as a policy
#'     target for most organizations.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_NAFB_UNRESTRICT_EOY`: 
#'     Unrestricted net assets, EOY (`unrestricted_net_assets`)
#'   - `F9_10_ASSET_LAND_BLDG_NET_EOY`: 
#'     Net land, buildings, and equipment (`net_fixed_assets`)
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
#' d <- get_operating_reserve_ratio( df = dat10k )
#' head( d[ , c( "op_reserve", "op_reserve_w", "op_reserve_z", "op_reserve_p" ) ] )
#'
#' @export
get_operating_reserve_ratio <- function( df,
                     unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
                     net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
                     total_expenses          = "F9_09_EXP_TOT_TOT",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  if ( is.null( unrestricted_net_assets ) )
    stop( "`unrestricted_net_assets` cannot be NULL." )
  if ( is.null( net_fixed_assets ) )
    stop( "`net_fixed_assets` cannot be NULL." )
  if ( is.null( total_expenses ) )
    stop( "`total_expenses` cannot be NULL." )

  if ( length( unrestricted_net_assets ) > 2 )
    stop( "`unrestricted_net_assets` must be one or two column names." )
  if ( length( net_fixed_assets ) > 2 )
    stop( "`net_fixed_assets` must be one or two column names." )
  if ( length( total_expenses ) > 2 )
    stop( "`total_expenses` must be one or two column names." )

  all_cols <- c( unrestricted_net_assets, net_fixed_assets, total_expenses )
  vars <- c( unrestricted_net_assets, net_fixed_assets, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  una <- resolve_col( dt, unrestricted_net_assets )
  nfa <- resolve_col( dt, net_fixed_assets )
  exp <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( exp == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  exp[ exp == 0 ] <- NA

  orr <- ( una - nfa ) / exp

  v <- winsorize_var( orr, winsorize )
  OP_RESERVE <- data.frame( op_reserve   = v$raw,
                     op_reserve_w = v$winsorized,
                     op_reserve_z = v$z,
                     op_reserve_p = v$pctile )

  if ( summarize ) {
    print( summary( OP_RESERVE ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( OP_RESERVE$op_reserve, na.rm = TRUE ), main = "OP_RESERVE (raw)" )
    plot( density( OP_RESERVE$op_reserve_w, na.rm = TRUE ), main = "OP_RESERVE Winsorized" )
    plot( density( OP_RESERVE$op_reserve_z, na.rm = TRUE ), main = "OP_RESERVE Standardized (Z)" )
    plot( density( OP_RESERVE$op_reserve_p, na.rm = TRUE ), main = "OP_RESERVE Percentile" )
  }

  return( cbind( df, OP_RESERVE ) )
}
