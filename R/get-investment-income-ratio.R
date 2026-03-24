###---------------------------------------------------
###   INVESTMENT INCOME DEPENDENCY RATIO
###---------------------------------------------------

#' @title
#' Investment Income Dependency Ratio
#'
#' @description
#' Measures reliance on investment-related revenue as a share of total revenue.
#'
#' **Formula:**
#' ```
#' iidr = investment_income / total_revenue
#'
#' investment_income = invest_income + bond_proceeds + rent_income + asset_sale_income
#' ```
#'
#' **Definitional Range**
#'
#' Bounded \[0, 1\] in most years, but values above 1.0 are possible in years with
#' large asset sale gains. Values below zero can result from asset sale losses or
#' negative investment returns.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - Values above 0.30 indicate significant financial asset dependency and warrant
#'     monitoring of portfolio performance.
#'   - Large year-over-year swings often reflect one-time asset transactions rather
#'     than structural revenue changes.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param invest_income Investment income.
#' @param bond_proceeds Tax-exempt bond proceeds.
#' @param rent_income Gross rents from personal property.
#' @param asset_sale_income Net gain from sales of assets other than inventory.
#' @param total_revenue Total revenue.
#' @param numerator Optional. A pre-aggregated column for investment income. Cannot be
#'   combined with the individual component arguments.
#' @param denominator Optional. A pre-aggregated column for total revenue. Cannot be
#'   combined with `total_revenue`.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"np"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_investment_income_ratio( df,
#'   invest_income    = "F9_08_REV_OTH_INVEST_INCOME_TOT",
#'   bond_proceeds    = "F9_08_REV_OTH_INVEST_BOND_TOT",
#'   rent_income      = "F9_08_REV_OTH_RENT_GRO_PERS",
#'   asset_sale_income= "F9_08_REV_OTH_SALE_ASSET_OTH",
#'   total_revenue    = "F9_08_REV_TOT_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98 ,
#'   range     = "np",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `invest_income`   - investment income dependency ratio (raw)
#'     - `invest_income_w` - winsorized version
#'     - `invest_income_z` - standardized z-score (based on winsorized values)
#'     - `invest_income_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The investment income dependency ratio measures the share of total revenue from
#' investment-related sources: interest, dividend income, rental income, bond proceeds,
#' and proceeds from asset sales. It captures financial portfolio dependency - the
#' degree to which the organization's budget relies on endowment returns, rental
#' properties, or investment gains.
#'
#' A high ratio may be a sign of financial maturity (large endowment generating
#' returns) or financial risk (dependency on volatile investment markets). During
#' periods of market decline or low interest rates, organizations with high investment
#' income ratios face greater revenue volatility.
#'
#' ## Formula variations and their sources
#'
#' (Investment income + bond income + rental income + asset sale income) / total
#' revenue (Part VIII lines 3 + 4 + 6a(ii) + 7d / line 12A). Note that this combines
#' recurring investment income with potentially one-time asset sale proceeds; some
#' analysts separate these. The asset sale proceeds (line 7d) are particularly
#' volatile and may distort the ratio in years with large asset disposals.
#'
#' ## Canonical citations
#'
#'
#'   - Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     *VOLUNTAS*, 5(3), 273-290.
#'   - Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in nonprofit
#'     organizations. *Journal of Public Administration Research and Theory*,
#'     19(4), 947-966.
#'
#'
#' ## Variables used:
#'
#'   - `F9_08_REV_OTH_INVEST_INCOME_TOT`: 
#'     Investment income (`invest_income`)
#'   - `F9_08_REV_OTH_INVEST_BOND_TOT`: 
#'     Income from bond proceeds (`bond_proceeds`)
#'   - `F9_08_REV_OTH_RENT_GRO_PERS`: 
#'     Gross rental income (`rent_income`)
#'   - `F9_08_REV_OTH_SALE_ASSET_OTH`: 
#'     Net gain from asset sales (`asset_sale_income`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)
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
#' d <- get_investment_income_ratio( df = dat10k )
#' head( d[ , c( "invest_income", "invest_income_w", "invest_income_z", "invest_income_p" ) ] )
#'
#' @export
get_investment_income_ratio <- function( df,
                      invest_income     = "F9_08_REV_OTH_INVEST_INCOME_TOT",
                      bond_proceeds     = "F9_08_REV_OTH_INVEST_BOND_TOT",
                      rent_income       = "F9_08_REV_OTH_RENT_GRO_PERS",
                      asset_sale_income = "F9_08_REV_OTH_SALE_ASSET_OTH",
                      total_revenue     = "F9_08_REV_TOT_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98  ,
                     range     = "np" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  using_component_num <- !is.null( invest_income ) | !is.null( bond_proceeds ) |
                         !is.null( rent_income ) | !is.null( asset_sale_income )
  using_component_den <- !is.null( total_revenue )

  if ( !is.null( numerator ) & using_component_num ) {
    stop( "Supply either `numerator` OR the individual component arguments, not both." )
  }
  if ( !is.null( denominator ) & using_component_den ) {
    stop( "Supply either `denominator` OR `total_revenue`, not both." )
  }
  if ( is.null( numerator ) & !using_component_num ) {
    stop( "No numerator specified. Supply `numerator` or the individual investment columns." )
  }
  if ( is.null( denominator ) & !using_component_den ) {
    stop( "No denominator specified. Supply `denominator` or `total_revenue`." )
  }

  all_cols <- c( invest_income, bond_proceeds, rent_income, asset_sale_income,
                 total_revenue, numerator, denominator )
  vars <- c( invest_income, bond_proceeds, rent_income, asset_sale_income, total_revenue, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    num <- dt[[ invest_income ]] + dt[[ bond_proceeds ]] +
           dt[[ rent_income ]] + dt[[ asset_sale_income ]]
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]]
  } else {
    den <- dt[[ total_revenue ]]
  }

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total revenue equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  iidr <- num / den

  v <- apply_transformations( iidr, winsorize, range )
  INVEST_INCOME <- data.frame( invest_income   = v$raw,
                      invest_income_w = v$winsorized,
                      invest_income_z = v$z,
                      invest_income_p = v$pctile )

  if ( summarize ) {
    print( summary( INVEST_INCOME ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( INVEST_INCOME$invest_income, na.rm = TRUE ), main = "INVEST_INCOME (raw)" )
    plot( density( INVEST_INCOME$invest_income_w, na.rm = TRUE ), main = "INVEST_INCOME Winsorized" )
    plot( density( INVEST_INCOME$invest_income_z, na.rm = TRUE ), main = "INVEST_INCOME Standardized (Z)" )
    plot( density( INVEST_INCOME$invest_income_p, na.rm = TRUE ), main = "INVEST_INCOME Percentile" )
  }

  return( cbind( df, INVEST_INCOME ) )
}
