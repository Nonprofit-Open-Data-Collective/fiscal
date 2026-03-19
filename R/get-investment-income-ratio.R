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
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param invest_income Investment income. (On 990: Part VIII, line 3; \code{F9_08_REV_OTH_INVEST_INCOME_TOT})
#' @param bond_proceeds Tax-exempt bond proceeds. (On 990: Part VIII, line 7a; \code{F9_08_REV_OTH_INVEST_BOND_TOT})
#' @param rent_income Gross rents from personal property. (On 990: Part VIII, line 6b; \code{F9_08_REV_OTH_RENT_GRO_PERS})
#' @param asset_sale_income Net gain from sales of assets other than inventory. (On 990: Part VIII, line 7d; \code{F9_08_REV_OTH_SALE_ASSET_OTH})
#' @param total_revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
#' @param numerator Optional. A pre-aggregated column for investment income. Cannot be
#'   combined with the individual component arguments.
#' @param denominator Optional. A pre-aggregated column for total revenue. Cannot be
#'   combined with \code{total_revenue}.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_investment_income_ratio( df,
#'   invest_income    = "F9_08_REV_OTH_INVEST_INCOME_TOT",
#'   bond_proceeds    = "F9_08_REV_OTH_INVEST_BOND_TOT",
#'   rent_income      = "F9_08_REV_OTH_RENT_GRO_PERS",
#'   asset_sale_income= "F9_08_REV_OTH_SALE_ASSET_OTH",
#'   total_revenue    = "F9_08_REV_TOT_TOT",
#'   numerator = NULL, denominator = NULL, winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{invest_income}   — investment income dependency ratio (raw)
#'     \item \code{invest_income_w} — winsorized version
#'     \item \code{invest_income_z} — standardized z-score (based on winsorized values)
#'     \item \code{invest_income_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' \strong{Primary uses and key insights}
#'
#' The investment income dependency ratio measures the share of total revenue from
#' investment-related sources: interest, dividend income, rental income, bond proceeds,
#' and proceeds from asset sales. It captures financial portfolio dependency — the
#' degree to which the organization's budget relies on endowment returns, rental
#' properties, or investment gains.
#'
#' A high ratio may be a sign of financial maturity (large endowment generating
#' returns) or financial risk (dependency on volatile investment markets). During
#' periods of market decline or low interest rates, organizations with high investment
#' income ratios face greater revenue volatility.
#'
#' \strong{Formula variations and their sources}
#'
#' (Investment income + bond income + rental income + asset sale income) / total
#' revenue (Part VIII lines 3 + 4 + 6a(ii) + 7d / line 12A). Note that this combines
#' recurring investment income with potentially one-time asset sale proceeds; some
#' analysts separate these. The asset sale proceeds (line 7d) are particularly
#' volatile and may distort the ratio in years with large asset disposals.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     \emph{VOLUNTAS}, 5(3), 273-290.
#'   \item Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in nonprofit
#'     organizations. \emph{Journal of Public Administration Research and Theory},
#'     19(4), 947-966.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded \[0, 1\] in most years, but values above 1.0 are possible in years with
#' large asset sale gains. Values below zero can result from asset sale losses or
#' negative investment returns.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item Values above 0.30 indicate significant financial asset dependency and
#'     warrant monitoring of portfolio performance.
#'   \item Large year-over-year swings in this ratio often reflect one-time asset
#'     transactions rather than structural revenue changes.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_08_REV_OTH_INVEST_INCOME_TOT}: Investment income (\code{invest_income})
#'   \item \code{F9_08_REV_OTH_INVEST_BOND_TOT}: Income from bond proceeds (\code{bond_proceeds})
#'   \item \code{F9_08_REV_OTH_RENT_GRO_PERS}: Gross rental income (\code{rent_income})
#'   \item \code{F9_08_REV_OTH_SALE_ASSET_OTH}: Net gain from asset sales (\code{asset_sale_income})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue (\code{total_revenue})
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
                      winsorize = 0.98 ,
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

  message( paste0( "Total revenue equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  iidr <- num / den

  v <- winsorize_var( iidr, winsorize )
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
