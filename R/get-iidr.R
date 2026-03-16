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
#' get_iidr( df,
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
#'     \item \code{iidr}   — investment income dependency ratio (raw)
#'     \item \code{iidr_w} — winsorized version
#'     \item \code{iidr_z} — standardized z-score (based on winsorized values)
#'     \item \code{iidr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The investment income dependency ratio measures the share of total revenue derived from
#' investment-related sources. Higher values may indicate a financially stable endowment
#' base but also greater exposure to market fluctuations.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_OTH_INVEST_INCOME_TOT}: Investment income (\code{invest_income})
#'   \item \code{F9_08_REV_OTH_INVEST_BOND_TOT}: Bond proceeds (\code{bond_proceeds})
#'   \item \code{F9_08_REV_OTH_RENT_GRO_PERS}: Rental income (\code{rent_income})
#'   \item \code{F9_08_REV_OTH_SALE_ASSET_OTH}: Asset sale income (\code{asset_sale_income})
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
#' d <- get_iidr( df = dat10k )
#' head( d[ , c( "iidr", "iidr_w", "iidr_z", "iidr_p" ) ] )
#'
#' @export
get_iidr <- function( df,
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
  IIDR <- data.frame( iidr   = v$raw,
                      iidr_w = v$winsorized,
                      iidr_z = v$z,
                      iidr_p = v$pctile )

  if ( summarize ) {
    print( summary( IIDR ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( IIDR$iidr, na.rm = TRUE ), main = "IIDR (raw)" )
    plot( density( IIDR$iidr_w, na.rm = TRUE ), main = "IIDR Winsorized" )
    plot( density( IIDR$iidr_z, na.rm = TRUE ), main = "IIDR Standardized (Z)" )
    plot( density( IIDR$iidr_p, na.rm = TRUE ), main = "IIDR Percentile" )
  }

  return( cbind( df, IIDR ) )
}
