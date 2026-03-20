###---------------------------------------------------
###   FUNDRAISING EFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Fundraising Efficiency Ratio
#'
#' @description
#' Cost of raising one dollar of contributions; measures how efficiently fundraising
#' expenses generate donated revenue.
#'
#' **Formula:**
#' ```
#' fer = fundraising_expenses / total_contributions
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param fundraising_expenses Fundraising expenses.
#' @param total_contributions Total contributions received.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_fundraising_efficiency_ratio( df,
#'   fundraising_expenses = "F9_09_EXP_TOT_FUNDR",
#'   total_contributions  = "F9_08_REV_CONTR_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `fundr_eff`   - fundraising efficiency ratio (raw)
#'     - `fundr_eff_w` - winsorized version
#'     - `fundr_eff_z` - standardized z-score (based on winsorized values)
#'     - `fundr_eff_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The fundraising efficiency ratio measures the cost of raising one dollar of
#' contributions. A ratio of 0.20 means the organization spends 20 cents to raise
#' each dollar - a common benchmark in the charity watchdog literature. Lower values
#' indicate more efficient fundraising; higher values indicate more costly fundraising
#' relative to the contributions raised.
#'
#' This metric should be interpreted alongside the fundraising strategy: organizations
#' investing in major gift programs or capital campaigns may show temporarily high
#' ratios that reflect future revenue not yet received. Mature direct mail programs
#' with large donor databases tend to show very low ratios.
#'
#' ## Formula variations and their sources
#'
#' Fundraising expenses (Part IX line 25D) / total contributions (Part VIII line 1h).
#' An alternative uses total revenue rather than contributions in the denominator,
#' which produces a lower ratio but conflates earned and contributed income. The
#' contributions-only denominator is the most direct measure of fundraising productivity.
#'
#' Note: contributions (Part VIII 1h) include grants and federated campaign revenue
#' alongside individual gifts, which may inflate the denominator for organizations
#' receiving large government or foundation grants with minimal fundraising cost.
#'
#' ## Canonical citations
#'
#'
#'   - Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial vulnerability
#'     of charitable organizations. *Nonprofit Management and Leadership*, 11(2),
#'     199-210.
#'   - Charity Navigator. *Financial Health Methodology*. charitynavigator.org.
#'     - Uses fundraising efficiency as a rating factor (target: 10 cents per dollar or less).
#'   - Hager, M.A. (2001). Financial vulnerability among arts organizations.
#'     *Nonprofit and Voluntary Sector Quarterly*, 30(2), 376-392.
#'
#'
#' ## Definitional range
#'
#' Bounded below at zero; values above 1.0 (spending more than a dollar to raise a
#' dollar) are possible, particularly for young organizations building donor bases or
#' those in difficult fundraising environments. The typical range is approximately
#' \[0, 0.50\] for most operating nonprofits.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - **10 cents per dollar or less (ratio <= 0.10)**: Charity Navigator threshold
#'     for a full fundraising efficiency score.
#'   - **35 cents per dollar or less**: BBB Wise Giving Alliance standard.
#'   - **Above 50 cents**: Commonly flagged as inefficient; may indicate
#'     declining donor base or heavy reliance on expensive acquisition channels.
#'   - **Zero**: Organizations with no fundraising expenses but positive
#'     contributions (common for government-funded nonprofits) show a ratio of zero.
#'
#'
#' ## Variables used:
#'
#'   - `F9_09_EXP_TOT_FUNDR`: Total fundraising expenses (`fundraising_expenses`)
#'   - `F9_08_REV_CONTR_TOT`: Total contributions received (`total_contributions`)
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
#' d <- get_fundraising_efficiency_ratio( df = dat10k )
#' head( d[ , c( "fundr_eff", "fundr_eff_w", "fundr_eff_z", "fundr_eff_p" ) ] )
#'
#' @export
get_fundraising_efficiency_ratio <- function( df,
                     fundraising_expenses = "F9_09_EXP_TOT_FUNDR",
                     total_contributions  = "F9_08_REV_CONTR_TOT",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, fundraising_expenses, total_contributions,
                   "fundraising_expenses", "total_contributions" )

  if ( length( fundraising_expenses ) > 2 )
    stop( "`fundraising_expenses` must be one or two column names." )
  if ( length( total_contributions ) > 2 )
    stop( "`total_contributions` must be one or two column names." )

  vars <- c( fundraising_expenses, total_contributions )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  f <- resolve_col( dt, fundraising_expenses )
  c <- resolve_col( dt, total_contributions )

  message( paste0( "Total contributions equal to zero: ", sum( c == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  c[ c == 0 ] <- NA

  fer <- f / c

  v <- winsorize_var( fer, winsorize )
  FUNDR_EFF <- data.frame( fundr_eff   = v$raw,
                     fundr_eff_w = v$winsorized,
                     fundr_eff_z = v$z,
                     fundr_eff_p = v$pctile )

  if ( summarize ) {
    print( summary( FUNDR_EFF ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( FUNDR_EFF$fundr_eff, na.rm = TRUE ), main = "FUNDR_EFF (raw)" )
    plot( density( FUNDR_EFF$fundr_eff_w, na.rm = TRUE ), main = "FUNDR_EFF Winsorized" )
    plot( density( FUNDR_EFF$fundr_eff_z, na.rm = TRUE ), main = "FUNDR_EFF Standardized (Z)" )
    plot( density( FUNDR_EFF$fundr_eff_p, na.rm = TRUE ), main = "FUNDR_EFF Percentile" )
  }

  return( cbind( df, FUNDR_EFF ) )
}
