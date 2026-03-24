###---------------------------------------------------
###   SURPLUS MARGIN
###---------------------------------------------------

#' @title
#' Surplus Margin
#'
#' @description
#' Net surplus or deficit as a share of total revenue.
#'
#' **Formula:**
#' ```
#' sm = revenues_less_expenses / total_revenue
#' ```
#'
#' **Definitional Range**
#'
#' Bounded above at 1.0 (expenses cannot be negative). Bounded below at negative infinity in theory,
#' but in practice the empirical range for nonprofits is approximately \[-0.30, 0.30\]
#' in most years. Values below -0.50 or above 0.50 typically reflect unusual one-time
#' events (large gifts, major write-downs, asset sales).
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **Near zero**: Normal and expected for nonprofits operating close to break-even.
#'   - **0.02-0.07** (2-7%): Commonly considered a healthy surplus range.
#'   - **Below -0.05** for two or more consecutive years: Common vulnerability
#'     threshold (Greenlee & Trussel 2000).
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param revenues_less_expenses Revenues less expenses (net surplus or deficit) for the
#'   current year. Accepts one or two column names; if two are provided they are coalesced
#'   with the 990 value taking priority over 990EZ.
#'
#' @param total_revenue Total revenue. Accepts one or two column names.
#'
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"np"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_surplus_margin_ratio( df,
#'   revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
#'   total_revenue          = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
#'   winsorize = 0.98 ,
#'   range     = "np",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `surplus_margin`   - surplus margin (raw)
#'     - `surplus_margin_w` - winsorized version
#'     - `surplus_margin_z` - standardized z-score (based on winsorized values)
#'     - `surplus_margin_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The surplus margin (also called the operating margin or profit margin) measures
#' what fraction of total revenue remains after all expenses. It is the most direct
#' measure of annual financial performance: positive values indicate a surplus year
#' (more revenue than expenses); negative values indicate a deficit year.
#'
#' Unlike [get_return_assets_ratio()] (which scales by assets) or
#' [get_return_netassets_ratio()] (which scales by equity), the surplus
#' margin scales by revenue - answering the question: "of each dollar raised, how
#' much is left after all expenses?" It is the nonprofit equivalent of the commercial
#' net profit margin.
#'
#' ## Formula variations and their sources
#'
#' Revenues less expenses / total revenue. The numerator uses the Part I summary
#' line (`F9_01_EXP_REV_LESS_EXP_CY`), which is available on both 990 and 990EZ
#' and may include adjustments not captured in a simple revenue-minus-expenses
#' calculation from Parts VIII and IX. An alternative uses (Part VIII total revenue -
#' Part IX total expenses) / Part VIII revenue, which is more precise for full-990
#' filers but unavailable for 990EZ filers. The Part I version is used here for
#' maximum coverage.
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
#'   - Tuckman, H.P. & Chang, C.F. (1992). Nonprofit equity: A behavioral model and
#'     its policy implications. *Journal of Policy Analysis and Management*, 11(1),
#'     76-87.
#'
#'
#' ## Variables used:
#'
#'   - `F9_01_EXP_REV_LESS_EXP_CY`: 
#'     Revenues less expenses, current year (`revenues_less_expenses`)
#'   - `F9_08_REV_TOT_TOT`: Total revenue from Part VIII (`total_revenue`, 990)
#'   - `F9_01_REV_TOT_CY`: Total revenue from Part I (`total_revenue`, 990EZ fallback)
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
#' d <- get_surplus_margin_ratio( df = dat10k )
#' head( d[ , c( "surplus_margin", "surplus_margin_w", "surplus_margin_z", "surplus_margin_p" ) ] )
#'
#' @export
get_surplus_margin_ratio <- function( df,
                    revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
                    total_revenue          = c( "F9_08_REV_TOT_TOT",
                                                "F9_01_REV_TOT_CY" ),
                    winsorize = 0.98  ,
                     range     = "np" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, revenues_less_expenses, total_revenue,
                   "revenues_less_expenses", "total_revenue" )

  if ( length( revenues_less_expenses ) > 2 )
    stop( "`revenues_less_expenses` must be one or two column names." )
  if ( length( total_revenue ) > 2 )
    stop( "`total_revenue` must be one or two column names." )

  all_cols <- c( revenues_less_expenses, total_revenue )
  vars <- c( revenues_less_expenses, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  s <- resolve_col( dt, revenues_less_expenses )
  r <- resolve_col( dt, total_revenue )

  nan.count <- sum( r == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total revenue equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  r[ r == 0 ] <- NaN

  sm <- s / r

  v <- apply_transformations( sm, winsorize, range )
  SURPLUS_MARGIN <- data.frame( surplus_margin   = v$raw,
                    surplus_margin_w = v$winsorized,
                    surplus_margin_z = v$z,
                    surplus_margin_p = v$pctile )

  if ( summarize ) {
    print( summary( SURPLUS_MARGIN ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( SURPLUS_MARGIN$surplus_margin, na.rm = TRUE ), main = "SURPLUS_MARGIN (raw)" )
    plot( density( SURPLUS_MARGIN$surplus_margin_w, na.rm = TRUE ), main = "SURPLUS_MARGIN Winsorized" )
    plot( density( SURPLUS_MARGIN$surplus_margin_z, na.rm = TRUE ), main = "SURPLUS_MARGIN Standardized (Z)" )
    plot( density( SURPLUS_MARGIN$surplus_margin_p, na.rm = TRUE ), main = "SURPLUS_MARGIN Percentile" )
  }

  return( cbind( df, SURPLUS_MARGIN ) )
}
