###---------------------------------------------------
###   SELF SUFFICIENCY RATIO
###---------------------------------------------------

#' @title
#' Self Sufficiency Ratio
#'
#' @description
#' Measures whether program service revenue is sufficient to cover total expenses.
#'
#' **Formula:**
#' ```
#' ssr = program_service_revenue / total_expenses
#' ```
#'
#' **Definitional Range**
#'
#' Bounded below at zero; values above 1.0 are possible (and indicate program revenue
#' exceeds total expenses - a surplus from earned income alone). The typical range for
#' nonprofits is approximately \[0, 1.5\], with most values below 1.0 since most
#' nonprofits depend on some contributed income.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **SSR > 1.0**: Fully self-sufficient from program revenue; characteristic
#'     of mature social enterprises and fee-based service providers.
#'   - **SSR 0.50-1.0**: Majority of costs covered by program revenue; moderate
#'     philanthropy dependence.
#'   - **SSR < 0.25**: Heavily dependent on contributed income; common for advocacy
#'     organizations, arts nonprofits, and grant-funded research entities.
#'   - High SSR is not universally better -- organizations serving low-income
#'     populations often require philanthropic subsidy by design.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param program_service_rev Program service revenue. Accepts one or two column names;
#'   if two are provided they are coalesced with the 990 value taking priority over 990EZ.
#'
#' @param total_expenses Total functional expenses. Accepts one or two column names.
#'
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zp"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_self_sufficiency_ratio( df,
#'   program_service_rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY" ),
#'   total_expenses      = c( "F9_09_EXP_TOT_TOT",      "F9_01_EXP_TOT_CY"      ),
#'   winsorize = 0.98 ,
#'   range     = "zp",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `self_suff`   - self sufficiency ratio (raw)
#'     - `self_suff_w` - winsorized version
#'     - `self_suff_z` - standardized z-score (based on winsorized values)
#'     - `self_suff_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The self-sufficiency ratio (SSR) measures whether an organization's earned program
#' service revenue is sufficient to cover its total expenses. A ratio of 1.0 means
#' the organization could fully sustain itself from program fees and earned income
#' alone, without any reliance on donations, grants, or investment income. Values
#' below 1.0 indicate the degree of dependence on contributed revenue.
#'
#' SSR is particularly important in the nonprofit financial sustainability literature
#' as a measure of earned income dependency and mission-related revenue generation.
#' High SSR organizations (social enterprises, fee-for-service providers) are
#' sometimes considered more financially resilient because earned revenue is more
#' predictable and controllable than philanthropic support.
#'
#' ## Formula variations and their sources
#'
#' Program service revenue / total expenses. Some studies (Young 2007) use total
#' earned income (including membership dues and investment income) in the numerator
#' for a broader self-sufficiency concept. This implementation uses program service
#' revenue only, consistent with the most common definition in the financial
#' vulnerability literature.
#'
#' ## Canonical citations
#'
#'
#'   - Young, D.R. (2007). Financing nonprofits: Putting theory into practice.
#'     AltaMira Press. - Extensive discussion of earned income and self-sufficiency
#'     concepts.
#'   - Weisbrod, B.A. (1998). The nonprofit mission and its financing.
#'     *Journal of Policy Analysis and Management*, 17(2), 165-174. - Examines
#'     the tension between earned income and mission.
#'   - Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the financial
#'     vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 20(4), 445-460.
#'
#'
#' ## Variables used:
#'
#'   - `F9_08_REV_PROG_TOT_TOT`: Program service revenue (`program_service_rev`, 990)
#'   - `F9_01_REV_PROG_TOT_CY`: Program revenue from Part I (`program_service_rev`, 990EZ fallback)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`, 990)
#'   - `F9_01_EXP_TOT_CY`: Total expenses from Part I (`total_expenses`, 990EZ fallback)
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
#' d <- get_self_sufficiency_ratio( df = dat10k )
#' head( d[ , c( "self_suff", "self_suff_w", "self_suff_z", "self_suff_p" ) ] )
#'
#' @export
get_self_sufficiency_ratio <- function( df,
                     program_service_rev = c( "F9_08_REV_PROG_TOT_TOT", "F9_01_REV_PROG_TOT_CY" ),
                     total_expenses      = c( "F9_09_EXP_TOT_TOT",      "F9_01_EXP_TOT_CY"      ),
                     winsorize = 0.98  ,
                     range     = "zp" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, program_service_rev, total_expenses,
                   "program_service_rev", "total_expenses" )

  if ( length( program_service_rev ) > 2 ) stop( "`program_service_rev` must be one or two column names." )
  if ( length( total_expenses )      > 2 ) stop( "`total_expenses` must be one or two column names."      )

  vars <- c( program_service_rev, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  p <- resolve_col( dt, program_service_rev )
  e <- resolve_col( dt, total_expenses )

  nan.count <- sum( e == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total expenses equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  e[ e == 0 ] <- NaN

  ssr <- p / e

  v <- apply_transformations( ssr, winsorize, range )
  SELF_SUFF <- data.frame( self_suff   = v$raw,
                     self_suff_w = v$winsorized,
                     self_suff_z = v$z,
                     self_suff_p = v$pctile )

  if ( summarize ) {
    print( summary( SELF_SUFF ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( SELF_SUFF$self_suff, na.rm = TRUE ), main = "SELF_SUFF (raw)" )
    plot( density( SELF_SUFF$self_suff_w, na.rm = TRUE ), main = "SELF_SUFF Winsorized" )
    plot( density( SELF_SUFF$self_suff_z, na.rm = TRUE ), main = "SELF_SUFF Standardized (Z)" )
    plot( density( SELF_SUFF$self_suff_p, na.rm = TRUE ), main = "SELF_SUFF Percentile" )
  }

  return( cbind( df, SELF_SUFF ) )
}
