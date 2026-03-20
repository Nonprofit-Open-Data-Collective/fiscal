###---------------------------------------------------
###   GOVERNMENT GRANT RATIO
###---------------------------------------------------

#' @title
#' Government Grant Ratio
#'
#' @description
#' Share of total revenue originating from government grants.
#'
#' **Formula:**
#' ```
#' ggr = government_grants / total_revenue
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param government_grants Government grants and contributions.
#' @param total_revenue Total revenue.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_grants_govt_ratio( df,
#'   government_grants = "F9_08_REV_CONTR_GOVT_GRANT",
#'   total_revenue     = "F9_08_REV_TOT_TOT",
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `grants_govt`   - government grant ratio (raw)
#'     - `grants_govt_w` - winsorized version
#'     - `grants_govt_z` - standardized z-score (based on winsorized values)
#'     - `grants_govt_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' The government grant ratio measures what fraction of total revenue comes from
#' government grants and contributions. It is a revenue diversification indicator
#' specifically focused on public sector dependency. High ratios indicate heavy
#' reliance on government funding, which creates exposure to policy changes, budget
#' cycles, and political priorities that can suddenly alter the funding environment.
#'
#' Government funding also typically comes with extensive compliance, reporting, and
#' audit requirements that increase administrative costs, making the government grant
#' ratio a useful input to overhead analysis. This ratio is a core component of revenue
#' concentration and diversification studies in the nonprofit literature.
#'
#' ## Formula variations and their sources
#'
#' Government grants (Part VIII line 1e) / total revenue (Part VIII line 12A). Some
#' studies also include government contracts reported as program service revenue
#' (Part VIII line 2), which would capture government fees-for-service alongside
#' grants, but these cannot be separated from private program revenue on the 990.
#'
#' ## Canonical citations
#'
#'
#'   - Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among nonprofits.
#'     *VOLUNTAS*, 5(3), 273-290. - Foundational paper on revenue concentration risk.
#'   - Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in nonprofit
#'     organizations: Does it lead to financial stability? *Journal of Public
#'     Administration Research and Theory*, 19(4), 947-966.
#'   - Froelich, K.A. (1999). Diversification of revenue strategies: Evolving
#'     resource dependence in nonprofit organizations. *Nonprofit and Voluntary
#'     Sector Quarterly*, 28(3), 246-268.
#'
#'
#' ## Definitional range
#'
#' Bounded \[0, 1\]. Zero means no government funding; one means all revenue comes
#' from government sources. The empirical distribution varies widely by subsector:
#' social service organizations often show ratios above 0.50; arts organizations
#' typically below 0.20.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - Values above 0.50 indicate majority government-funded organizations, which
#'     face heightened vulnerability to government budget changes.
#'   - Revenue diversification theory (Chang & Tuckman 1994) suggests maintaining
#'     no single source above 0.33-0.50 for optimal stability, though this depends on
#'     the predictability of each revenue source.
#'
#'
#' ## Variables used:
#'
#'   - `F9_08_REV_CONTR_GOVT_GRANT`: 
#'     Government grants and contributions (`government_grants`)
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
#' d <- get_grants_govt_ratio( df = dat10k )
#' head( d[ , c( "grants_govt", "grants_govt_w", "grants_govt_z", "grants_govt_p" ) ] )
#'
#' @export
get_grants_govt_ratio <- function( df,
                     government_grants = "F9_08_REV_CONTR_GOVT_GRANT",
                     total_revenue     = "F9_08_REV_TOT_TOT",
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, government_grants, total_revenue, "government_grants", "total_revenue" )

  if ( length( government_grants ) > 2 ) stop( "`government_grants` must be one or two column names." )
  if ( length( total_revenue )     > 2 ) stop( "`total_revenue` must be one or two column names."     )

  vars <- c( government_grants, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  g <- resolve_col( dt, government_grants )
  r <- resolve_col( dt, total_revenue )

  message( paste0( "Total revenue equal to zero: ", sum( r == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  r[ r == 0 ] <- NA

  ggr <- g / r

  v <- winsorize_var( ggr, winsorize )
  GRANTS_GOVT <- data.frame( grants_govt   = v$raw,
                     grants_govt_w = v$winsorized,
                     grants_govt_z = v$z,
                     grants_govt_p = v$pctile )

  if ( summarize ) {
    print( summary( GRANTS_GOVT ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( GRANTS_GOVT$grants_govt, na.rm = TRUE ), main = "GRANTS_GOVT (raw)" )
    plot( density( GRANTS_GOVT$grants_govt_w, na.rm = TRUE ), main = "GRANTS_GOVT Winsorized" )
    plot( density( GRANTS_GOVT$grants_govt_z, na.rm = TRUE ), main = "GRANTS_GOVT Standardized (Z)" )
    plot( density( GRANTS_GOVT$grants_govt_p, na.rm = TRUE ), main = "GRANTS_GOVT Percentile" )
  }

  return( cbind( df, GRANTS_GOVT ) )
}
