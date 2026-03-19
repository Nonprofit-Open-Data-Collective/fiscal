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
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param government_grants Government grants and contributions. (On 990: Part VIII, line 1e; \code{F9_08_REV_CONTR_GOVT_GRANT})
#' @param total_revenue Total revenue. (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT})
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
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{ggr}   — government grant ratio (raw)
#'     \item \code{ggr_w} — winsorized version
#'     \item \code{ggr_z} — standardized z-score (based on winsorized values)
#'     \item \code{ggr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The government grant ratio measures the proportion of total revenue derived from
#' government sources. High values indicate heavy reliance on public funding, which may
#' create vulnerability if that funding is reduced or eliminated.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_08_REV_CONTR_GOVT_GRANT}: Government grants and contributions (\code{government_grants})
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
