###---------------------------------------------------
###   SECURED DEBT RATIO
###---------------------------------------------------

#' @title
#' Secured Debt Ratio
#'
#' @description
#' Secured mortgages and notes payable as a share of total liabilities.
#'
#' **Formula:**
#' ```
#' debt_secured = secured_mortgages_notes / total_liabilities
#' ```
#'
#' **Definitional Range**
#'
#' Bounded \[0, 1\]. Most nonprofits have zero secured debt; capital-intensive
#' organizations (housing, healthcare, higher education) can show values above 0.70.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - No standard benchmark. Most informative when tracked over time or compared
#'     within subsectors with similar capital structures.
#'   - For organizations with high secured debt ratios, lenders will scrutinize
#'     the collateral value and debt service coverage ratio.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param secured_mortgages_notes Secured mortgages and notes payable, EOY.
#'
#' @param total_liabilities Total liabilities, EOY.
#'
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zo"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#' @details
#' ## Primary uses and key insights
#'
#' The secured debt ratio measures what share of total liabilities consists of
#' mortgages and notes payable secured by collateral. It is a liability composition
#' indicator: a high ratio means the organization's debt is predominantly secured
#' (usually real estate mortgages), while a low ratio means most debt is unsecured.
#'
#' This metric is useful for understanding the nature of an organization's leverage,
#' since secured debt typically has lower interest rates but pledges specific assets
#' as collateral, limiting flexibility. It is particularly relevant for housing
#' nonprofits, healthcare organizations, and educational institutions.
#'
#' ## Formula variations and their sources
#'
#' Secured mortgages and notes payable (Part X line 23B) / total liabilities (line 26B).
#' The complement is approximately [get_debt_unsecured_ratio()] (line 24B /
#' line 26B), though the two do not sum to 1.0 because there are other liability categories.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good: Executive
#'     compensation in nonprofit organizations. *Policy and Society*, 20(4), 94-112.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_LIAB_MTG_NOTE_EOY`: Secured mortgages and notes payable, EOY (`secured_mortgages_notes`)
#'   - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`total_liabilities`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_debt_secured_ratio( df,
#'   secured_mortgages_notes   = "F9_10_LIAB_MTG_NOTE_EOY",
#'   total_liabilities         = "F9_10_LIAB_TOT_EOY",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four columns appended:
#'   `debt_secured`, `debt_secured_w`,
#'   `debt_secured_z`, `debt_secured_p`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_debt_secured_ratio( df = dat10k )
#' head( d[ , c( "debt_secured", "debt_secured_w", "debt_secured_z", "debt_secured_p" ) ] )
#'
#' @export
get_debt_secured_ratio <- function( df,
                     secured_mortgages_notes   = "F9_10_LIAB_MTG_NOTE_EOY",
                     total_liabilities         = "F9_10_LIAB_TOT_EOY",
                     winsorize  = 0.98 ,
                     range     = "zo" ,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, secured_mortgages_notes, total_liabilities,
                   "secured_mortgages_notes", "total_liabilities" )

  vars <- c( secured_mortgages_notes, total_liabilities )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, secured_mortgages_notes )
  den <- resolve_col( dt, total_liabilities )

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total liabilities equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  debt_secured <- num / den

  v <- apply_transformations( debt_secured, winsorize, range )
  DEBT_SECURED <- data.frame(
    debt_secured   = v$raw,
    debt_secured_w = v$winsorized,
    debt_secured_z = v$z,
    debt_secured_p = v$pctile )

  if ( summarize ) {
    print( summary( DEBT_SECURED ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DEBT_SECURED$debt_secured,   na.rm = TRUE ), main = "DEBT_SECURED (raw)" )
    plot( density( DEBT_SECURED$debt_secured_w, na.rm = TRUE ), main = "DEBT_SECURED Winsorized" )
    plot( density( DEBT_SECURED$debt_secured_z, na.rm = TRUE ), main = "DEBT_SECURED Standardized (Z)" )
    plot( density( DEBT_SECURED$debt_secured_p, na.rm = TRUE ), main = "DEBT_SECURED Percentile" )
  }

  return( cbind( df, DEBT_SECURED ) )
}
