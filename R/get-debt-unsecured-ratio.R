###---------------------------------------------------
###   UNSECURED DEBT RATIO
###---------------------------------------------------

#' @title
#' Unsecured Debt Ratio
#'
#' @description
#' Unsecured notes and loans payable as a share of total liabilities.
#'
#' **Formula:**
#' ```
#' debt_unsecured = unsecured_notes_loans / total_liabilities
#' ```
#'
#' **Definitional Range**
#'
#' Bounded \[0, 1\]. Most nonprofits carry no unsecured notes, so the distribution
#' is highly concentrated at zero with a long right tail.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - No established benchmark. Presence of unsecured debt is not inherently
#'     negative -- many healthy nonprofits use lines of credit for cash flow
#'     management -- but a large and growing ratio warrants scrutiny.
#'   - Trend analysis is more informative than a single year's value.
#'
#' **Calculated For:** 990 + 990EZ filers.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param unsecured_notes_loans Unsecured notes and loans payable, EOY.
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
#' The unsecured debt ratio measures what share of total liabilities consists of
#' unsecured notes and loans payable (Part X line 24B), typically including lines
#' of credit and unsecured bank loans. Used alongside [get_debt_secured_ratio()],
#' it provides a decomposition of the liability structure.
#'
#' ## Formula variations and their sources
#'
#' Unsecured notes and loans payable (Part X line 24B) / total liabilities (line 26B).
#' A direct read from two 990 balance sheet fields.
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
#'     and Society*, 20(4), 94-112.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_LIAB_NOTE_UNSEC_EOY`: 
#'     Unsecured notes and loans payable, EOY (`unsecured_notes_loans`)
#'   - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`total_liabilities`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_debt_unsecured_ratio( df,
#'   unsecured_notes_loans     = "F9_10_LIAB_NOTE_UNSEC_EOY",
#'   total_liabilities         = "F9_10_LIAB_TOT_EOY",
#'   winsorize  = 0.98 ,
#'   range     = "zo",
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original `data.frame` with four columns appended:
#'   `debt_unsecured`, `debt_unsecured_w`,
#'   `debt_unsecured_z`, `debt_unsecured_p`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_debt_unsecured_ratio( df = dat10k )
#' head( d[ , c( "debt_unsecured", "debt_unsecured_w", "debt_unsecured_z", "debt_unsecured_p" ) ] )
#'
#' @export
get_debt_unsecured_ratio <- function( df,
                     unsecured_notes_loans     = "F9_10_LIAB_NOTE_UNSEC_EOY",
                     total_liabilities         = "F9_10_LIAB_TOT_EOY",
                     winsorize  = 0.98 ,
                     range     = "zo" ,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  validate_inputs( winsorize, unsecured_notes_loans, total_liabilities,
                   "unsecured_notes_loans", "total_liabilities" )

  vars <- c( unsecured_notes_loans, total_liabilities )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  num <- resolve_col( dt, unsecured_notes_loans )
  den <- resolve_col( dt, total_liabilities )

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Total liabilities equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  debt_unsecured <- num / den

  v <- apply_transformations( debt_unsecured, winsorize, range )
  DEBT_UNSECURED <- data.frame(
    debt_unsecured   = v$raw,
    debt_unsecured_w = v$winsorized,
    debt_unsecured_z = v$z,
    debt_unsecured_p = v$pctile )

  if ( summarize ) {
    print( summary( DEBT_UNSECURED ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DEBT_UNSECURED$debt_unsecured,   na.rm = TRUE ), main = "DEBT_UNSECURED (raw)" )
    plot( density( DEBT_UNSECURED$debt_unsecured_w, na.rm = TRUE ), main = "DEBT_UNSECURED Winsorized" )
    plot( density( DEBT_UNSECURED$debt_unsecured_z, na.rm = TRUE ), main = "DEBT_UNSECURED Standardized (Z)" )
    plot( density( DEBT_UNSECURED$debt_unsecured_p, na.rm = TRUE ), main = "DEBT_UNSECURED Percentile" )
  }

  return( cbind( df, DEBT_UNSECURED ) )
}
