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
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param unsecured_notes_loans Unsecured notes and loans payable, EOY.
#'   (On 990: Part X, line 24B; \code{F9_10_LIAB_NOTE_UNSEC_EOY})
#' @param total_liabilities Total liabilities, EOY.
#'   (On 990: Part X, line 26B; \code{F9_10_LIAB_TOT_EOY})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_debt_unsecured_ratio( df,
   unsecured_notes_loans     = "F9_10_LIAB_NOTE_UNSEC_EOY",
   total_liabilities         = "F9_10_LIAB_TOT_EOY",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{debt_unsecured}, \code{debt_unsecured_w},
#'   \code{debt_unsecured_z}, \code{debt_unsecured_p}.
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
                     winsorize  = 0.98,
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

  message( paste0( "Total liabilities equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  debt_unsecured <- num / den

  v <- winsorize_var( debt_unsecured, winsorize )
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
