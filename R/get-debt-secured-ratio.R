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
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param secured_mortgages_notes Secured mortgages and notes payable, EOY.
#'   (On 990: Part X, line 23B; \code{F9_10_LIAB_MTG_NOTE_EOY})
#' @param total_liabilities Total liabilities, EOY.
#'   (On 990: Part X, line 26B; \code{F9_10_LIAB_TOT_EOY})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, imputes zero for NA
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default \code{FALSE}). If \code{TRUE}, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_debt_secured_ratio( df,
   secured_mortgages_notes   = "F9_10_LIAB_MTG_NOTE_EOY",
   total_liabilities         = "F9_10_LIAB_TOT_EOY",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{debt_secured}, \code{debt_secured_w},
#'   \code{debt_secured_z}, \code{debt_secured_p}.
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
                     winsorize  = 0.98,
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

  message( paste0( "Total liabilities equal to zero: ", sum( den == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  den[ den == 0 ] <- NA

  debt_secured <- num / den

  v <- winsorize_var( debt_secured, winsorize )
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
