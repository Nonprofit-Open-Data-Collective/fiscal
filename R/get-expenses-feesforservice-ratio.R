###---------------------------------------------------
###   FEES FOR SERVICES EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Fees for Services Expense Ratio
#'
#' @description
#' Total fees paid for outside services (management, legal, accounting, lobbying,
#' professional fundraising, investment management, and other) as a share of total
#' functional expenses.
#'
#' **Formula:**
#' ```
#' expenses_feesforservice = ( mgmt_fees + legal_fees + accounting_fees
#'                             + lobbying_fees + prof_fundraising_fees
#'                             + investment_mgmt_fees + other_fees )
#'                           / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param mgmt_fees Management fees (total). (On 990: Part IX, line 11a Col A; \code{F9_09_EXP_FEE_SVC_MGMT_TOT})
#' @param legal_fees Legal fees (total). (On 990: Part IX, line 11b Col A; \code{F9_09_EXP_FEE_SVC_LEGAL_TOT})
#' @param accounting_fees Accounting fees (total). (On 990: Part IX, line 11c Col A; \code{F9_09_EXP_FEE_SVC_ACC_TOT})
#' @param lobbying_fees Lobbying fees (total). (On 990: Part IX, line 11d Col A; \code{F9_09_EXP_FEE_SVC_LOB_TOT})
#' @param prof_fundraising_fees Professional fundraising fees (total). (On 990: Part IX, line 11e Col A; \code{F9_09_EXP_FEE_SVC_FUNDR_TOT})
#' @param investment_mgmt_fees Investment management fees (total). (On 990: Part IX, line 11f Col A; \code{F9_09_EXP_FEE_SVC_INVEST_TOT})
#' @param other_fees Other fees for services (total). (On 990: Part IX, line 11g Col A; \code{F9_09_EXP_FEE_SVC_OTH_TOT})
#' @param total_expenses Total functional expenses. (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @details
#' \strong{Primary uses and key insights}
#'
#' The fees for services ratio measures the share of total expenses paid to outside
#' contractors across seven categories: management consultants, legal, accounting,
#' lobbying, professional fundraisers, investment managers, and other. It captures
#' the degree to which the organization outsources professional expertise.
#'
#' \strong{Formula variations and their sources}
#'
#' Sum of seven Part IX fee fields (lines 11a-11g, Column A) / total expenses (line 25A).
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Frumkin, P. & Keating, E.K. (2001). The price of doing good. \emph{Policy
#'     and Society}, 20(4), 94-112.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded \[0, 1\]. Most nonprofits show values in the \[0.01, 0.20\] range.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item No universal benchmark; most meaningful within-subsector or over time.
#'   \item High investment management fees relative to total expenses may indicate
#'     poor endowment management or excessive fee structures.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_09_EXP_FEE_SVC_MGMT_TOT}: Management fees (\code{mgmt_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_LEGAL_TOT}: Legal fees (\code{legal_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_ACC_TOT}: Accounting fees (\code{accounting_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_LOB_TOT}: Lobbying fees (\code{lobbying_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_FUNDR_TOT}: Professional fundraising fees (\code{prof_fundraising_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_INVEST_TOT}: Investment management fees (\code{investment_mgmt_fees})
#'   \item \code{F9_09_EXP_FEE_SVC_OTH_TOT}: Other fees for services (\code{other_fees})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#' }
#'
#' @param sanitize Logical (default \code{TRUE}).
#' @param summarize Logical (default \code{FALSE}).
#'
#' @usage
#' get_expenses_feesforservice_ratio( df,
#'   mgmt_fees            = "F9_09_EXP_FEE_SVC_MGMT_TOT",
#'   legal_fees           = "F9_09_EXP_FEE_SVC_LEGAL_TOT",
#'   accounting_fees      = "F9_09_EXP_FEE_SVC_ACC_TOT",
#'   lobbying_fees        = "F9_09_EXP_FEE_SVC_LOB_TOT",
#'   prof_fundraising_fees = "F9_09_EXP_FEE_SVC_FUNDR_TOT",
#'   investment_mgmt_fees = "F9_09_EXP_FEE_SVC_INVEST_TOT",
#'   other_fees           = "F9_09_EXP_FEE_SVC_OTH_TOT",
#'   total_expenses       = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{expenses_feesforservice}, \code{expenses_feesforservice_w},
#'   \code{expenses_feesforservice_z}, \code{expenses_feesforservice_p}.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_expenses_feesforservice_ratio( df = dat10k )
#' head( d[ , c( "expenses_feesforservice", "expenses_feesforservice_w", "expenses_feesforservice_z", "expenses_feesforservice_p" ) ] )
#'
#' @export
get_expenses_feesforservice_ratio <- function( df,
                     mgmt_fees             = "F9_09_EXP_FEE_SVC_MGMT_TOT",
                     legal_fees            = "F9_09_EXP_FEE_SVC_LEGAL_TOT",
                     accounting_fees       = "F9_09_EXP_FEE_SVC_ACC_TOT",
                     lobbying_fees         = "F9_09_EXP_FEE_SVC_LOB_TOT",
                     prof_fundraising_fees = "F9_09_EXP_FEE_SVC_FUNDR_TOT",
                     investment_mgmt_fees  = "F9_09_EXP_FEE_SVC_INVEST_TOT",
                     other_fees            = "F9_09_EXP_FEE_SVC_OTH_TOT",
                     total_expenses        = "F9_09_EXP_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )
  for ( arg in c("mgmt_fees","legal_fees","accounting_fees","lobbying_fees",
                 "prof_fundraising_fees","investment_mgmt_fees","other_fees","total_expenses") ) {
    if ( is.null( get(arg) ) ) stop( paste0("`", arg, "` cannot be NULL.") )
  }

  vars <- c( mgmt_fees, legal_fees, accounting_fees, lobbying_fees,
             prof_fundraising_fees, investment_mgmt_fees, other_fees, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  f1 <- resolve_col( dt, mgmt_fees )
  f2 <- resolve_col( dt, legal_fees )
  f3 <- resolve_col( dt, accounting_fees )
  f4 <- resolve_col( dt, lobbying_fees )
  f5 <- resolve_col( dt, prof_fundraising_fees )
  f6 <- resolve_col( dt, investment_mgmt_fees )
  f7 <- resolve_col( dt, other_fees )
  te <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( te == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  te[ te == 0 ] <- NA

  expenses_feesforservice <- ( f1 + f2 + f3 + f4 + f5 + f6 + f7 ) / te

  v <- winsorize_var( expenses_feesforservice, winsorize )
  EXPENSES_FEESFORSERVICE <- data.frame(
    expenses_feesforservice   = v$raw,
    expenses_feesforservice_w = v$winsorized,
    expenses_feesforservice_z = v$z,
    expenses_feesforservice_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_FEESFORSERVICE ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_FEESFORSERVICE$expenses_feesforservice,   na.rm = TRUE ), main = "EXPENSES_FEESFORSERVICE (raw)" )
    plot( density( EXPENSES_FEESFORSERVICE$expenses_feesforservice_w, na.rm = TRUE ), main = "EXPENSES_FEESFORSERVICE Winsorized" )
    plot( density( EXPENSES_FEESFORSERVICE$expenses_feesforservice_z, na.rm = TRUE ), main = "EXPENSES_FEESFORSERVICE Standardized (Z)" )
    plot( density( EXPENSES_FEESFORSERVICE$expenses_feesforservice_p, na.rm = TRUE ), main = "EXPENSES_FEESFORSERVICE Percentile" )
  }

  return( cbind( df, EXPENSES_FEESFORSERVICE ) )
}
