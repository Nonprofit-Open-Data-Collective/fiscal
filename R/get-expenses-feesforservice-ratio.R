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
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param mgmt_fees Management fees (total).
#' @param legal_fees Legal fees (total).
#' @param accounting_fees Accounting fees (total).
#' @param lobbying_fees Lobbying fees (total).
#' @param prof_fundraising_fees Professional fundraising fees (total).
#' @param investment_mgmt_fees Investment management fees (total).
#' @param other_fees Other fees for services (total).
#' @param total_expenses Total functional expenses.
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @details
#' ## Primary uses and key insights
#'
#' The fees for services ratio measures the share of total expenses paid to outside
#' contractors across seven categories: management consultants, legal, accounting,
#' lobbying, professional fundraisers, investment managers, and other. It captures
#' the degree to which the organization outsources professional expertise.
#'
#' ## Formula variations and their sources
#'
#' Sum of seven Part IX fee fields (lines 11a-11g, Column A) / total expenses (line 25A).
#'
#' ## Canonical citations
#'
#'
#'   - Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
#'     and Society*, 20(4), 94-112.
#'
#'
#' ## Definitional range
#'
#' Bounded \[0, 1\]. Most nonprofits show values in the \[0.01, 0.20\] range.
#'
#' ## Benchmarks and rules of thumb
#'
#'
#'   - No universal benchmark; most meaningful within-subsector or over time.
#'   - High investment management fees relative to total expenses may indicate
#'     poor endowment management or excessive fee structures.
#'
#'
#' ## Variables used:
#'
#'   - `F9_09_EXP_FEE_SVC_MGMT_TOT`: Management fees (`mgmt_fees`)
#'   - `F9_09_EXP_FEE_SVC_LEGAL_TOT`: Legal fees (`legal_fees`)
#'   - `F9_09_EXP_FEE_SVC_ACC_TOT`: 
#'     Accounting fees (`accounting_fees`)
#'   - `F9_09_EXP_FEE_SVC_LOB_TOT`: Lobbying fees (`lobbying_fees`)
#'   - `F9_09_EXP_FEE_SVC_FUNDR_TOT`: 
#'     Professional fundraising fees (`prof_fundraising_fees`)
#'   - `F9_09_EXP_FEE_SVC_INVEST_TOT`: 
#'     Investment management fees (`investment_mgmt_fees`)
#'   - `F9_09_EXP_FEE_SVC_OTH_TOT`: 
#'     Other fees for services (`other_fees`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'
#'
#' @param sanitize Logical (default `TRUE`).
#' @param summarize Logical (default `FALSE`).
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
#' @return The original `data.frame` with four new columns:
#'
#'   - `expenses_feesforservice` (raw ratio)
#'   - `expenses_feesforservice_w` (winsorized)
#'   - `expenses_feesforservice_z` (z-score)
#'   - `expenses_feesforservice_p` (percentile rank, 1-100)
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
