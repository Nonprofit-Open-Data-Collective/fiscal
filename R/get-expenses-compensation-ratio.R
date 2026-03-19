###---------------------------------------------------
###   COMPENSATION EXPENSE RATIO
###---------------------------------------------------

#' @title
#' Compensation Expense Ratio
#'
#' @description
#' Total compensation and employee-related expenses as a share of total functional expenses.
#'
#' **Formula:**
#' ```
#' expenses_compensation = ( officer_comp + disqualified_comp + other_salaries
#'                           + pension_contributions + other_employee_benefits
#'                           + payroll_taxes ) / total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param officer_comp Compensation of current officers, directors, and key employees (total).
#'   (On 990: Part IX, line 5 Column A; \code{F9_09_EXP_COMP_DTK_TOT})
#' @param disqualified_comp Compensation of disqualified persons (total).
#'   (On 990: Part IX, line 6 Column A; \code{F9_09_EXP_COMP_DSQ_PERS_TOT})
#' @param other_salaries Other salaries and wages (total).
#'   (On 990: Part IX, line 7 Column A; \code{F9_09_EXP_OTH_SAL_WAGE_TOT})
#' @param pension_contributions Pension plan accruals and contributions (total).
#'   (On 990: Part IX, line 8 Column A; \code{F9_09_EXP_PENSION_CONTR_TOT})
#' @param other_employee_benefits Other employee benefits (total).
#'   (On 990: Part IX, line 9 Column A; \code{F9_09_EXP_OTH_EMPL_BEN_TOT})
#' @param payroll_taxes Payroll taxes (total).
#'   (On 990: Part IX, line 10 Column A; \code{F9_09_EXP_PAYROLL_TAX_TOT})
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param winsorize Winsorization proportion between 0 and 1 (default \code{0.98}).
#' @details
#' \strong{Primary uses and key insights}
#'
#' The compensation expense ratio measures the share of total functional expenses
#' devoted to all forms of employee compensation: salaries, benefits, payroll taxes,
#' and pension contributions. For most service-delivering nonprofits this is the
#' largest single expense category (60-80\% of total expenses). It is a fundamental
#' indicator of labor intensity and human capital investment.
#'
#' \strong{Formula variations and their sources}
#'
#' Sum of six Part IX compensation fields (lines 5-10, Column A) / total expenses
#' (line 25A): officer compensation, disqualified person compensation, other salaries,
#' pension contributions, other employee benefits, and payroll taxes.
#'
#' \strong{Canonical citations}
#'
#' \itemize{
#'   \item Frumkin, P. & Keating, E.K. (2001). The price of doing good. \emph{Policy
#'     and Society}, 20(4), 94-112.
#'   \item Leete, L. (2001). Whither the nonprofit wage differential? \emph{Journal of
#'     Labor Economics}, 19(1), 136-170.
#' }
#'
#' \strong{Definitional range}
#'
#' Bounded \[0, 1\]. Most service-delivery nonprofits fall in the \[0.50, 0.85\] range.
#' Grant-making organizations may show ratios below 0.20.
#'
#' \strong{Benchmarks and rules of thumb}
#'
#' \itemize{
#'   \item No universal benchmark — labor intensity varies fundamentally by mission type.
#'   \item Human services, health, and education nonprofits typically show ratios above 0.60.
#'   \item A sharp year-over-year increase may indicate revenue contraction rather than
#'     compensation growth.
#' }
#'
#' \strong{Variables used:}
#' \itemize{
#'   \item \code{F9_09_EXP_COMP_DTK_TOT}: Officer/director compensation (\code{officer_comp})
#'   \item \code{F9_09_EXP_COMP_DSQ_PERS_TOT}: Disqualified person compensation (\code{disqualified_comp})
#'   \item \code{F9_09_EXP_OTH_SAL_WAGE_TOT}: Other salaries and wages (\code{other_salaries})
#'   \item \code{F9_09_EXP_PENSION_CONTR_TOT}: Pension contributions (\code{pension_contributions})
#'   \item \code{F9_09_EXP_OTH_EMPL_BEN_TOT}: Other employee benefits (\code{other_employee_benefits})
#'   \item \code{F9_09_EXP_PAYROLL_TAX_TOT}: Payroll taxes (\code{payroll_taxes})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#' }
#'
#' @param sanitize Logical (default \code{TRUE}).
#' @param summarize Logical (default \code{FALSE}).
#'
#' @usage
#' get_expenses_compensation_ratio( df,
#'   officer_comp           = "F9_09_EXP_COMP_DTK_TOT",
#'   disqualified_comp      = "F9_09_EXP_COMP_DSQ_PERS_TOT",
#'   other_salaries         = "F9_09_EXP_OTH_SAL_WAGE_TOT",
#'   pension_contributions  = "F9_09_EXP_PENSION_CONTR_TOT",
#'   other_employee_benefits = "F9_09_EXP_OTH_EMPL_BEN_TOT",
#'   payroll_taxes          = "F9_09_EXP_PAYROLL_TAX_TOT",
#'   total_expenses         = "F9_09_EXP_TOT_TOT",
#'   winsorize  = 0.98,
#'   sanitize   = TRUE,
#'   summarize  = FALSE )
#'
#' @return The original \code{data.frame} with four columns appended:
#'   \code{expenses_compensation}, \code{expenses_compensation_w},
#'   \code{expenses_compensation_z}, \code{expenses_compensation_p}.
#'
#' @details
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_09_EXP_COMP_DTK_TOT}: Officer/director compensation (\code{officer_comp})
#'   \item \code{F9_09_EXP_COMP_DSQ_PERS_TOT}: Disqualified person compensation (\code{disqualified_comp})
#'   \item \code{F9_09_EXP_OTH_SAL_WAGE_TOT}: Other salaries and wages (\code{other_salaries})
#'   \item \code{F9_09_EXP_PENSION_CONTR_TOT}: Pension contributions (\code{pension_contributions})
#'   \item \code{F9_09_EXP_OTH_EMPL_BEN_TOT}: Other employee benefits (\code{other_employee_benefits})
#'   \item \code{F9_09_EXP_PAYROLL_TAX_TOT}: Payroll taxes (\code{payroll_taxes})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#' }
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#' @examples
#' library( fiscal )
#' data( dat10k )
#' d <- get_expenses_compensation_ratio( df = dat10k )
#' head( d[ , c( "expenses_compensation", "expenses_compensation_w", "expenses_compensation_z", "expenses_compensation_p" ) ] )
#'
#' @export
get_expenses_compensation_ratio <- function( df,
                     officer_comp            = "F9_09_EXP_COMP_DTK_TOT",
                     disqualified_comp       = "F9_09_EXP_COMP_DSQ_PERS_TOT",
                     other_salaries          = "F9_09_EXP_OTH_SAL_WAGE_TOT",
                     pension_contributions   = "F9_09_EXP_PENSION_CONTR_TOT",
                     other_employee_benefits = "F9_09_EXP_OTH_EMPL_BEN_TOT",
                     payroll_taxes           = "F9_09_EXP_PAYROLL_TAX_TOT",
                     total_expenses          = "F9_09_EXP_TOT_TOT",
                     winsorize  = 0.98,
                     sanitize   = TRUE,
                     summarize  = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )
  for ( arg in c("officer_comp","disqualified_comp","other_salaries","pension_contributions",
                 "other_employee_benefits","payroll_taxes","total_expenses") ) {
    if ( is.null( get(arg) ) ) stop( paste0("`", arg, "` cannot be NULL.") )
  }

  vars <- c( officer_comp, disqualified_comp, other_salaries, pension_contributions,
             other_employee_benefits, payroll_taxes, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt   <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) { dt <- sanitize_financials( dt ) }

  c1 <- resolve_col( dt, officer_comp )
  c2 <- resolve_col( dt, disqualified_comp )
  c3 <- resolve_col( dt, other_salaries )
  c4 <- resolve_col( dt, pension_contributions )
  c5 <- resolve_col( dt, other_employee_benefits )
  c6 <- resolve_col( dt, payroll_taxes )
  te <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( te == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  te[ te == 0 ] <- NA

  expenses_compensation <- ( c1 + c2 + c3 + c4 + c5 + c6 ) / te

  v <- winsorize_var( expenses_compensation, winsorize )
  EXPENSES_COMPENSATION <- data.frame(
    expenses_compensation   = v$raw,
    expenses_compensation_w = v$winsorized,
    expenses_compensation_z = v$z,
    expenses_compensation_p = v$pctile )

  if ( summarize ) {
    print( summary( EXPENSES_COMPENSATION ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( EXPENSES_COMPENSATION$expenses_compensation,   na.rm = TRUE ), main = "EXPENSES_COMPENSATION (raw)" )
    plot( density( EXPENSES_COMPENSATION$expenses_compensation_w, na.rm = TRUE ), main = "EXPENSES_COMPENSATION Winsorized" )
    plot( density( EXPENSES_COMPENSATION$expenses_compensation_z, na.rm = TRUE ), main = "EXPENSES_COMPENSATION Standardized (Z)" )
    plot( density( EXPENSES_COMPENSATION$expenses_compensation_p, na.rm = TRUE ), main = "EXPENSES_COMPENSATION Percentile" )
  }

  return( cbind( df, EXPENSES_COMPENSATION ) )
}
