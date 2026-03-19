###---------------------------------------------------
###   PROGRAM EXPENSE RATIO
###---------------------------------------------------

#' @title Program Expense Ratio
#'
#' @description
#' Share of total expenses devoted to program services.
#'
#' **Formula**
#'
#' `per = program_expenses / total_expenses`
#'
#' **Calculated for:** 990 and 990-EZ filers.
#'
#' @param df A data.frame containing the fields required for computing the metric.
#' @param program_expenses Program service expenses
#'   (Form 990, Part IX, line 25B; `F9_09_EXP_TOT_PROG`).
#' @param total_expenses Total functional expenses. Accepts one or two column names;
#'   if two are provided, they are coalesced with the 990 value taking priority over
#'   the 990-EZ value. Sources include Form 990, Part IX, line 25A
#'   (`F9_09_EXP_TOT_TOT`) and Form 990-EZ, Part I, line 17
#'   (`F9_01_EXP_TOT_CY`).
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`).
#' @param sanitize Logical (default `TRUE`). If `TRUE`, imputes zero for missing
#'   financial fields before computing, respecting form scope.
#' @param summarize Logical (default `FALSE`). If `TRUE`, prints summary
#'   statistics and density plots for all four output columns.
#'
#' @usage
#' get_program_expenses_ratio( df,
#'   program_expenses = "F9_09_EXP_TOT_PROG",
#'   total_expenses   = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" ),
#'   winsorize = 0.98,
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return The original data.frame with four appended columns:
#' - `prog_exp`: raw program expense ratio
#' - `prog_exp_w`: winsorized ratio
#' - `prog_exp_z`: standardized z-score based on winsorized values
#' - `prog_exp_p`: percentile rank
#'
#' @details
#' ## Primary uses and key insights
#'
#' The program expense ratio is one of the most widely cited metrics in nonprofit
#' financial analysis. It measures the share of total expenses devoted directly to
#' mission-related program services, as opposed to management, administration, and
#' fundraising. It is commonly used in charity ratings and by donors, funders, and
#' boards as a shorthand measure of financial efficiency.
#'
#' It is also one of the most criticized metrics in the literature. The measure can
#' be gamed through cost allocation practices, overhead is often necessary for long-term
#' effectiveness, and the "ideal" ratio varies substantially by mission, scale, and
#' organizational model.
#'
#' ## Formula variations and sources
#'
#' The standard formulation is program service expenses divided by total functional
#' expenses, using Form 990 Part IX line 25B divided by line 25A.
#'
#' For 990-EZ filers, total expenses from Part I may be used as a denominator fallback.
#' However, because the 990-EZ does not separately report program expenses, this ratio
#' is primarily meaningful for full Form 990 filers.
#'
#' An alternative formulation uses total revenue rather than total expenses in the
#' denominator, but the expense-based version is more standard and avoids distortions
#' caused by surplus or deficit years.
#'
#' ## Canonical citations
#'
#' - Tuckman, H.P. and Chang, C.F. (1991). A methodology for measuring the financial
#'   vulnerability of charitable nonprofit organizations. *Nonprofit and Voluntary
#'   Sector Quarterly*, 20(4), 445-460.
#' - Charity Navigator, *Financial Health Methodology*. Uses the program expense ratio
#'   as a major rating component.
#' - Lecy, J.D. and Searing, E.A. (2015). Anatomy of the nonprofit starvation cycle.
#'   *Nonprofit and Voluntary Sector Quarterly*, 44(3), 539-563.
#' - Overhead Myth Campaign (GuideStar, BBB Wise Giving Alliance, Charity Navigator,
#'   2013). Industry statement cautioning against overreliance on overhead ratios.
#'
#' ## Definitional range
#'
#' The ratio is bounded between 0 and 1. A value of 0 indicates no spending on
#' programs, while a value of 1 indicates that all reported spending is allocated to
#' programs. In practice, values above 0.95 are unusual and may reflect accounting
#' allocation choices. The empirical range for many operating nonprofits is roughly
#' 0.50 to 0.95.
#'
#' ## Benchmarks and rules of thumb
#'
#' - `>= 75%`: Charity Navigator threshold for a favorable efficiency score.
#' - `>= 65%`: BBB Wise Giving Alliance minimum standard.
#' - `< 50%`: Often flagged as a concern, though it may be legitimate for organizations
#'   in startup phases or those with unusually high fundraising requirements.
#' - Very high program ratios can sometimes reflect underinvestment in administrative
#'   capacity rather than true efficiency.
#'
#' ## Variables used
#'
#' - `F9_09_EXP_TOT_PROG`: Program service expenses (`program_expenses`, Form 990)
#' - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`, Form 990)
#' - `F9_01_EXP_TOT_CY`: Total expenses (`total_expenses`, 990-EZ fallback)
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
#' d <- get_program_expenses_ratio( df = dat10k )
#' head( d[ , c( "prog_exp", "prog_exp_w", "prog_exp_z", "prog_exp_p" ) ] )
#'
#' @export
get_program_expenses_ratio <- function( df,
                     program_expenses = "F9_09_EXP_TOT_PROG",
                     total_expenses   = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" ),
                     winsorize = 0.98 ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  validate_inputs( winsorize, program_expenses, total_expenses, "program_expenses", "total_expenses" )

  if ( length( program_expenses ) > 2 ) stop( "`program_expenses` must be one or two column names." )
  if ( length( total_expenses )   > 2 ) stop( "`total_expenses` must be one or two column names."   )

  vars <- c( program_expenses, total_expenses )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  p <- resolve_col( dt, program_expenses )
  e <- resolve_col( dt, total_expenses )

  message( paste0( "Total expenses equal to zero: ", sum( e == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  e[ e == 0 ] <- NA

  per <- p / e

  v <- winsorize_var( per, winsorize )
  PROG_EXP <- data.frame( prog_exp   = v$raw,
                     prog_exp_w = v$winsorized,
                     prog_exp_z = v$z,
                     prog_exp_p = v$pctile )

  if ( summarize ) {
    print( summary( PROG_EXP ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( PROG_EXP$prog_exp, na.rm = TRUE ), main = "PROG_EXP (raw)" )
    plot( density( PROG_EXP$prog_exp_w, na.rm = TRUE ), main = "PROG_EXP Winsorized" )
    plot( density( PROG_EXP$prog_exp_z, na.rm = TRUE ), main = "PROG_EXP Standardized (Z)" )
    plot( density( PROG_EXP$prog_exp_p, na.rm = TRUE ), main = "PROG_EXP Percentile" )
  }

  return( cbind( df, PROG_EXP ) )
}
