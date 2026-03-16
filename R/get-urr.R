###---------------------------------------------------
###   UNRESTRICTED REVENUE RATIO
###---------------------------------------------------

#' @title
#' Unrestricted Revenue Ratio
#'
#' @description
#' Estimated share of total revenue that flows into unrestricted net assets,
#' approximated from balance sheet changes.
#'
#' **Formula:**
#' ```
#' urr = unrestricted_revenue / total_revenue
#'
#' unrestricted_revenue = ( unrestricted_net_assets_eoy
#'                          - unrestricted_net_assets_boy )
#'                        + total_expenses
#' ```
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A \code{data.frame} containing the fields required for computing the metric.
#' @param unrestricted_net_assets_eoy Unrestricted net assets, end of year.
#'   (On 990: Part X, line 27B; \code{F9_10_NAFB_UNRESTRICT_EOY})
#' @param unrestricted_net_assets_boy Unrestricted net assets, beginning of year.
#'   (On 990: Part X, line 27A; \code{F9_10_NAFB_UNRESTRICT_BOY})
#' @param total_expenses Total functional expenses.
#'   (On 990: Part IX, line 25A; \code{F9_09_EXP_TOT_TOT})
#' @param total_revenue Total revenue. Accepts one or two column names.
#'   (On 990: Part VIII, line 12A; \code{F9_08_REV_TOT_TOT};
#'   On EZ: Part I, line 9; \code{F9_01_REV_TOT_CY})
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#'
#' @usage
#' get_urr( df,
#'   unrestricted_net_assets_eoy = "F9_10_NAFB_UNRESTRICT_EOY",
#'   unrestricted_net_assets_boy = "F9_10_NAFB_UNRESTRICT_BOY",
#'   total_expenses              = "F9_09_EXP_TOT_TOT",
#'   total_revenue               = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
#'   winsorize = 0.98,
#'   sanitize = TRUE )
#'
#' @return Object of class \code{data.frame}: the original dataframe appended with four
#'   new columns:
#'   \itemize{
#'     \item \code{urr}   — unrestricted revenue ratio (raw)
#'     \item \code{urr_w} — winsorized version
#'     \item \code{urr_z} — standardized z-score (based on winsorized values)
#'     \item \code{urr_p} — percentile rank (1-100)
#'   }
#'
#' @details
#' The unrestricted revenue ratio measures the proportion of total revenue that is
#' available without donor restrictions. Higher values indicate greater financial
#' flexibility, while lower values suggest heavy reliance on restricted gifts that
#' cannot be used for general operations.
#'
#' \strong{Important note on construction:} The IRS 990 Part VIII revenue statement
#' does not include a column for unrestricted versus restricted revenue. Unrestricted
#' revenue must therefore be approximated from the balance sheet. The approach used
#' here back-calculates unrestricted revenue as the change in unrestricted net assets
#' plus total expenses:
#'
#' \eqn{\text{unrestricted revenue} = (\text{UNA}_{EOY} - \text{UNA}_{BOY}) + \text{total expenses}}
#'
#' This identity holds because: \emph{unrestricted revenue - expenses = change in
#' unrestricted net assets}. The estimate may differ slightly from true unrestricted
#' revenue if there are transfers between restriction classes, investment gains booked
#' directly to net assets, or other balance sheet adjustments.
#'
#' Available to full 990 filers only because the unrestricted/restricted breakdown of
#' net assets (Part X, lines 27A-B) does not appear on the 990EZ.
#'
#' Cited by Keating et al. (2005) and NCCS.
#'
#' **Variables used:**
#' \itemize{
#'   \item \code{F9_10_NAFB_UNRESTRICT_EOY}: Unrestricted net assets, EOY (\code{unrestricted_net_assets_eoy})
#'   \item \code{F9_10_NAFB_UNRESTRICT_BOY}: Unrestricted net assets, BOY (\code{unrestricted_net_assets_boy})
#'   \item \code{F9_09_EXP_TOT_TOT}: Total functional expenses (\code{total_expenses})
#'   \item \code{F9_08_REV_TOT_TOT}: Total revenue from Part VIII (\code{total_revenue}, 990)
#'   \item \code{F9_01_REV_TOT_CY}: Total revenue from Part I (\code{total_revenue}, 990EZ)
#' }
#'
#' @param sanitize Logical (default \code{TRUE}). If \code{TRUE}, NA values in
#'   the financial input columns are imputed to zero before the ratio is computed,
#'   respecting form scope: Part X and VIII/IX fields (990 only) are imputed only
#'   for 990 filers; Part I summary fields (990 + 990EZ) are imputed for all filers.
#'   The returned dataframe always contains the original unmodified input columns.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' d <- get_urr( df = dat10k )
#' head( d[ , c( "urr", "urr_w", "urr_z", "urr_p" ) ] )
#'
#' @export
get_urr <- function( df,
                     unrestricted_net_assets_eoy = "F9_10_NAFB_UNRESTRICT_EOY",
                     unrestricted_net_assets_boy = "F9_10_NAFB_UNRESTRICT_BOY",
                     total_expenses              = "F9_09_EXP_TOT_TOT",
                     total_revenue               = c( "F9_08_REV_TOT_TOT",
                                                      "F9_01_REV_TOT_CY" ),
                     winsorize = 0.98 ,
                     sanitize = TRUE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  for ( arg_name in c( "unrestricted_net_assets_eoy", "unrestricted_net_assets_boy",
                        "total_expenses" ) ) {
    arg_val <- get( arg_name )
    if ( is.null( arg_val ) )
      stop( paste0( "`", arg_name, "` cannot be NULL." ) )
    if ( length( arg_val ) > 2 )
      stop( paste0( "`", arg_name, "` must be one or two column names." ) )
  }

  if ( is.null( total_revenue ) )
    stop( "`total_revenue` cannot be NULL." )
  if ( length( total_revenue ) > 2 )
    stop( "`total_revenue` must be one or two column names." )

  all_cols <- c( unrestricted_net_assets_eoy, unrestricted_net_assets_boy,
                 total_expenses, total_revenue )
  vars <- c( unrestricted_net_assets_eoy, unrestricted_net_assets_boy, total_expenses, total_revenue )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }
  dat <- dt

  una_eoy <- resolve_col( dat, unrestricted_net_assets_eoy )
  una_boy <- resolve_col( dat, unrestricted_net_assets_boy )
  exp_tot <- resolve_col( dat, total_expenses )
  rev_tot <- resolve_col( dat, total_revenue )

  # Approximate unrestricted revenue from balance sheet identity:
  # unrestricted_revenue - expenses = change in unrestricted net assets
  # => unrestricted_revenue = (UNA_EOY - UNA_BOY) + expenses
  unrestricted_rev <- ( una_eoy - una_boy ) + exp_tot

  message( paste0( "Total revenue equal to zero: ", sum( rev_tot == 0, na.rm = TRUE ),
                   " case(s) replaced with NA." ) )
  rev_tot[ rev_tot == 0 ] <- NA

  urr <- unrestricted_rev / rev_tot

  v <- winsorize_var( urr, winsorize )
  URR <- data.frame( urr   = v$raw,
                     urr_w = v$winsorized,
                     urr_z = v$z,
                     urr_p = v$pctile )

  return( cbind( df, URR ) )
}
