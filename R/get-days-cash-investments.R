###---------------------------------------------------
###   DAYS OF OPERATING CASH AND INVESTMENTS
###---------------------------------------------------

#' @title
#' Days of Operating Cash and Investments
#'
#' @description
#' Measures how many days an organization can operate using its liquid assets plus
#' its investment securities.
#'
#' **Formula:**
#' ```
#' doci = ( liquid_assets + investments ) / daily_expenses
#'
#' liquid_assets  = cash + savings + pledges_receivable + accounts_receivable
#' investments    = publicly_traded_securities + other_securities
#' daily_expenses = ( total_expenses - depreciation ) / 365
#' ```
#'
#' **Definitional Range**
#'
#' Zero or positive for ordinary filings and unbounded above, expressed in days.
#' Values above 365 indicate more than one year of coverage. The metric adds
#' investments to the numerator of [get_days_cash_operations()], so it is at least
#' as large whenever the reported investment balances are non-negative. Negative
#' values come only from filing anomalies: negative receivable or investment
#' balances, or depreciation larger than total expenses.
#'
#' **Benchmarks and rules of thumb**
#'
#'   - **180-365 days**: Considered a solid reserve position for endowed organizations.
#'   - A large gap between this metric and [get_days_cash_operations()] indicates
#'     liquidity concentrated in investments rather than accessible cash.
#'
#' **Calculated For:** 990 filers only.
#'
#' @param df A `data.frame` containing the fields required for computing the metric.
#' @param cash Cash on hand, EOY.
#' @param savings Savings and temporary cash investments, EOY.
#' @param pledges_receivable Net pledges and grants receivable, EOY.
#' @param accounts_receivable Accounts receivable, net, EOY.
#' @param investments Investment securities, EOY. One or more column names whose
#'   values are **summed** (unlike most column arguments, which coalesce a 990
#'   and a 990-EZ field). Defaults to publicly traded securities (Part X line 11)
#'   plus other securities (line 12).
#' @param total_expenses Total functional expenses.
#' @param depreciation Depreciation, depletion, and amortization.
#' @param numerator Optional. A pre-aggregated column for liquid assets plus
#'   investments. When supplied, `cash`, `savings`, `pledges_receivable`,
#'   `accounts_receivable`, and `investments` are ignored; it is an error to also set
#'   one of them explicitly.
#' @param denominator Optional. A pre-aggregated column for annual non-depreciation
#'   expenses (the function divides this by 365 internally). When supplied,
#'   `total_expenses`, `depreciation` are ignored; it is an error to also set one of
#'   them explicitly.
#' @param winsorize The winsorization value (between 0 and 1), defaults to 0.98, which
#'   winsorizes at the 1st and 99th percentiles.
#' @param range Character string specifying the theoretical range of the ratio,
#'   used to set winsorization bounds. Default `"zp"`. Options:
#'   `"np"` (negative to positive), `"zp"` (zero to positive),
#'   `"zo"` (zero to one), `"nz"` (negative to zero), or a custom
#'   `"lo;hi"` pair (e.g. `"0;10"`).
#'
#' @usage
#' get_days_cash_investments( df,
#'   cash                = "F9_10_ASSET_CASH_EOY",
#'   savings             = "F9_10_ASSET_SAVING_EOY",
#'   pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
#'   accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
#'   investments         = c( "F9_10_ASSET_INVEST_SEC_EOY",
#'                            "F9_10_ASSET_INVEST_SEC_OTH_EOY" ),
#'   total_expenses      = "F9_09_EXP_TOT_TOT",
#'   depreciation        = "F9_09_EXP_DEPREC_TOT",
#'   numerator = NULL, denominator = NULL,
#'   winsorize = 0.98 ,
#'   range     = "zp",
#'   sanitize  = TRUE,
#'   summarize = FALSE )
#'
#' @return Object of class `data.frame`: the original dataframe appended with four
#'   new columns:
#'
#'     - `days_cash_inv`   - days of operating cash and investments (raw)
#'     - `days_cash_inv_w` - winsorized version
#'     - `days_cash_inv_z` - standardized z-score (based on winsorized values)
#'     - `days_cash_inv_p` - percentile rank (1-100)
#'
#'
#' @details
#' ## Primary uses and key insights
#'
#' Days of cash and investments extends [get_days_cash_operations()] by adding
#' investment securities to the liquidity numerator. It asks: if the organization
#' drew on its cash, receivables, and investment portfolio, how many days of
#' operations could it fund? This broader measure captures organizations that hold
#' significant reserves in investment portfolios rather than bank accounts.
#'
#' It is most relevant for endowed organizations, foundations, and mature nonprofits
#' that hold investment portfolios. For organizations with no investments,
#' [get_days_cash_operations()] and this metric are identical, so the difference
#' between the two is the coverage that investments add.
#'
#' ## Formula variations and their sources
#'
#' (Cash + savings + pledges receivable + accounts receivable + publicly traded
#' securities + other securities) / ((total expenses - depreciation) / 365), using
#' Part X lines 1, 2, 3, 4, 11, and 12 and Part IX line 25 less line 22. Rating
#' agencies use a similar "days cash on hand" measure that adds unrestricted cash and
#' investments; this version keeps the receivables in [get_days_cash_operations()]
#' so the two metrics nest. Program-related investments (line 13) are excluded
#' because they are not held for liquidity.
#'
#' The Form 990 does not separate donor-restricted investments, such as permanent
#' endowment, from unrestricted ones, so the numerator can overstate the resources
#' available for general operations. For organizations with large restricted
#' endowments, read this metric alongside [get_netassets_composition_ratio()].
#'
#' Earlier versions started from unrestricted net assets, added investments, and
#' subtracted both net fixed assets and mortgages. That counted investments twice
#' (they are already part of net assets) and treated mortgage debt as reducing
#' liquidity a second time, so organizations with mortgaged buildings often scored
#' negative. Earlier versions also used `F9_10_ASSET_INV_SALE_EOY` (Part X line 8,
#' inventories) as investments.
#'
#' ## Canonical citations
#'
#'
#'   - Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
#'     for Nonprofit Organizations*. Wiley.
#'   - Calabrese, T.D. (2013). Running on empty: The operating reserves of U.S.
#'     nonprofit organizations. *Nonprofit Management and Leadership*, 23(3),
#'     281-302.
#'
#'
#' ## Variables used:
#'
#'   - `F9_10_ASSET_CASH_EOY`: Cash, non-interest bearing, EOY (Part X line 1) (`cash`)
#'   - `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY (Part X
#'     line 2) (`savings`)
#'   - `F9_10_ASSET_PLEDGE_NET_EOY`: Pledges and grants receivable, net, EOY (Part X
#'     line 3) (`pledges_receivable`)
#'   - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY (Part X line 4)
#'     (`accounts_receivable`)
#'   - `F9_10_ASSET_INVEST_SEC_EOY`: Investments, publicly traded securities,
#'     EOY (Part X line 11) (`investments`)
#'   - `F9_10_ASSET_INVEST_SEC_OTH_EOY`: Investments, other securities, EOY
#'     (Part X line 12) (`investments`)
#'   - `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)
#'   - `F9_09_EXP_DEPREC_TOT`: Depreciation and amortization (`depreciation`)
#'
#'
#' @param sanitize Logical (default `TRUE`). If `TRUE`, NA values in
#'   the financial input columns are imputed to zero before the ratio is computed,
#'   respecting form scope: Part X and VIII/IX fields (990 only) are imputed only
#'   for 990 filers; Part I summary fields (990 + 990EZ) are imputed for all filers.
#'   The returned dataframe always contains the original unmodified input columns.
#'
#' @param summarize Logical. If `TRUE`, prints a `summary()` of
#'   the results and plots density curves for all four output columns
#'   (raw, winsorized, z-score, percentile). Defaults to `FALSE`.
#'
#' @import dplyr
#' @import stringr
#' @import magrittr
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' d <- get_days_cash_investments( df = dat10k )
#' head( d[ , c( "days_cash_inv", "days_cash_inv_w", "days_cash_inv_z", "days_cash_inv_p" ) ] )
#'
#' @export
get_days_cash_investments <- function( df,
                      cash                = "F9_10_ASSET_CASH_EOY",
                      savings             = "F9_10_ASSET_SAVING_EOY",
                      pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
                      accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
                      investments         = c( "F9_10_ASSET_INVEST_SEC_EOY",
                                               "F9_10_ASSET_INVEST_SEC_OTH_EOY" ),
                      total_expenses      = "F9_09_EXP_TOT_TOT",
                      depreciation        = "F9_09_EXP_DEPREC_TOT",
                      numerator   = NULL,
                      denominator = NULL,
                      winsorize = 0.98  ,
                     range     = "zp" ,
                     sanitize  = TRUE,
                     summarize = FALSE )
{
  if ( winsorize > 1 | winsorize < 0 ) stop( "winsorize must be between 0 and 1." )

  supplied <- names( match.call() )[ -1 ]
  list2env( resolve_components( numerator, "numerator",
    list( cash = cash, savings = savings, pledges_receivable = pledges_receivable,
          accounts_receivable = accounts_receivable, investments = investments ),
    supplied ), environment() )
  list2env( resolve_components( denominator, "denominator",
    list( total_expenses = total_expenses, depreciation = depreciation ), supplied ), environment() )

  vars <- c( cash, savings, pledges_receivable, accounts_receivable, investments,
             total_expenses, depreciation, numerator, denominator )
  KEEP <- intersect( c( .IDVARS, vars ), colnames( df ) )
  dt   <- dplyr::select( df, dplyr::any_of( KEEP ) )
  dt     <- coerce_numeric( dt, vars = intersect( vars, colnames( dt ) ) )
  if ( sanitize ) {
    dt <- sanitize_financials( dt )
  }

  if ( !is.null( numerator ) ) {
    num <- dt[[ numerator ]]
  } else {
    missing_inv <- setdiff( investments, colnames( dt ) )
    if ( length( missing_inv ) > 0 )
      stop( "Investment column(s) not found in the data: ",
            paste( missing_inv, collapse = ", " ) )
    # Investment columns are summed (Part X lines 11 + 12), not coalesced.
    inv <- Reduce( `+`, lapply( investments, function( v ) dt[[ v ]] ) )
    num <- dt[[ cash ]] + dt[[ savings ]] +
           dt[[ pledges_receivable ]] + dt[[ accounts_receivable ]] + inv
  }

  if ( !is.null( denominator ) ) {
    den <- dt[[ denominator ]] / 365
  } else {
    den <- ( dt[[ total_expenses ]] - dt[[ depreciation ]] ) / 365
  }

  nan.count <- sum( den == 0, na.rm = TRUE ) |> format( big.mark="," )
  message( paste0( "   :: Daily operating expenses equal to zero :: ", nan.count,
                   " case(s) replaced with NaN" ) )
  den[ den == 0 ] <- NaN

  doci <- num / den

  v <- apply_transformations( doci, winsorize, range )
  DAYS_CASH_INV <- data.frame( days_cash_inv   = v$raw,
                      days_cash_inv_w = v$winsorized,
                      days_cash_inv_z = v$z,
                      days_cash_inv_p = v$pctile )

  if ( summarize ) {
    print( summary( DAYS_CASH_INV ) )
    op <- par( mfrow = c(2,2) )
    on.exit( par(op), add = TRUE )
    plot( density( DAYS_CASH_INV$days_cash_inv, na.rm = TRUE ), main = "DAYS_CASH_INV (raw)" )
    plot( density( DAYS_CASH_INV$days_cash_inv_w, na.rm = TRUE ), main = "DAYS_CASH_INV Winsorized" )
    plot( density( DAYS_CASH_INV$days_cash_inv_z, na.rm = TRUE ), main = "DAYS_CASH_INV Standardized (Z)" )
    plot( density( DAYS_CASH_INV$days_cash_inv_p, na.rm = TRUE ), main = "DAYS_CASH_INV Percentile" )
  }

  return( cbind( df, DAYS_CASH_INV ) )
}
