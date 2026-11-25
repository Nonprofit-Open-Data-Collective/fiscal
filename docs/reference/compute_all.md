# Compute all fiscal health metrics in one call

Applies every `get_*()` ratio function in the `fiscal` package to a data
frame in a single call. The data frame is sanitized once up front using
[`sanitize_financials()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/sanitize_financials.md),
then each function is called with `sanitize = FALSE` to avoid redundant
imputation passes. A metric that cannot be computed (for example,
because one of its input columns is missing from `df`) is skipped with a
warning.

## Usage

``` r
compute_all(
  df,
  metrics = c("ratio", "w", "z", "p"),
  append_to_df = TRUE,
  winsorize = 0.98,
  verbose = TRUE
)
```

## Arguments

- df:

  A `data.frame` containing IRS 990 efile financial fields.

- metrics:

  Character vector of metric variants to return. Any combination of
  `"ratio"`, `"w"`, `"z"`, `"p"`. Defaults to all four.

- append_to_df:

  Logical. If `TRUE` (default), returns the original `df` with metric
  columns appended. If `FALSE`, returns only `.IDVARS` identifier
  columns plus the selected metric columns.

- winsorize:

  Winsorization proportion (default `0.98`).

- verbose:

  Logical. If `TRUE` (default), prints progress messages.

## Value

A `data.frame`.

## Examples

``` r
library( fiscal )
data( dat10k )
d <- compute_all( dat10k )
#> Sanitizing financial fields ...
#> 
#> }} Computing get_current_ratio ...
#>    :: Current liabilities equal to zero :: 2,608 case(s) replaced with NaN
#> 
#> }} Computing get_quick_ratio ...
#>    :: Current liabilities equal to zero :: 2,608 case(s) replaced with NaN
#> 
#> }} Computing get_cash_liquidity_ratio ...
#>    :: Current liabilities equal to zero :: 2,608 case(s) replaced with NaN
#> 
#> }} Computing get_cash_assets_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_cash_on_hand ...
#> 
#> }} Computing get_cash_burn_ratio ...
#>    :: Beginning-of-year cash equal to zero :: 646 case(s) replaced with NaN
#> 
#> }} Computing get_days_cash_operations ...
#>    :: Daily operating expenses equal to zero :: 63 case(s) replaced with NaN
#> 
#> }} Computing get_days_cash_investments ...
#>    :: Daily operating expenses equal to zero :: 63 case(s) replaced with NaN
#> 
#> }} Computing get_months_cash_operations ...
#>    :: Monthly operating expenses equal to zero :: 63 case(s) replaced with NaN
#> 
#> }} Computing get_liquid_assets_months ...
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_debt_assets_ratio ...
#>    :: Assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_debt_equity_ratio ...
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Equity equal to zero :: 170 case(s) replaced with NaN
#> 
#> }} Computing get_debt_netassets_ratio ...
#>    :: Net assets equal to zero :: 335 case(s) replaced with NaN
#> 
#> }} Computing get_debt_shortterm_ratio ...
#>    :: Net assets equal to zero :: 335 case(s) replaced with NaN
#> 
#> }} Computing get_debt_secured_ratio ...
#>    :: Total liabilities equal to zero :: 4,835 case(s) replaced with NaN
#> 
#> }} Computing get_debt_unsecured_ratio ...
#>    :: Total liabilities equal to zero :: 4,835 case(s) replaced with NaN
#> 
#> }} Computing get_self_sufficiency_ratio ...
#>    :: Total expenses equal to zero :: 206 case(s) replaced with NaN
#> 
#> }} Computing get_earned_income_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_grants_govt_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_donations_revenue_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_investment_income_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_assets_revenue_ratio ...
#>    :: Total revenue equal to zero :: 212 case(s) replaced with NaN
#> 
#> }} Computing get_revenue_fedcampaign_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_revenue_membdues_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_revenue_fundevents_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_revenue_reltdorgs_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_revenue_programs_ratio ...
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_program_expenses_ratio ...
#>    :: Total expenses equal to zero :: 206 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_admin_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_overhead_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_fundraising_efficiency_ratio ...
#>    :: Total contributions equal to zero :: 862 case(s) replaced with NaN
#> 
#> }} Computing get_profit_margin_postdepr ...
#>    :: Revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_profit_margin_predepr ...
#>    :: Revenue equal to zero :: 64 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_grants_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_membbenefits_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_compensation_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_feesforservice_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_expenses_affiliates_ratio ...
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_equity_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_netassets_composition_ratio ...
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Total net assets equal to zero :: 335 case(s) replaced with NaN
#> 
#> }} Computing get_operating_reserve_ratio ...
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
#> 
#> }} Computing get_land_assets_gross_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_land_assets_net_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_investments_assets_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_netassets_growth_ratio ...
#>    :: Beginning net assets equal to zero :: 591 case(s) replaced with NaN
#> 
#> }} Computing get_surplus_margin_ratio ...
#>    :: Total revenue equal to zero :: 212 case(s) replaced with NaN
#> 
#> }} Computing get_return_assets_ratio ...
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
#> 
#> }} Computing get_return_netassets_ratio ...
#>    :: Beginning net assets equal to zero :: 591 case(s) replaced with NaN
#> Done. 192 metric column(s) produced.
dim( d )
#> [1] 10000   305
```
