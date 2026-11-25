# Fees for Services Expense Ratio

Total fees paid for outside services (management, legal, accounting,
lobbying, professional fundraising, investment management, and other) as
a share of total functional expenses.

**Formula:**

    expenses_feesforservice = ( mgmt_fees + legal_fees + accounting_fees
                                + lobbying_fees + prof_fundraising_fees
                                + investment_mgmt_fees + other_fees )
                              / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Most nonprofits show values in the \[0.01, 0.20\]
range.

**Benchmarks and rules of thumb**

- No universal benchmark; most meaningful within subsector or over time.

- Unusually high investment management fees relative to total expenses
  may indicate poor endowment management or excessive fee structures.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_feesforservice_ratio( df,
  mgmt_fees            = "F9_09_EXP_FEE_SVC_MGMT_TOT",
  legal_fees           = "F9_09_EXP_FEE_SVC_LEGAL_TOT",
  accounting_fees      = "F9_09_EXP_FEE_SVC_ACC_TOT",
  lobbying_fees        = "F9_09_EXP_FEE_SVC_LOB_TOT",
  prof_fundraising_fees = "F9_09_EXP_FEE_SVC_FUNDR_TOT",
  investment_mgmt_fees = "F9_09_EXP_FEE_SVC_INVEST_TOT",
  other_fees           = "F9_09_EXP_FEE_SVC_OTH_TOT",
  total_expenses       = "F9_09_EXP_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- mgmt_fees:

  Management fees (total).

- legal_fees:

  Legal fees (total).

- accounting_fees:

  Accounting fees (total).

- lobbying_fees:

  Lobbying fees (total).

- prof_fundraising_fees:

  Professional fundraising fees (total).

- investment_mgmt_fees:

  Investment management fees (total).

- other_fees:

  Other fees for services (total).

- total_expenses:

  Total functional expenses.

- winsorize:

  Winsorization proportion between 0 and 1 (default `0.98`).

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"zo"`. Options: `"np"` (negative
  to positive), `"zp"` (zero to positive), `"zo"` (zero to one), `"nz"`
  (negative to zero), or a custom `"lo;hi"` pair (e.g. `"0;10"`).

- sanitize:

  Logical (default `TRUE`).

- summarize:

  Logical (default `FALSE`).

## Value

The original `data.frame` with four new columns:

- `expenses_feesforservice` (raw ratio)

- `expenses_feesforservice_w` (winsorized)

- `expenses_feesforservice_z` (z-score)

- `expenses_feesforservice_p` (percentile rank, 1-100)

## Details

### Primary uses and key insights

The fees for services ratio measures the share of total expenses paid to
outside contractors across seven categories: management consultants,
legal, accounting, lobbying, professional fundraisers, investment
managers, and other. It captures the degree to which the organization
outsources professional expertise.

### Formula variations and their sources

Sum of seven Part IX fee fields (lines 11a-11g, Column A) / total
expenses (line 25A).

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

### Variables used:

- `F9_09_EXP_FEE_SVC_MGMT_TOT`: Management fees (`mgmt_fees`)

- `F9_09_EXP_FEE_SVC_LEGAL_TOT`: Legal fees (`legal_fees`)

- `F9_09_EXP_FEE_SVC_ACC_TOT`: Accounting fees (`accounting_fees`)

- `F9_09_EXP_FEE_SVC_LOB_TOT`: Lobbying fees (`lobbying_fees`)

- `F9_09_EXP_FEE_SVC_FUNDR_TOT`: Professional fundraising fees
  (`prof_fundraising_fees`)

- `F9_09_EXP_FEE_SVC_INVEST_TOT`: Investment management fees
  (`investment_mgmt_fees`)

- `F9_09_EXP_FEE_SVC_OTH_TOT`: Other fees for services (`other_fees`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_expenses_feesforservice_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_feesforservice", "expenses_feesforservice_w", "expenses_feesforservice_z", "expenses_feesforservice_p" ) ] )
#>   expenses_feesforservice expenses_feesforservice_w expenses_feesforservice_z
#> 1                      NA                        NA                        NA
#> 2              0.00000000                0.00000000                -2.4378773
#> 3                      NA                        NA                        NA
#> 4              0.02869526                0.02869526                -0.2757575
#> 5                      NA                        NA                        NA
#> 6              1.00000000                0.96530569                 3.0751296
#>   expenses_feesforservice_p
#> 1                        NA
#> 2                         1
#> 3                        NA
#> 4                        47
#> 5                        NA
#> 6                       100
```
