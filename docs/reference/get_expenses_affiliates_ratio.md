# Payments to Affiliates Ratio

Payments to affiliates as a share of total expenses.

**Formula:**

    expenses_affiliates = payments_to_affiliates / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Near zero for standalone organizations; potentially
above 0.50 for federated intermediaries.

**Benchmarks and rules of thumb**

- Interpret in the context of organizational structure.

- Large payments to affiliates in the absence of a known federated
  structure warrant review of related-party transaction disclosures.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_affiliates_ratio( df,
  payments_to_affiliates    = "F9_09_EXP_PAY_AFFIL_TOT",
  total_expenses            = "F9_09_EXP_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- payments_to_affiliates:

  Payments to affiliates (total).

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

  Logical (default `TRUE`). If `TRUE`, imputes zero for NA financial
  fields before computing, respecting form scope.

- summarize:

  Logical (default `FALSE`). If `TRUE`, prints summary statistics and
  density plots for all four output columns.

## Value

The original `data.frame` with four new columns:

- `expenses_affiliates` (raw ratio)

- `expenses_affiliates_w` (winsorized)

- `expenses_affiliates_z` (z-score)

- `expenses_affiliates_p` (percentile rank, 1-100)

## Details

### Primary uses and key insights

The payments to affiliates ratio measures the share of total functional
expenses transferred to affiliated organizations. It captures
inter-organizational resource flows within nonprofit families (federated
structures, supporting organizations, shared-services models).

### Formula

Payments to affiliates (Part IX line 21, Column A) / total expenses
(line 25A).

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

### Variables used:

- `F9_09_EXP_PAY_AFFIL_TOT`: Payments to affiliates
  (`payments_to_affiliates`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_expenses_affiliates_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_affiliates", "expenses_affiliates_w", "expenses_affiliates_z", "expenses_affiliates_p" ) ] )
#>   expenses_affiliates expenses_affiliates_w expenses_affiliates_z
#> 1                  NA                    NA                    NA
#> 2                   0                     0             -2.659065
#> 3                  NA                    NA                    NA
#> 4                   0                     0             -2.659065
#> 5                  NA                    NA                    NA
#> 6                   0                     0             -2.659065
#>   expenses_affiliates_p
#> 1                    NA
#> 2                     1
#> 3                    NA
#> 4                     1
#> 5                    NA
#> 6                     1
```
