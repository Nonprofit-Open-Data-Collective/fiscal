# Member Benefits Expense Ratio

Benefits paid to or for members as a share of total expenses.

**Formula:**

    expenses_membbenefits = member_benefits / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Near zero for most public benefit charities;
potentially high (above 0.50) for mutual benefit organizations.

**Benchmarks and rules of thumb**

- Interpret in the context of the organization's exempt purpose and
  subsection code.

- Non-zero values for 501(c)(3) organizations should be reviewed for
  compliance with private benefit restrictions.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_membbenefits_ratio( df,
  member_benefits           = "F9_09_EXP_BEN_PAID_MEMB_TOT",
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

- member_benefits:

  Benefits paid to or for members (total).

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

- `expenses_membbenefits` (raw ratio)

- `expenses_membbenefits_w` (winsorized)

- `expenses_membbenefits_z` (z-score)

- `expenses_membbenefits_p` (percentile rank, 1-100)

## Details

### Primary uses and key insights

The member benefits expense ratio measures the share of total functional
expenses paid out as direct benefits to members. Primarily relevant for
membership associations (unions, trade associations, professional
societies) where member benefit payments are a core activity. For most
public benefit nonprofits this ratio is near zero.

### Formula

Benefits paid to or for members (Part IX line 4, Column A) / total
expenses (line 25A). The numerator field is PZ scope, but the
denominator (`F9_09_EXP_TOT_TOT`) is reported only on the full 990, so
the ratio is calculated for 990 filers only.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

### Variables used:

- `F9_09_EXP_BEN_PAID_MEMB_TOT`: Benefits paid to members
  (`member_benefits`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_expenses_membbenefits_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_membbenefits", "expenses_membbenefits_w", "expenses_membbenefits_z", "expenses_membbenefits_p" ) ] )
#>   expenses_membbenefits expenses_membbenefits_w expenses_membbenefits_z
#> 1                    NA                      NA                      NA
#> 2                     0                       0               -1.616375
#> 3                    NA                      NA                      NA
#> 4                     0                       0               -1.616375
#> 5                    NA                      NA                      NA
#> 6                     0                       0               -1.616375
#>   expenses_membbenefits_p
#> 1                      NA
#> 2                       1
#> 3                      NA
#> 4                       1
#> 5                      NA
#> 6                       1
```
