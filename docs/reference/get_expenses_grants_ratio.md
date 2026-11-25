# Grants-to-Others Expense Ratio

Total grants paid to domestic organizations, domestic individuals, and
foreign entities as a share of total functional expenses.

**Formula:**

    expenses_grants = ( us_org_grants + us_indiv_grants + foreign_grants )
                      / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Near zero for direct-service organizations; near 1.0
for pure pass-through grant-makers.

**Benchmarks and rules of thumb**

- Subsector comparisons are essential: a 0.80 ratio is expected for a
  community foundation; the same value would be anomalous for a
  hospital.

- Rising ratios over time may indicate a strategic shift toward a more
  redistributive model.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_grants_ratio( df,
  us_org_grants    = "F9_09_EXP_GRANT_US_ORG_TOT",
  us_indiv_grants  = "F9_09_EXP_GRANT_US_INDIV_TOT",
  foreign_grants   = "F9_09_EXP_GRANT_FRGN_TOT",
  total_expenses   = "F9_09_EXP_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- us_org_grants:

  Grants to domestic organizations and governments (total).

- us_indiv_grants:

  Grants and assistance to domestic individuals (total).

- foreign_grants:

  Grants to foreign organizations and governments (total).

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

The original `data.frame` with four columns appended: `expenses_grants`,
`expenses_grants_w`, `expenses_grants_z`, `expenses_grants_p`.

## Details

### Primary uses and key insights

The grants-to-others ratio measures the share of total expenses paid out
as grants to other organizations, individuals, or foreign entities. It
is the defining characteristic of grant-making and pass-through
organizations - foundations, federated funders, and community chests -
where the primary activity is resource redistribution rather than direct
service delivery.

### Formula

(Domestic org grants + domestic individual grants + foreign grants) /
total expenses. Note: US organization grants (line 1) are PZ scope;
other components are PC scope.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

### Variables used:

- `F9_09_EXP_GRANT_US_ORG_TOT`: Grants to domestic organizations
  (`us_org_grants`)

- `F9_09_EXP_GRANT_US_INDIV_TOT`: Grants to domestic individuals
  (`us_indiv_grants`)

- `F9_09_EXP_GRANT_FRGN_TOT`: Grants to foreign entities
  (`foreign_grants`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_expenses_grants_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_grants", "expenses_grants_w", "expenses_grants_z", "expenses_grants_p" ) ] )
#>   expenses_grants expenses_grants_w expenses_grants_z expenses_grants_p
#> 1              NA                NA                NA                NA
#> 2               0                 0         -2.085138                 1
#> 3              NA                NA                NA                NA
#> 4               0                 0         -2.085138                 1
#> 5              NA                NA                NA                NA
#> 6               0                 0         -2.085138                 1
```
