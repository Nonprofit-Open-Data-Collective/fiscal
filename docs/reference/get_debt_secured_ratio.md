# Secured Debt Ratio

Secured mortgages and notes payable as a share of total liabilities.

**Formula:**

    debt_secured = secured_mortgages_notes / total_liabilities

**Definitional Range**

Bounded \[0, 1\]. Most nonprofits have zero secured debt;
capital-intensive organizations (housing, healthcare, higher education)
can show values above 0.70.

**Benchmarks and rules of thumb**

- No standard benchmark. Most informative when tracked over time or
  compared within subsectors with similar capital structures.

- For organizations with high secured debt ratios, lenders will
  scrutinize the collateral value and debt service coverage ratio.

**Calculated For:** 990 filers only.

## Usage

``` r
get_debt_secured_ratio( df,
  secured_mortgages_notes   = "F9_10_LIAB_MTG_NOTE_EOY",
  total_liabilities         = "F9_10_LIAB_TOT_EOY",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- secured_mortgages_notes:

  Secured mortgages and notes payable, EOY.

- total_liabilities:

  Total liabilities, EOY.

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

The original `data.frame` with four columns appended: `debt_secured`,
`debt_secured_w`, `debt_secured_z`, `debt_secured_p`.

## Details

### Primary uses and key insights

The secured debt ratio measures what share of total liabilities consists
of mortgages and notes payable secured by collateral. It is a liability
composition indicator: a high ratio means the organization's debt is
predominantly secured (usually real estate mortgages), while a low ratio
means most debt is unsecured.

This metric is useful for understanding the nature of an organization's
leverage, since secured debt typically has lower interest rates but
pledges specific assets as collateral, limiting flexibility. It is
particularly relevant for housing nonprofits, healthcare organizations,
and educational institutions.

### Formula variations and their sources

Secured mortgages and notes payable (Part X line 23B) / total
liabilities (line 26B). The complement is approximately
[`get_debt_unsecured_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_debt_unsecured_ratio.md)
(line 24B / line 26B), though the two do not sum to 1.0 because there
are other liability categories.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good: Executive
  compensation in nonprofit organizations. *Policy and Society*, 20(4),
  94-112.

### Variables used:

- `F9_10_LIAB_MTG_NOTE_EOY`: Secured mortgages and notes payable, EOY
  (`secured_mortgages_notes`)

- `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`total_liabilities`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_debt_secured_ratio( df = dat10k )
#>    :: Total liabilities equal to zero :: 3,156 case(s) replaced with NaN
head( d[ , c( "debt_secured", "debt_secured_w", "debt_secured_z", "debt_secured_p" ) ] )
#>   debt_secured debt_secured_w debt_secured_z debt_secured_p
#> 1           NA             NA             NA             NA
#> 2          NaN            NaN            NaN             NA
#> 3           NA             NA             NA             NA
#> 4            0              0      -2.754481              1
#> 5           NA             NA             NA             NA
#> 6            0              0      -2.754481              1
```
