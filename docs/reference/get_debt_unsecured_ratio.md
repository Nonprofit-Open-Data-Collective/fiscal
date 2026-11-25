# Unsecured Debt Ratio

Unsecured notes and loans payable as a share of total liabilities.

**Formula:**

    debt_unsecured = unsecured_notes_loans / total_liabilities

**Definitional Range**

Bounded \[0, 1\]. Most nonprofits carry no unsecured notes, so the
distribution is highly concentrated at zero with a long right tail.

**Benchmarks and rules of thumb**

- No established benchmark. Presence of unsecured debt is not inherently
  negative – many healthy nonprofits use lines of credit for cash flow
  management – but a large and growing ratio warrants scrutiny.

- Trend analysis is more informative than a single year's value.

**Calculated For:** 990 filers only.

## Usage

``` r
get_debt_unsecured_ratio( df,
  unsecured_notes_loans     = "F9_10_LIAB_NOTE_UNSEC_EOY",
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

- unsecured_notes_loans:

  Unsecured notes and loans payable, EOY.

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

The original `data.frame` with four columns appended: `debt_unsecured`,
`debt_unsecured_w`, `debt_unsecured_z`, `debt_unsecured_p`.

## Details

### Primary uses and key insights

The unsecured debt ratio measures what share of total liabilities
consists of unsecured notes and loans payable (Part X line 24B),
typically including lines of credit and unsecured bank loans. Used
alongside
[`get_debt_secured_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_debt_secured_ratio.md),
it provides a decomposition of the liability structure.

### Formula variations and their sources

Unsecured notes and loans payable (Part X line 24B) / total liabilities
(line 26B). A direct read from two 990 balance sheet fields.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

### Variables used:

- `F9_10_LIAB_NOTE_UNSEC_EOY`: Unsecured notes and loans payable, EOY
  (`unsecured_notes_loans`)

- `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`total_liabilities`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_debt_unsecured_ratio( df = dat10k )
#>    :: Total liabilities equal to zero :: 3,156 case(s) replaced with NaN
head( d[ , c( "debt_unsecured", "debt_unsecured_w", "debt_unsecured_z", "debt_unsecured_p" ) ] )
#>   debt_unsecured debt_unsecured_w debt_unsecured_z debt_unsecured_p
#> 1             NA               NA               NA               NA
#> 2            NaN              NaN              NaN               NA
#> 3             NA               NA               NA               NA
#> 4              0                0        -1.977498                1
#> 5             NA               NA               NA               NA
#> 6              0                0        -1.977498                1
```
