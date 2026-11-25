# Post-Depreciation Profitability Margin

Operating surplus or deficit as a share of total revenue, after
depreciation.

**Formula:**

    podpm = ( total_revenue - total_expenses ) / total_revenue

**Definitional Range**

Bounded above at 1.0; unbounded below. The typical range for nonprofits
is approximately \[-0.30, 0.30\]. Extreme negative values may reflect
major write-downs or one-time capital expenses.

**Benchmarks and rules of thumb**

- **Near zero**: Normal for nonprofits operating close to break-even.

- **0.02-0.07** (2-7%): Healthy.

- **Below -0.05** for two or more consecutive years: Common
  vulnerability threshold.

- A persistent gap between post- and pre-depreciation margins signals
  capital consumption requiring future capital expenditure.

**Calculated For:** 990 filers only.

## Usage

``` r
get_profit_margin_postdepr( df,
  expenses  = "F9_09_EXP_TOT_TOT",
  revenue   = "F9_08_REV_TOT_TOT",
  winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- expenses:

  Total functional expenses.

- revenue:

  Total revenue.

- winsorize:

  The winsorization value (between 0 and 1), defaults to 0.98, which
  winsorizes at the 1st and 99th percentiles.

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"np"`. Options: `"np"` (negative
  to positive), `"zp"` (zero to positive), `"zo"` (zero to one), `"nz"`
  (negative to zero), or a custom `"lo;hi"` pair (e.g. `"0;10"`).

- sanitize:

  Logical (default `TRUE`). If `TRUE`, NA values in the financial input
  columns are imputed to zero before the ratio is computed, respecting
  form scope: Part X and VIII/IX fields (990 only) are imputed only for
  990 filers; Part I summary fields (990 + 990EZ) are imputed for all
  filers. The returned dataframe always contains the original unmodified
  input columns.

- summarize:

  Logical. If `TRUE`, prints a
  [`summary()`](https://rdrr.io/r/base/summary.html) of the results and
  plots density curves for all four output columns (raw, winsorized,
  z-score, percentile). Defaults to `FALSE`.

## Value

Object of class `data.frame`: the original dataframe appended with four
new columns:

    - `profit_postdepr`   - post-depreciation profitability margin (raw)
    - `profit_postdepr_w` - winsorized version
    - `profit_postdepr_z` - standardized z-score (based on winsorized values)
    - `profit_postdepr_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The post-depreciation profit margin measures the operating surplus or
deficit as a share of total revenue, using expenses that include
depreciation charges. It is the most conservative profit margin measure
because depreciation, while non-cash, represents the consumption of
capital assets that will eventually require replacement.

This measure is closely related to
[`get_surplus_margin_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_surplus_margin_ratio.md)
but uses Part VIII revenue and Part IX expenses directly (both 990-only,
PC scope) rather than the Part I summary line, making it more precise
but narrower in coverage.

### Formula variations and their sources

(Total revenue - Total expenses) / Total revenue, using Part VIII line
12A and Part IX line 25A. The pre-depreciation version
([`get_profit_margin_predepr()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_profit_margin_predepr.md))
adds back depreciation to better approximate cash flow. The PODPM is
also called the operating margin in some sources.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005).
  Assessing financial vulnerability in the nonprofit sector. *Harvard
  Business School Working Paper 04-016*.

### Variables used:

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`expenses`)

- `F9_08_REV_TOT_TOT`: Total revenue (`revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_profit_margin_postdepr( df = dat10k )
#>    :: Revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "profit_postdepr", "profit_postdepr_w", "profit_postdepr_z", "profit_postdepr_p" ) ] )
#>   profit_postdepr profit_postdepr_w profit_postdepr_z profit_postdepr_p
#> 1              NA                NA                NA                NA
#> 2      -0.8396223        -0.8396223        -1.7201124                 5
#> 3              NA                NA                NA                NA
#> 4      -0.1312060        -0.1312060        -0.8691617                20
#> 5              NA                NA                NA                NA
#> 6             NaN               NaN                NA                NA
```
