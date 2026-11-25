# Pre-Depreciation Profitability Margin

Operating surplus or deficit as a share of total revenue, before
depreciation.

**Formula:**

    predpm = ( total_revenue - ( total_expenses - depreciation ) ) / total_revenue

**Definitional Range**

Bounded above at 1.0; unbounded below. Always equal to or greater than
the post-depreciation margin
([`get_profit_margin_postdepr()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_profit_margin_postdepr.md))
since adding back depreciation can only increase the margin. The typical
range is approximately \[-0.20, 0.40\].

**Benchmarks and rules of thumb**

- A pre-depreciation margin above zero with a post-depreciation margin
  below zero is common and generally manageable for capital-intensive
  nonprofits.

- If both margins are negative, the organization is generating a true
  cash flow deficit, not just an accounting loss.

**Calculated For:** 990 filers only.

## Usage

``` r
get_profit_margin_predepr( df,
  revenue      = "F9_08_REV_TOT_TOT",
  expenses     = "F9_09_EXP_TOT_TOT",
  depreciation = "F9_09_EXP_DEPREC_TOT",
  numerator = NULL, denominator = NULL, winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- revenue:

  Total revenue.

- expenses:

  Total functional expenses.

- depreciation:

  Depreciation, depletion, and amortization.

- numerator:

  Optional. A pre-calculated column for the numerator (revenue minus
  non-depreciation expenses). When supplied, `expenses` and
  `depreciation` are ignored, and `revenue` is used only for the
  denominator; it is an error to also set `expenses` or `depreciation`
  explicitly.

- denominator:

  Optional. A pre-calculated column for the denominator. When supplied,
  `revenue` is used only for the numerator. If both `numerator` and
  `denominator` are supplied, `revenue` is ignored and it is an error to
  set it explicitly.

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

    - `profit_predepr`   - pre-depreciation profitability margin (raw)
    - `profit_predepr_w` - winsorized version
    - `profit_predepr_z` - standardized z-score (based on winsorized values)
    - `profit_predepr_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The pre-depreciation profit margin approximates cash flow from
operations by adding back non-cash depreciation charges to the surplus.
Since depreciation is an accounting allocation of past capital
expenditure rather than a current cash outflow, this measure better
reflects the organization's actual cash-generating ability. A positive
pre-depreciation margin alongside a negative post-depreciation margin
indicates the organization is cash-flow positive but "booking" a loss
due to large depreciation charges.

### Formula variations and their sources

(Total revenue - (Total expenses - Depreciation)) / Total revenue.
Equivalently, (revenue - expenses + depreciation) / revenue. This is the
simplest cash flow approximation available from 990 data; it does not
account for changes in working capital, capital expenditures, or debt
service, which are not directly reported on the 990.

The EBITDA margin used in commercial analysis is similar in spirit but
also adds back interest, taxes, and amortization. The pre-depreciation
margin here adds only depreciation (and amortization where included),
making it a more conservative approximation.

### Canonical citations

- Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005).
  Assessing financial vulnerability in the nonprofit sector. *Harvard
  Business School Working Paper 04-016*.

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley.

### Variables used:

- `F9_08_REV_TOT_TOT`: Total revenue (`revenue`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`expenses`)

- `F9_09_EXP_DEPREC_TOT`: Depreciation and amortization (`depreciation`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_profit_margin_predepr( df = dat10k )
#>    :: Revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "profit_predepr", "profit_predepr_w", "profit_predepr_z", "profit_predepr_p" ) ] )
#>   profit_predepr profit_predepr_w profit_predepr_z profit_predepr_p
#> 1             NA               NA               NA               NA
#> 2     -0.8396223       -0.8396223        -1.768022                4
#> 3             NA               NA               NA               NA
#> 4     -0.1119100       -0.1119100        -0.935851               18
#> 5             NA               NA               NA               NA
#> 6            NaN              NaN               NA               NA
```
