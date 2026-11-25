# Months of Operating Cash on Hand

Number of months the organization can operate using available liquid
assets.

**Formula:**

    moch = liquid_assets / monthly_expenses

    liquid_assets    = cash + savings + pledges_receivable + accounts_receivable
    monthly_expenses = ( total_expenses - depreciation ) / 12

**Definitional Range**

Bounded below at zero; unbounded above. The typical operating range for
nonprofits is approximately \[0, 24\] months, with values above 12
months uncommon for operating organizations (more typical for
foundations).

**Benchmarks and rules of thumb**

- **Less than 1 month**: Acute liquidity risk.

- **1-3 months**: Below adequate; common guidance is at least 3 months.

- **3-6 months**: Generally considered healthy for most operating
  nonprofits.

- **6+ months**: Strong reserve position.

- The Nonprofit Finance Fund recommends 3-6 months as a target.

**Calculated For:** 990 filers only.

## Usage

``` r
get_months_cash_operations( df,
  cash                 = "F9_10_ASSET_CASH_EOY",
  savings              = "F9_10_ASSET_SAVING_EOY",
  pledges_receivable   = "F9_10_ASSET_PLEDGE_NET_EOY",
  accounts_receivable  = "F9_10_ASSET_ACC_NET_EOY",
  total_expenses       = "F9_09_EXP_TOT_TOT",
  depreciation         = "F9_09_EXP_DEPREC_TOT",
  numerator = NULL, denominator = NULL, winsorize = 0.98 ,
  range     = "zp",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- cash:

  Cash on hand, EOY.

- savings:

  Short-term investments (savings), EOY.

- pledges_receivable:

  Pledges and grants receivable, EOY.

- accounts_receivable:

  Accounts receivable, EOY.

- total_expenses:

  Total functional expenses.

- depreciation:

  Depreciation, depletion, and amortization.

- numerator:

  Optional. A pre-aggregated column name for liquid assets. When
  supplied, `cash`, `savings`, `pledges_receivable`,
  `accounts_receivable` are ignored; it is an error to also set one of
  them explicitly.

- denominator:

  Optional. A pre-aggregated column name for the denominator (annual
  non-depreciation expenses). The function divides this by 12
  internally. When supplied, `total_expenses`, `depreciation` are
  ignored; it is an error to also set one of them explicitly.

- winsorize:

  The winsorization value (between 0 and 1), defaults to 0.98, which
  winsorizes at the 1st and 99th percentiles.

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"zp"`. Options: `"np"` (negative
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

    - `months_cash_ops`   - months of operating cash on hand (raw)
    - `months_cash_ops_w` - winsorized version
    - `months_cash_ops_z` - standardized z-score (based on winsorized values)
    - `months_cash_ops_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

Months of cash on hand is the monthly expression of
[`get_days_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_days_cash_operations.md),
expressing the same liquidity concept in a unit that is more natural for
annual budget planning and board reporting. It is one of the most
commonly reported metrics in nonprofit financial dashboards and funder
due diligence.

### Formula variations and their sources

The denominator uses (total expenses - depreciation) / 12. Subtracting
depreciation follows standard practice (Zietlow et al. 2007) because the
ratio measures cash coverage of cash expenses. Some formulations use
total expenses without the depreciation adjustment; others use a rolling
12-month average of monthly expenses to smooth seasonal variation, which
is not possible with annual 990 data.

### Canonical citations

- Nonprofit Finance Fund. *State of the Nonprofit Sector Survey*
  (annual).

  - Uses months of cash as a primary financial health indicator.

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley.

- GuideStar/Candid. *Nonprofit Finance Indicators*. - Months of cash is
  one of the core metrics in the GuideStar financial health profile.

### Variables used:

- `F9_10_ASSET_CASH_EOY`: Cash on hand (`cash`)

- `F9_10_ASSET_SAVING_EOY`: Savings (`savings`)

- `F9_10_ASSET_PLEDGE_NET_EOY`: Net pledges receivable
  (`pledges_receivable`)

- `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable (`accounts_receivable`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

- `F9_09_EXP_DEPREC_TOT`: Depreciation and amortization (`depreciation`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_months_cash_operations( df = dat10k )
#>    :: Monthly operating expenses equal to zero :: 63 case(s) replaced with NaN
head( d[ , c( "months_cash_ops", "months_cash_ops_w", "months_cash_ops_z", "months_cash_ops_p" ) ] )
#>   months_cash_ops months_cash_ops_w months_cash_ops_z months_cash_ops_p
#> 1              NA                NA                NA                NA
#> 2      41.3263838        41.3263838         2.0815954                92
#> 3              NA                NA                NA                NA
#> 4       0.5622686         0.5622686        -0.9977032                 8
#> 5              NA                NA                NA                NA
#> 6      29.2800000        29.2800000         1.6570830                88
```
