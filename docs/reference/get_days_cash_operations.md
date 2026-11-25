# Days of Operating Cash on Hand

Measures how many days an organization can operate using available
liquid assets.

**Formula:**

    doch = liquid_assets / daily_expenses

    liquid_assets = cash + savings + pledges_receivable + accounts_receivable
    daily_expenses = ( total_expenses - depreciation ) / 365

**Definitional Range**

Bounded below at zero and unbounded above, expressed in days. The
typical range for nonprofits is approximately \[0, 365\]. Values above
365 (more than one year of cash) indicate very large liquid reserves
relative to expenses, which is uncommon for operating nonprofits but
typical for foundations.

**Benchmarks and rules of thumb**

- **Below 30 days**: Generally considered a distress indicator.

- **60-90 days**: Commonly cited minimum threshold (Nonprofit Finance
  Fund).

- **90-180 days**: Considered healthy for most operating nonprofits.

- Healthcare nonprofits are typically benchmarked at 150+ days due to
  longer receivables cycles.

**Calculated For:** 990 filers only.

## Usage

``` r
get_days_cash_operations( df,
  cash                = "F9_10_ASSET_CASH_EOY",
  savings             = "F9_10_ASSET_SAVING_EOY",
  pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
  accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
  total_expenses      = "F9_09_EXP_TOT_TOT",
  depreciation        = "F9_09_EXP_DEPREC_TOT",
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

  Net pledges and grants receivable, EOY.

- accounts_receivable:

  Accounts receivable, net, EOY.

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

  Optional. A pre-aggregated column name for annual non-depreciation
  expenses (the function divides this by 365 internally). When supplied,
  `total_expenses`, `depreciation` are ignored; it is an error to also
  set one of them explicitly.

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

    - `days_cash_ops`   - days of operating cash on hand (raw)
    - `days_cash_ops_w` - winsorized version
    - `days_cash_ops_z` - standardized z-score (based on winsorized values)
    - `days_cash_ops_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

Days of cash on hand expresses liquidity in an operationally intuitive
unit: how many days of expenses could the organization cover if all
revenue stopped today? It converts the raw dollar amount of liquid
assets into a time-based runway measure that is easier for non-financial
audiences (boards, program staff, funders) to interpret than a ratio. It
is widely used in nonprofit financial management, healthcare finance,
and government financial reporting.

### Formula variations and their sources

The denominator in this implementation is cash operating expenses per
day, computed as (total expenses - depreciation) / 365. Depreciation is
subtracted because it is a non-cash charge that does not represent
actual cash outflow. Some formulations use total expenses without the
depreciation adjustment, which is more conservative (produces lower days
values). The monthly equivalent is
[`get_months_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_months_cash_operations.md),
which uses /12 instead of /365.

The numerator includes cash, savings, pledges receivable, and accounts
receivable - the same four items used in the quick ratio
([`get_quick_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_quick_ratio.md)).
Some stricter formulations use only cash and savings (excluding
receivables); some broader ones add investments held for sale. The
four-item version follows the Healthcare Financial Management
Association (HFMA) standard, which is the most commonly cited benchmark
source.

### Why this formula was chosen

The HFMA definition is the most widely benchmarked version and is
increasingly applied to nonprofits outside healthcare. Subtracting
depreciation from the denominator follows standard financial management
practice (Zietlow et al. 2007) because the ratio is intended to measure
cash coverage of cash expenses.

### Canonical citations

- Healthcare Financial Management Association (HFMA). *Key Hospital
  Financial Statistics and Ratio Medians*. Annual report. - Source for
  the most widely cited days-of-cash benchmarks; originally
  healthcare-focused but increasingly applied to nonprofits broadly.

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley. - Adopts the HFMA approach for
  nonprofits.

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

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

d <- get_days_cash_operations( df = dat10k )
#>    :: Daily operating expenses equal to zero :: 63 case(s) replaced with NaN
head( d[ , c( "days_cash_ops", "days_cash_ops_w", "days_cash_ops_z", "days_cash_ops_p" ) ] )
#>   days_cash_ops days_cash_ops_w days_cash_ops_z days_cash_ops_p
#> 1            NA              NA              NA              NA
#> 2    1257.01084      1257.01084       2.0815954              92
#> 3            NA              NA              NA              NA
#> 4      17.10234        17.10234      -0.9977032               8
#> 5            NA              NA              NA              NA
#> 6     890.60000       890.60000       1.6570830              88
```
