# Current Ratio

Measures short-term liquidity by comparing current assets to current
liabilities.

**Formula:**

    cr = current_assets / current_liabilities

    current_assets      = cash + savings + pledges_receivable + accounts_receivable
                          + inventories + prepaid_expenses
    current_liabilities = accounts_payable + grants_payable

**Definitional Range**

The current ratio is bounded below at zero and unbounded above. Values
below 1.0 mean current liabilities exceed current assets. The empirical
range for nonprofits is approximately \[0, 15\], with most organizations
clustered between 0.5 and 5.

**Benchmarks and rules of thumb**

- **1.0**: Conventional adequacy threshold – liabilities exactly
  covered.

- **Below 0.5**: Indicates acute short-term risk (Tuckman & Chang 1991).

- **Above 3.0-4.0**: May indicate excess cash or slow receivables
  collection.

- Sector medians generally fall in the \[1.0, 2.5\] range (Zietlow et
  al. 2007).

**Calculated For:** 990 filers only.

## Usage

``` r
get_current_ratio( df,
  cash                = "F9_10_ASSET_CASH_EOY",
  savings             = "F9_10_ASSET_SAVING_EOY",
  pledges_receivable  = "F9_10_ASSET_PLEDGE_NET_EOY",
  accounts_receivable = "F9_10_ASSET_ACC_NET_EOY",
  inventories         = "F9_10_ASSET_INV_SALE_EOY",
  prepaid_expenses    = "F9_10_ASSET_EXP_PREPAID_EOY",
  accounts_payable    = "F9_10_LIAB_ACC_PAYABLE_EOY",
  grants_payable      = "F9_10_LIAB_GRANT_PAYABLE_EOY",
  numerator = NULL, denominator = NULL, winsorize = 0.98 ,
  range     = "zp",
  sanitize  = TRUE,
  summarize = FALSE,
  investment_sales = NULL )
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

- inventories:

  Inventories for sale or use, EOY (Part X line 8).

- prepaid_expenses:

  Prepaid expenses and deferred charges, EOY.

- accounts_payable:

  Accounts payable and accrued expenses, EOY.

- grants_payable:

  Grants and similar amounts payable, EOY.

- numerator:

  Optional. A pre-aggregated column name for current assets. When
  supplied, the asset component arguments are ignored (it is an error to
  also set one of them explicitly).

- denominator:

  Optional. A pre-aggregated column name for current liabilities. When
  supplied, `accounts_payable` and `grants_payable` are ignored (it is
  an error to also set one of them explicitly).

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

- investment_sales:

  Deprecated alias for `inventories`, retained for backward
  compatibility. The field it defaulted to, `F9_10_ASSET_INV_SALE_EOY`,
  is inventories, not investments.

## Value

Object of class `data.frame`: the original dataframe appended with four
new columns:

    - `current`   - current ratio (raw)
    - `current_w` - winsorized version
    - `current_z` - standardized z-score (based on winsorized values)
    - `current_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The current ratio is the most widely used measure of short-term
liquidity in both commercial and nonprofit finance. It asks whether an
organization has sufficient near-term assets to meet its near-term
obligations without requiring asset sales, additional borrowing, or
emergency fundraising. For nonprofits it is particularly important as a
stress-test, because grant revenue often arrives in irregular cycles
that do not align with payroll and vendor payment schedules.

Unlike the quick ratio
([`get_quick_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_quick_ratio.md)),
the current ratio includes prepaid expenses and inventories, making it a
somewhat more generous (and less conservative) measure of liquidity. In
practice for nonprofits, the two ratios often move together since
prepaid expenses are rarely a large share of assets.

### Formula variations and their sources

The standard commercial definition (current assets / current
liabilities) uses balance sheet categories that are explicitly labelled
as current. The 990 balance sheet (Part X) does not use current vs.
non-current labels. This implementation follows the operationalization
in Tuckman & Chang (1991) and subsequent studies by defining:

- **Current assets**: cash + savings + pledges receivable + accounts
  receivable + inventories for sale or use + prepaid expenses (Part X
  lines 1, 2, 3, 4, 8, 9).

- **Current liabilities**: accounts payable + grants payable (Part X
  lines 17, 18). Mortgage and note obligations are excluded as
  long-term.

Some analysts also include publicly traded securities (Part X line 11)
in current assets as marketable investments; they are excluded here.

### Why this formula was chosen

The asset components selected are the most reliably current items
available on the 990 balance sheet. The liability components exclude
long-term debt, which matches the intent of the current ratio (near-term
coverage) while working within the structural constraints of the 990.
This formulation is consistent with the majority of nonprofit financial
health studies and the NCCS financial indicators project.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial
  vulnerability of charitable organizations. *Nonprofit Management and
  Leadership*, 11(2), 199-210.

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley.

### Variables used:

- `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)

- `F9_10_ASSET_SAVING_EOY`: Savings and short-term investments, EOY
  (`savings`)

- `F9_10_ASSET_PLEDGE_NET_EOY`: Net pledges receivable, EOY
  (`pledges_receivable`)

- `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY
  (`accounts_receivable`)

- `F9_10_ASSET_INV_SALE_EOY`: Inventories for sale or use, EOY (Part X
  line 8) (`inventories`)

- `F9_10_ASSET_EXP_PREPAID_EOY`: Prepaid expenses and deferred charges,
  EOY (`prepaid_expenses`)

- `F9_10_LIAB_ACC_PAYABLE_EOY`: Accounts payable and accrued expenses,
  EOY (`accounts_payable`)

- `F9_10_LIAB_GRANT_PAYABLE_EOY`: Grants and similar amounts payable,
  EOY (`grants_payable`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_current_ratio( df = dat10k )
#>    :: Current liabilities equal to zero :: 2,490 case(s) replaced with NaN
head( d[ , c( "current", "current_w", "current_z", "current_p" ) ] )
#>    current current_w  current_z current_p
#> 1       NA        NA         NA        NA
#> 2      NaN       NaN         NA        NA
#> 3       NA        NA         NA        NA
#> 4 2.468337  2.468337 -0.9275652        18
#> 5       NA        NA         NA        NA
#> 6 5.228571  5.228571 -0.4996621        31
```
