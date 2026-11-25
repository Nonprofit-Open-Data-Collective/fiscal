# Cash Ratio

Measures immediate liquidity using only cash and cash equivalents
against current liabilities.

**Formula:**

    casr = ( cash + savings ) / current_liabilities

    current_liabilities = accounts_payable + grants_payable

**Definitional Range**

Bounded below at zero and unbounded above. A ratio of 1.0 means cash
exactly covers current payables. The empirical range for nonprofits is
approximately \[0, 20\], with most values between 0.1 and 5.0. Very high
values are common for grant-making foundations and endowed organizations
that carry minimal accounts payable.

**Benchmarks and rules of thumb**

- **Below 0.5**: Organization cannot cover half of near-term payables
  from cash alone; reliant on receivables collection or borrowing.

- **1.0 or above**: Full cash coverage of current liabilities.

- Because this is a highly conservative measure, values somewhat below
  1.0 are normal if receivables are healthy.

**Calculated For:** 990 filers only.

## Usage

``` r
get_cash_liquidity_ratio( df,
  cash             = "F9_10_ASSET_CASH_EOY",
  savings          = "F9_10_ASSET_SAVING_EOY",
  accounts_payable = "F9_10_LIAB_ACC_PAYABLE_EOY",
  grants_payable   = "F9_10_LIAB_GRANT_PAYABLE_EOY",
  winsorize = 0.98 ,
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

- accounts_payable:

  Accounts payable and accrued expenses, EOY.

- grants_payable:

  Grants and similar amounts payable, EOY.

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

    - `cash_liq`   - cash ratio (raw)
    - `cash_liq_w` - winsorized version
    - `cash_liq_z` - standardized z-score (based on winsorized values)
    - `cash_liq_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The cash liquidity ratio is the most conservative liquidity test: it
asks whether an organization could cover its most immediate obligations
using only cash and savings, without collecting any receivables or
selling any investments. It is sometimes called the absolute liquidity
ratio or cash coverage ratio. For nonprofits with long or uncertain
receivables collection cycles (e.g., government reimbursement contracts,
multi-year pledge collections), it provides a pessimistic but useful
lower bound on liquidity.

This ratio is stricter than both the quick ratio
([`get_quick_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_quick_ratio.md))
and the current ratio
([`get_current_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_current_ratio.md)),
which both include receivables in the numerator.

### Formula variations and their sources

The commercial equivalent (sometimes called the "super-quick" ratio)
uses only cash and marketable securities. The nonprofit version here
uses cash (line 1B) plus savings/temporary investments (line 2B) as the
numerator, and accounts payable plus grants payable as the denominator.
This follows the same liability proxy used across the package (Tuckman &
Chang 1991).

### Why this formula was chosen

Cash and savings are the two fields on the 990 that most unambiguously
represent immediately available funds. The denominator (accounts
payable + grants payable) represents the most pressing and legally
enforceable near-term obligations. Together this pair provides the most
conservative 990-based liquidity measure available.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley.

### Variables used:

- `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)

- `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY
  (`savings`)

- `F9_10_LIAB_ACC_PAYABLE_EOY`: Accounts payable and accrued expenses,
  EOY (`accounts_payable`)

- `F9_10_LIAB_GRANT_PAYABLE_EOY`: Grants and similar amounts payable,
  EOY (`grants_payable`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_cash_liquidity_ratio( df = dat10k )
#>    :: Current liabilities equal to zero :: 2,483 case(s) replaced with NaN
head( d[ , c( "cash_liq", "cash_liq_w", "cash_liq_z", "cash_liq_p" ) ] )
#>   cash_liq cash_liq_w cash_liq_z cash_liq_p
#> 1       NA         NA         NA         NA
#> 2      NaN        NaN         NA         NA
#> 3       NA         NA         NA         NA
#> 4 1.645558   1.645558 -0.7946472         21
#> 5       NA         NA         NA         NA
#> 6 5.228571   5.228571 -0.2824576         39
```
