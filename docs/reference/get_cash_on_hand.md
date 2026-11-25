# Cash on Hand

Absolute dollar amount of cash and liquid savings held at end of year.

**Formula:**

    coh = cash + savings

**Definitional Range**

Cash on hand is bounded below at zero (cannot be negative on a properly
prepared balance sheet) and unbounded above. Unlike ratio measures, the
scale is in dollars, so comparison across organizations of different
sizes requires normalization. Use
[`get_months_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_months_cash_operations.md)
or
[`get_days_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_days_cash_operations.md)
for size-adjusted comparisons.

**Benchmarks and rules of thumb**

- As an absolute measure, benchmarks depend on the organization's
  expense base. A common practitioner rule is at least 60-90 days of
  operating expenses in liquid cash.

- The Nonprofit Finance Fund recommends a minimum of three months of
  operating expenses in accessible reserves.

**Calculated For:** 990 filers only.

## Usage

``` r
get_cash_on_hand( df,
  cash    = "F9_10_ASSET_CASH_EOY",
  savings = "F9_10_ASSET_SAVING_EOY",
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

    - `cash_on_hand`   - cash on hand in dollars (raw)
    - `cash_on_hand_w` - winsorized version
    - `cash_on_hand_z` - standardized z-score (based on winsorized values)
    - `cash_on_hand_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

Cash on hand is a simple dollar-denominated liquidity measure rather
than a ratio. It answers the most direct version of the liquidity
question: how much immediately spendable money does the organization
have right now? It is most useful for communicating financial position
to non-technical audiences (boards, funders), for absolute comparison
against known fixed costs (monthly payroll, rent), and as the numerator
input for ratio-based measures such as months of cash
([`get_months_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_months_cash_operations.md))
and days of cash
([`get_days_cash_operations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_days_cash_operations.md)).

### Formula variations and their sources

Some practitioners define cash on hand as cash only (Part X line 1B),
excluding savings. Others include marketable securities or investments
held for sale. This implementation uses cash plus savings (lines 1B +
2B) as the most common definition in the nonprofit financial management
literature (Zietlow et al. 2007), since savings accounts and money
market funds are effectively as liquid as operating cash for most
organizations.

### Why this formula was chosen

Lines 1B (cash) and 2B (savings and temporary cash investments) are the
two most reliably liquid items on the 990 balance sheet and are the
least subject to valuation uncertainty. Pledges and receivables are
excluded because their timing and collectibility are uncertain. This
definition is consistent with the "cash and cash equivalents" concept in
nonprofit financial management practice.

### Canonical citations

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley. - Standard practitioner reference
  for cash management in nonprofits.

- Calabrese, T.D. (2013). Running on empty: The operating reserves of
  U.S. nonprofit organizations. *Nonprofit Management and Leadership*,
  23(3), 281-302. - Examines operating cash and reserves as components
  of financial sustainability.

### Variables used:

- `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)

- `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY
  (`savings`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_cash_on_hand( df = dat10k )
head( d[ , c( "cash_on_hand", "cash_on_hand_w", "cash_on_hand_z", "cash_on_hand_p" ) ] )
#>   cash_on_hand cash_on_hand_w cash_on_hand_z cash_on_hand_p
#> 1           NA             NA             NA             NA
#> 2       173774         173774     -0.2139278             43
#> 3           NA             NA             NA             NA
#> 4         7094           7094     -0.8875391              6
#> 5           NA             NA             NA             NA
#> 6         1830           1830     -0.9105427              3
```
