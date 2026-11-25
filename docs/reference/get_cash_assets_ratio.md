# Cash and Savings to Assets Ratio

Cash and savings as a share of total assets; measures the liquid
composition of the asset base.

**Formula:**

    cash_assets = ( cash + savings ) / total_assets

**Definitional Range**

Bounded \[0, 1\]: zero means no liquid assets; one means the entire
asset base is cash and savings. In practice most nonprofits fall in the
\[0.05, 0.50\] range. Values above 0.60 are uncommon for operating
nonprofits and may indicate excess cash hoarding.

**Benchmarks and rules of thumb**

- No universally accepted benchmark. Values below 0.05 suggest minimal
  liquid cushion; values above 0.60 may indicate under-deployment of
  assets toward mission.

- Foundations and endowed institutions naturally show higher ratios than
  operating nonprofits.

**Calculated For:** 990 filers only.

## Usage

``` r
get_cash_assets_ratio( df,
  cash                      = "F9_10_ASSET_CASH_EOY",
  total_assets              = "F9_10_ASSET_TOT_EOY",
  savings                   = "F9_10_ASSET_SAVING_EOY",
  winsorize  = 0.98 ,
  range     = "zp",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- cash:

  Cash on hand, EOY.

- total_assets:

  Total assets, EOY.

- savings:

  Savings and temporary cash investments, EOY.

- winsorize:

  Winsorization proportion between 0 and 1 (default `0.98`).

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"zp"`. Options: `"np"` (negative
  to positive), `"zp"` (zero to positive), `"zo"` (zero to one), `"nz"`
  (negative to zero), or a custom `"lo;hi"` pair (e.g. `"0;10"`).

- sanitize:

  Logical (default `TRUE`). If `TRUE`, imputes zero for NA financial
  fields before computing, respecting form scope.

- summarize:

  Logical (default `FALSE`). If `TRUE`, prints summary statistics and
  density plots for all four output columns.

## Value

The original `data.frame` with four columns appended: `cash_assets`,
`cash_assets_w`, `cash_assets_z`, `cash_assets_p`.

## Details

### Primary uses and key insights

The cash and savings to assets ratio measures what fraction of an
organization's total asset base is held in immediately liquid form. It
is an asset composition indicator rather than a pure liquidity test: a
high ratio means the organization holds a large share of its resources
in cash (potentially signalling under-investment in mission-related
assets), while a low ratio may indicate heavy investment in fixed assets
or program-related investments.

This metric is distinct from
[`get_cash_liquidity_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_cash_liquidity_ratio.md),
which compares the same cash+savings numerator to current liabilities (a
coverage test), and from
[`get_cash_on_hand()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_cash_on_hand.md),
which reports the dollar amount rather than a proportion.

### Formula variations and their sources

The Stata-derived version of this ratio (cs_ta) used in nonprofit
financial structure studies uses exactly this formulation: (cash +
savings) / total assets EOY. Some balance sheet decomposition studies
include pledges receivable and accounts receivable in the numerator to
capture all near-liquid items, but this implementation follows the
narrower two-item definition to maintain comparability with the cash
ratio literature.

### Why this formula was chosen

Cash (line 1B) and savings (line 2B) are the two most reliable, narrowly
liquid items on the 990 balance sheet. Total assets (line 16B) is the
standard denominator for asset composition ratios. This pairing cleanly
answers the question: what share of total resources is in immediately
spendable form?

### Canonical citations

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

- Frumkin, P. & Keating, E.K. (2001). The price of doing good: Executive
  compensation in nonprofit organizations. *Policy and Society*, 20(4),
  94-112.

### Variables used:

- `F9_10_ASSET_CASH_EOY`: Cash on hand, EOY (`cash`)

- `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY
  (`savings`)

- `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_cash_assets_ratio( df = dat10k )
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
head( d[ , c( "cash_assets", "cash_assets_w", "cash_assets_z", "cash_assets_p" ) ] )
#>   cash_assets cash_assets_w cash_assets_z cash_assets_p
#> 1          NA            NA            NA            NA
#> 2   0.2768345     0.2768345   -0.09472221            39
#> 3          NA            NA            NA            NA
#> 4   0.1887254     0.1887254   -0.30571477            32
#> 5          NA            NA            NA            NA
#> 6   1.0000000     1.0000000    2.95448971            81
```
