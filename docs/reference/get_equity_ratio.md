# Equity Ratio

Share of total assets financed through net assets (equity).

**Formula:**

    er = total_net_assets / total_assets

**Definitional Range**

Theoretically bounded \[0, 1\] when net assets are positive. Values
above 1.0 are not possible. Values below zero occur when net assets are
negative (accumulated deficit), which is a distress indicator. The
empirical range for most nonprofits is approximately \[0.10, 0.95\].

**Benchmarks and rules of thumb**

- **Above 0.50**: Equity finances more than half of assets – generally
  healthy.

- **Below 0.30**: High leverage; common in capital-intensive subsectors.

- Most useful as a trend measure: declining values warrant
  investigation.

**Calculated For:** 990 + 990EZ filers.

## Usage

``` r
get_equity_ratio( df,
  net_assets   = "F9_10_NAFB_TOT_EOY",
  total_assets = "F9_10_ASSET_TOT_EOY",
  winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- net_assets:

  Total net assets, EOY.

- total_assets:

  Total assets, EOY.

- winsorize:

  The winsorization value (between 0 and 1), defaults to 0.98, which
  winsorizes at the 1st and 99th percentiles.

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"zo"`. Options: `"np"` (negative
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

    - `equity`   - equity ratio (raw)
    - `equity_w` - winsorized version
    - `equity_z` - standardized z-score (based on winsorized values)
    - `equity_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The equity ratio measures what share of total assets is financed by net
assets (organizational equity) rather than liabilities. It is the
arithmetic complement of the debt to asset ratio: equity ratio + debt
ratio = 1.0 (when net assets are positive). A higher equity ratio
indicates greater financial independence and resilience; the
organization owns more of its assets outright.

For nonprofits, the equity ratio is particularly meaningful because net
assets represent the accumulated result of mission-related financial
decisions over time. A declining equity ratio over several years signals
that liabilities are growing faster than assets - a potential
sustainability warning.

### Formula variations and their sources

Total net assets / total assets. Some formulations use unrestricted net
assets in the numerator to focus on the portion of equity truly
available for general operations. This implementation uses total net
assets (restricted + unrestricted) for maximum coverage across both 990
and 990EZ filers; for a more conservative version, see
[`get_netassets_composition_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_netassets_composition_ratio.md).

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

### Variables used:

- `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (`net_assets`)

- `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_equity_ratio( df = dat10k )
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
head( d[ , c( "equity", "equity_w", "equity_z", "equity_p" ) ] )
#>      equity  equity_w   equity_z equity_p
#> 1 0.8983275 0.8983275 -0.5458178       28
#> 2 1.0000000 1.0000000  0.6744908       53
#> 3 1.0000000 1.0000000  0.6744908       53
#> 4 0.8853122 0.8853122 -0.5731253       27
#> 5       NaN       NaN         NA       NA
#> 6 0.8087432 0.8087432 -0.7080045       23
```
