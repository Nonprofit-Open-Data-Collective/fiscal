# Investment Assets Ratio

Investment securities as a share of total assets.

**Formula:**

    investments_assets = ( pub_traded_securities + other_securities ) / total_assets

**Definitional Range**

Bounded \[0, 1\]. Most operating nonprofits show values near zero;
foundations and endowed institutions may show values above 0.80.

**Benchmarks and rules of thumb**

- No universal benchmark. Operating nonprofits with values above 0.30
  are typically holding significant endowment or reserve portfolios.

- A high investment ratio combined with a high debt ratio may indicate
  borrowing to fund operations while holding investments – worth
  scrutinizing.

**Calculated For:** 990 filers only.

## Usage

``` r
get_investments_assets_ratio( df,
  pub_traded_securities     = "F9_10_ASSET_INVEST_SEC_EOY",
  total_assets              = "F9_10_ASSET_TOT_EOY",
  other_securities          = "F9_10_ASSET_INVEST_SEC_OTH_EOY",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- pub_traded_securities:

  Investments in publicly traded securities, EOY.

- total_assets:

  Total assets, EOY.

- other_securities:

  Investments in other securities, EOY.

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

The original `data.frame` with four new columns:

- `investments_assets` (raw ratio)

- `investments_assets_w` (winsorized)

- `investments_assets_z` (z-score)

- `investments_assets_p` (percentile rank, 1-100)

## Details

### Primary uses and key insights

The investment assets ratio measures what share of total assets is held
in financial securities (publicly traded and other investments). It
distinguishes financially oriented organizations (endowments,
foundations) from operationally oriented ones. A high ratio indicates
significant financial reserves; a low ratio indicates assets are
primarily operational. Most informative for endowed organizations and
foundations.

### Formula variations and their sources

(Publicly traded securities + other securities) / total assets. Both
investment fields (Part X lines 11B and 12B) are PZ scope. A more
comprehensive version would include program-related investments (line
13B).

### Canonical citations

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

- Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
  Leadership*, 23(3), 281-302.

### Variables used:

- `F9_10_ASSET_INVEST_SEC_EOY`: Publicly traded securities, EOY
  (`pub_traded_securities`)

- `F9_10_ASSET_INVEST_SEC_OTH_EOY`: Other securities, EOY
  (`other_securities`)

- `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_investments_assets_ratio( df = dat10k )
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
head( d[ , c( "investments_assets", "investments_assets_w", "investments_assets_z", "investments_assets_p" ) ] )
#>   investments_assets investments_assets_w investments_assets_z
#> 1                 NA                   NA                   NA
#> 2                  0                    0            -3.217002
#> 3                 NA                   NA                   NA
#> 4                  0                    0            -3.217002
#> 5                 NA                   NA                   NA
#> 6                  0                    0            -3.217002
#>   investments_assets_p
#> 1                   NA
#> 2                    1
#> 3                   NA
#> 4                    1
#> 5                   NA
#> 6                    1
```
