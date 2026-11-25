# Land, Buildings, and Equipment to Assets Ratio (Net)

Net land, buildings, and equipment as a share of total assets.

**Formula:**

    land_assets_net = land_bldg_equip_net / total_assets

**Definitional Range**

Bounded \[0, 1\] in normal conditions. Heavily skewed toward zero
because many nonprofits own no real property.

**Benchmarks and rules of thumb**

- Values below 0.10 are typical for lean service organizations.

- Values above 0.50 indicate that more than half of assets are fixed
  property, limiting financial flexibility.

- Declining values over time may reflect depreciation outpacing new
  capital investment rather than strategic deleveraging.

**Calculated For:** 990 + 990EZ filers.

## Usage

``` r
get_land_assets_net_ratio( df,
  land_bldg_equip_net       = "F9_10_ASSET_LAND_BLDG_NET_EOY",
  total_assets              = "F9_10_ASSET_TOT_EOY",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- land_bldg_equip_net:

  Net land, buildings, and equipment EOY (after accumulated
  depreciation).

- total_assets:

  Total assets, EOY.

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

The original `data.frame` with four columns appended: `land_assets_net`,
`land_assets_net_w`, `land_assets_net_z`, `land_assets_net_p`.

## Details

### Primary uses and key insights

The land, buildings, and equipment to assets ratio (net version)
measures what share of total assets is represented by the net book value
of fixed physical assets after accumulated depreciation. This is the
standard version used in most nonprofit financial analyses, since it
reflects current accounting value. High values indicate
capital-intensive organizations; low values indicate lean service
organizations. Very high values (above 0.60) may suggest illiquidity
risk. Available on both 990 and 990EZ forms (PZ scope).

### Formula variations and their sources

Net land, buildings, and equipment (Part X line 10cB) / total assets
(line 16B). The gross version
([`get_land_assets_gross_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_land_assets_gross_ratio.md))
uses accumulated depreciation instead of net book value.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

### Variables used:

- `F9_10_ASSET_LAND_BLDG_NET_EOY`: Net land, buildings, and equipment,
  EOY (`land_bldg_equip_net`)

- `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_land_assets_net_ratio( df = dat10k )
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
head( d[ , c( "land_assets_net", "land_assets_net_w", "land_assets_net_z", "land_assets_net_p" ) ] )
#>   land_assets_net land_assets_net_w land_assets_net_z land_assets_net_p
#> 1       0.0000000         0.0000000        -2.7866109                 1
#> 2       0.7231655         0.7231655         0.7440508                93
#> 3       0.0000000         0.0000000        -2.7866109                 1
#> 4       0.7169119         0.7169119         0.7312804                93
#> 5             NaN               NaN               NaN                NA
#> 6       0.0000000         0.0000000        -2.7866109                 1
```
