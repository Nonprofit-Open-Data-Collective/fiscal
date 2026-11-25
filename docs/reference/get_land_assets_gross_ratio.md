# Land Asset Ratio

Share of total assets invested in land, buildings, and equipment.

**Formula:**

    lar = land_buildings_equipment / total_assets

**Definitional Range**

Usually in \[0, 1\], but can exceed 1 when accumulated depreciation is
large relative to other assets. The distribution is highly right-skewed:
most nonprofits with no owned real estate show values near zero, while
capital-intensive organizations (healthcare, housing, higher education)
may show values above 0.50.

**Benchmarks and rules of thumb**

- Most informative for within-subsector comparisons.

- High values indicate illiquidity risk: if the majority of assets are
  fixed property, the organization has limited ability to quickly
  convert assets to cash.

**Calculated For:** 990 filers only.

## Usage

``` r
get_land_assets_gross_ratio( df,
  land_buildings = "F9_10_ASSET_LAND_BLDG",
  total_assets   = "F9_10_ASSET_TOT_EOY",
  winsorize = 0.98 ,
  range     = "zp",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- land_buildings:

  Land, buildings, and equipment at cost or other basis, before
  accumulated depreciation (Part X line 10a).

- total_assets:

  Total assets, EOY.

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

    - `land_assets_gross`   - land asset ratio (raw)
    - `land_assets_gross_w` - winsorized version
    - `land_assets_gross_z` - standardized z-score (based on winsorized values)
    - `land_assets_gross_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The land, buildings, and equipment to assets ratio (gross version)
measures what share of total assets is represented by the gross book
value of fixed physical assets before accumulated depreciation is netted
out. A high ratio indicates a capital-intensive organization with a
large physical footprint; a low ratio indicates a lean service
organization whose assets are primarily financial.

The gross version uses the cost or other basis of land, buildings, and
equipment (Part X line 10a), before accumulated depreciation (line 10b)
is netted out. This is distinct from the net version
([`get_land_assets_net_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_land_assets_net_ratio.md)),
which uses the net book value after depreciation (Part X line 10c). The
gross version gives a better picture of the original investment in fixed
assets; the net version better reflects current book value.

### Formula variations and their sources

F9_10_ASSET_LAND_BLDG (cost or other basis, line 10a) / total assets.
Because total assets carry fixed assets at net book value, the ratio can
exceed 1 for heavily depreciated organizations. Earlier versions used
accumulated depreciation (`F9_10_ASSET_LAND_BLDG_DEPREC`, line 10b),
which is not the gross cost.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112. - Asset composition analysis for
  nonprofits.

### Variables used:

- `F9_10_ASSET_LAND_BLDG`: Land, buildings, and equipment, cost or other
  basis, line 10a (`land_buildings`)

- `F9_10_ASSET_TOT_EOY`: Total assets, EOY (`total_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_land_assets_gross_ratio( df = dat10k )
#>    :: Total assets equal to zero :: 346 case(s) replaced with NaN
head( d[ , c( "land_assets_gross", "land_assets_gross_w", "land_assets_gross_z", "land_assets_gross_p" ) ] )
#>   land_assets_gross land_assets_gross_w land_assets_gross_z land_assets_gross_p
#> 1                NA                  NA                  NA                  NA
#> 2         0.9728302           0.9728302           0.5983719                  84
#> 3                NA                  NA                  NA                  NA
#> 4         1.4272793           1.4272793           0.9847328                  93
#> 5                NA                  NA                  NA                  NA
#> 6         0.0000000           0.0000000          -0.9649662                   1
```
