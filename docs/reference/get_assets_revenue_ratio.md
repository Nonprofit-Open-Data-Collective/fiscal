# Asset Revenue Ratio

Measures the size of an organization's asset base relative to its annual
revenue.

**Formula:**

    arr = total_assets / total_revenue

**Definitional Range**

Bounded below at zero; unbounded above. The ratio is undefined when
revenue is zero. Typical operating nonprofits show values in the \[0.5,
5.0\] range. Endowed organizations and capital-intensive nonprofits may
show values of 10 or higher.

**Benchmarks and rules of thumb**

- No universal benchmark; most meaningful for within-subsector
  comparisons.

- **Below 1.0**: Annual revenue exceeds total assets – common for lean
  service organizations with minimal physical assets.

- **Above 10**: Typically indicates a capital-heavy asset base or a
  small revenue base relative to accumulated assets.

**Calculated For:** 990 + 990EZ filers.

## Usage

``` r
get_assets_revenue_ratio( df,
  total_assets  = "F9_10_ASSET_TOT_EOY",
  total_revenue = c( "F9_08_REV_TOT_TOT", "F9_01_REV_TOT_CY" ),
  winsorize = 0.98 ,
  range     = "zp",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- total_assets:

  Total assets, EOY.

- total_revenue:

  Total revenue. Accepts one or two column names; if two are provided
  they are coalesced with the 990 value taking priority over 990EZ.

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

    - `assets_rev`   - asset revenue ratio (raw)
    - `assets_rev_w` - winsorized version
    - `assets_rev_z` - standardized z-score (based on winsorized values)
    - `assets_rev_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The asset revenue ratio measures how many dollars of assets are held per
dollar of annual revenue. It is an asset intensity measure:
capital-intensive organizations (hospitals, universities, housing
providers with large real estate portfolios) show high ratios; lean
operating nonprofits show low ratios. It can also be interpreted as an
approximate measure of how long the organization could theoretically
operate on its asset base - though this is not a direct liquidity
measure.

A related interpretation is efficiency: a lower ratio may indicate more
efficient use of assets to generate revenue, though this is not
universally true for nonprofits where asset accumulation may reflect
reserve-building rather than operational inefficiency.

### Formula variations and their sources

Total assets EOY / total revenue. The inverse (revenue / assets) is
sometimes called the asset turnover ratio and is more common in
commercial analysis. For nonprofits, the assets-to-revenue direction is
more intuitive because it expresses asset intensity in terms of revenue
multiples. Some studies use average assets ((BOY + EOY)/2) in the
denominator to account for mid-year asset changes, but the EOY value is
used here for consistency and data availability.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good: Executive
  compensation in nonprofit organizations. *Policy and Society*, 20(4),
  94-112.

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

### Variables used:

- `F9_10_ASSET_TOT_EOY`: Total assets, end of year (`total_assets`, 990)

- `F9_08_REV_TOT_TOT`: Total revenue from Part VIII (`total_revenue`,
  990)

- `F9_01_REV_TOT_CY`: Total revenue from Part I (`total_revenue`, 990EZ
  fallback)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_assets_revenue_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 212 case(s) replaced with NaN
head( d[ , c( "assets_rev", "assets_rev_w", "assets_rev_z", "assets_rev_p" ) ] )
#>   assets_rev assets_rev_w assets_rev_z assets_rev_p
#> 1  0.3987435    0.3987435   -0.6291590           24
#> 2 22.8851945   22.8851945    3.5871772           97
#> 3  0.2498218    0.2498218   -0.7975073           16
#> 4  0.2686368    0.2686368   -0.7758803           17
#> 5  0.0000000    0.0000000   -1.0906263            1
#> 6        NaN          NaN          NaN           NA
```
