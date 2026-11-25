# Net Assets Growth Ratio

Year-over-year change in total net assets, expressed as a proportion of
beginning net assets.

**Formula:**

    or = ( net_assets_eoy - net_assets_boy ) / net_assets_boy

**Definitional Range**

Unbounded in both directions. A value of 0 means net assets were
unchanged. Values below -1.0 (a drop of more than 100\\ lost more than
its entire starting equity base, resulting in negative net assets. The
ratio is undefined (NA) when BOY net assets equal zero.

**Benchmarks and rules of thumb**

- Small positive values (0.01-0.10) are typical and healthy for
  operating nonprofits.

- **Below -0.10** in a single year is a warning sign; sustained negative
  growth indicates a structural financial problem.

- Very high positive values (above 0.50) often reflect one-time large
  gifts or asset revaluations rather than sustained operational
  performance.

**Calculated For:** 990 + 990EZ filers.

## Usage

``` r
get_netassets_growth_ratio( df,
  net_assets_eoy = c( "F9_10_NAFB_TOT_EOY", "F9_01_NAFB_TOT_EOY" ),
  net_assets_boy = c( "F9_10_NAFB_TOT_BOY", "F9_01_NAFB_TOT_BOY" ),
  winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- net_assets_eoy:

  Total net assets, end of year. Accepts one or two column names; if two
  are provided they are coalesced with the 990 value taking priority
  over 990EZ.

- net_assets_boy:

  Total net assets, beginning of year. Accepts one or two column names.

- winsorize:

  The winsorization value (between 0 and 1), defaults to 0.98, which
  winsorizes at the 1st and 99th percentiles.

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"np"`. Options: `"np"` (negative
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

    - `netassets_growth`   - net assets growth ratio (raw)
    - `netassets_growth_w` - winsorized version
    - `netassets_growth_z` - standardized z-score (based on winsorized values)
    - `netassets_growth_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The net assets growth ratio measures the year-over-year percentage
change in total net assets - essentially the organizational equivalent
of return on equity growth. Positive values indicate the organization
grew its equity base during the year (surplus plus other balance sheet
adjustments); negative values indicate net asset erosion (deficit or
write-downs).

This ratio captures all changes to net assets, including unrealized
investment gains, prior-period adjustments, and currency translation
effects, not just the operating surplus. It is therefore a broader
measure than
[`get_return_netassets_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_return_netassets_ratio.md),
which uses only the revenues-less-expenses figure.

### Formula variations and their sources

(Net assets EOY - Net assets BOY) / Net assets BOY. This is the standard
percentage change formula. Some studies use the absolute dollar change
as the dependent variable rather than a ratio, especially when the BOY
base is near zero or negative.

Using BOY net assets (rather than EOY or average) as the denominator is
the standard practice for performance ratios: it measures the return
generated on the starting equity base, avoiding circular dependency
since the ending value is determined in part by the surplus being
measured.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460. - Equity
  growth is a component of the financial vulnerability framework.

- Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial
  vulnerability of charitable organizations. *Nonprofit Management and
  Leadership*, 11(2), 199-210.

### Variables used:

- `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (`net_assets_eoy`, 990)

- `F9_01_NAFB_TOT_EOY`: Net assets from Part I, EOY (`net_assets_eoy`,
  990EZ fallback)

- `F9_10_NAFB_TOT_BOY`: Total net assets, BOY (`net_assets_boy`, 990)

- `F9_01_NAFB_TOT_BOY`: Net assets from Part I, BOY (`net_assets_boy`,
  990EZ fallback)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_netassets_growth_ratio( df = dat10k )
#>    :: Beginning net assets equal to zero :: 591 case(s) replaced with NaN
head( d[ , c( "netassets_growth", "netassets_growth_w", "netassets_growth_z", "netassets_growth_p" ) ] )
#>   netassets_growth netassets_growth_w netassets_growth_z netassets_growth_p
#> 1              NaN                NaN                 NA                 NA
#> 2      -0.03539004        -0.03539004         -0.4512881                 33
#> 3      -0.63185913        -0.63185913         -1.5470228                  7
#> 4      -0.35553963        -0.35553963         -1.2101679                 12
#> 5              NaN                NaN                 NA                 NA
#> 6      -0.33632287        -0.33632287         -1.1850649                 12
```
