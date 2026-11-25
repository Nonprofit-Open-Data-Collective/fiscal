# Net Assets Composition Ratio

Share of total net assets that are unrestricted and available for
general use.

**Formula:**

    nacr = unrestricted_net_assets / total_net_assets

**Definitional Range**

Bounded \[0, 1\] when both numerator and denominator are positive: zero
means all net assets are restricted; one means all net assets are
unrestricted. Values outside this range occur when either unrestricted
or total net assets are negative.

**Benchmarks and rules of thumb**

- **Above 0.50**: At least half of net assets are unrestricted –
  generally healthy.

- **Below 0.20**: Heavy donor-restricted balance sheet; limited
  flexibility.

- Organizations with large permanent endowments may show low ratios
  without financial weakness.

**Calculated For:** 990 filers only.

## Usage

``` r
get_netassets_composition_ratio( df,
  unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
  total_net_assets        = "F9_10_NAFB_TOT_EOY",
  restricted_net_assets   = "F9_10_NAFB_RESTRICT_EOY",
  winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- unrestricted_net_assets:

  Unrestricted net assets, EOY.

- total_net_assets:

  Total net assets, EOY.

- restricted_net_assets:

  Restricted net assets, EOY. When unrestricted and restricted net
  assets are both zero (filers that do not follow SFAS 117), all net
  assets are treated as unrestricted; see
  [`resolve_unrestricted_net_assets()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/resolve_unrestricted_net_assets.md).

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

    - `netassets_comp`   - net assets composition ratio (raw)
    - `netassets_comp_w` - winsorized version
    - `netassets_comp_z` - standardized z-score (based on winsorized values)
    - `netassets_comp_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The net assets composition ratio measures what fraction of total net
assets is unrestricted - available for general operations without
donor-imposed constraints. A high ratio means the organization has
substantial financial flexibility; a low ratio means most net assets are
restricted and cannot be redirected to cover operating shortfalls or
unexpected needs.

This metric is specific to the nonprofit sector: commercial firms have
no equivalent concept because all equity is by definition
"unrestricted." It is particularly relevant for organizations that
receive large restricted gifts or endowments, where total net assets may
look strong but the unrestricted portion available for operations may be
minimal.

### Formula variations and their sources

Unrestricted net assets (Part X line 27B) / total net assets (Part X
line 33B). An alternative formulation divides unrestricted net assets by
total assets, which is more conservative and is sometimes used in rating
agency analyses. The net assets denominator version is more common in
the academic literature.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS: International Journal of Voluntary and
  Nonprofit Organizations*, 5(3), 273-290. - Discusses the role of
  unrestricted resources in financial flexibility.

- Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
  Leadership*, 23(3), 281-302. - Examines unrestricted reserve
  composition.

### Variables used:

- `F9_10_NAFB_UNRESTRICT_EOY`: Unrestricted net assets, EOY
  (`unrestricted_net_assets`)

- `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (`total_net_assets`)

- `F9_10_NAFB_RESTRICT_EOY`: Restricted net assets, EOY
  (`restricted_net_assets`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_netassets_composition_ratio( df = dat10k )
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Total net assets equal to zero :: 335 case(s) replaced with NaN
head( d[ , c( "netassets_comp", "netassets_comp_w", "netassets_comp_z", "netassets_comp_p" ) ] )
#>   netassets_comp netassets_comp_w netassets_comp_z netassets_comp_p
#> 1             NA               NA               NA               NA
#> 2              1                1         2.753406               28
#> 3             NA               NA               NA               NA
#> 4              1                1         2.753406               28
#> 5             NA               NA               NA               NA
#> 6              1                1         2.753406               28
```
