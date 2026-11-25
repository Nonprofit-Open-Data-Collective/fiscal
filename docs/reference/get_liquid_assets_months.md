# Months of Liquid Unrestricted Net Assets (LUNA)

Number of months an organization can operate using its liquid
unrestricted net assets.

**Formula:**

    luna = liquid_unrestricted_net_assets / monthly_expenses

    liquid_unrestricted_net_assets = unrestricted_net_assets
                                     - ( net_fixed_assets - mortgages_payable )
    monthly_expenses = total_expenses / 12

**Definitional Range**

Unbounded in both directions, expressed in months. Negative values
indicate that fixed property (net of mortgage debt) exceeds unrestricted
net assets. The practical range for operating nonprofits is
approximately \[-12, 36\] months.

**Benchmarks and rules of thumb**

- Same general benchmarks as
  [`get_operating_reserve_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_operating_reserve_ratio.md)
  (which is expressed in years rather than months): 3-6 months is
  considered adequate for most operating nonprofits.

- Negative LUNA values are particularly informative: they indicate
  unrestricted equity is entirely absorbed by illiquid fixed assets.

**Calculated For:** 990 filers only.

## Usage

``` r
get_liquid_assets_months( df,
  unrestricted_net_assets = "F9_10_NAFB_UNRESTRICT_EOY",
  net_fixed_assets        = "F9_10_ASSET_LAND_BLDG_NET_EOY",
  mortgages_payable       = "F9_10_LIAB_MTG_NOTE_EOY",
  total_expenses          = "F9_09_EXP_TOT_TOT",
  restricted_net_assets   = "F9_10_NAFB_RESTRICT_EOY",
  total_net_assets        = "F9_10_NAFB_TOT_EOY",
  winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- unrestricted_net_assets:

  Unrestricted net assets, EOY.

- net_fixed_assets:

  Net land, buildings, and equipment, EOY.

- mortgages_payable:

  Mortgages and notes payable, EOY.

- total_expenses:

  Total functional expenses.

- restricted_net_assets:

  Restricted net assets, EOY. Used with `total_net_assets` to detect
  filers that do not follow SFAS 117; see
  [`resolve_unrestricted_net_assets()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/resolve_unrestricted_net_assets.md).

- total_net_assets:

  Total net assets, EOY. Substituted for unrestricted net assets when
  unrestricted and restricted net assets are both zero. Set to `NULL` to
  disable the fallback.

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

    - `liquid_assets_months`   - months of liquid unrestricted net assets (raw)
    - `liquid_assets_months_w` - winsorized version
    - `liquid_assets_months_z` - standardized z-score (based on winsorized values)
    - `liquid_assets_months_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

Liquid unrestricted net assets (LUNA) measures how many months of
operating expenses are covered by unrestricted net assets that are not
tied up in illiquid fixed property (net of associated mortgage debt). It
is a refinement of the operating reserve ratio
([`get_operating_reserve_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_operating_reserve_ratio.md))
that more precisely isolates liquid resources by netting out the value
of fixed assets but adding back the portion financed by mortgage debt
(since that debt reduces the net equity encumbrance of the fixed asset).

### Formula variations and their sources

(Unrestricted net assets - net fixed assets + mortgages payable) /
(total expenses / 12). The addition of mortgage debt back into the
numerator reflects the logic that if the organization sold its building,
it would receive the net proceeds (market value minus mortgage
balance) - and the mortgage liability already reduces unrestricted net
assets. Adding it back avoids double-counting the encumbrance.

This formulation follows Zietlow et al. (2007) and is cited by Calabrese
(2013). It differs from
[`get_operating_reserve_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_operating_reserve_ratio.md)
in the addition of mortgages_payable to the numerator and in its units:
LUNA divides by monthly expenses (months), while the operating reserve
ratio divides by annual expenses (a multiple of annual expenses).

### Canonical citations

- Zietlow, J., Hankin, J.A. & Seidner, A. (2007). *Financial Management
  for Nonprofit Organizations*. Wiley. - Primary source for the LUNA
  definition.

- Calabrese, T.D. (2013). Running on empty. *Nonprofit Management and
  Leadership*, 23(3), 281-302. - Empirical application and validation.

### Variables used:

- `F9_10_NAFB_UNRESTRICT_EOY`: Unrestricted net assets, EOY
  (`unrestricted_net_assets`)

- `F9_10_ASSET_LAND_BLDG_NET_EOY`: Net land, buildings, and equipment
  (`net_fixed_assets`)

- `F9_10_LIAB_MTG_NOTE_EOY`: Mortgages and notes payable
  (`mortgages_payable`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

- `F9_10_NAFB_RESTRICT_EOY`: Restricted net assets, EOY
  (`restricted_net_assets`)

- `F9_10_NAFB_TOT_EOY`: Total net assets, EOY (`total_net_assets`)

Unrestricted net assets fall back to total net assets for filers that do
not follow SFAS 117 (see
[`resolve_unrestricted_net_assets()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/resolve_unrestricted_net_assets.md)).

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_liquid_assets_months( df = dat10k )
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "liquid_assets_months", "liquid_assets_months_w", "liquid_assets_months_z", "liquid_assets_months_p" ) ] )
#>   liquid_assets_months liquid_assets_months_w liquid_assets_months_z
#> 1                   NA                     NA                     NA
#> 2           41.3263838             41.3263838              1.0614178
#> 3                   NA                     NA                     NA
#> 4            0.4798969              0.4798969             -0.9656785
#> 5                   NA                     NA                     NA
#> 6           23.6800000             23.6800000              0.7950036
#>   liquid_assets_months_p
#> 1                     NA
#> 2                     86
#> 3                     NA
#> 4                     17
#> 5                     NA
#> 6                     79
```
