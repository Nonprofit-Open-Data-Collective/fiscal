# Debt to Equity Ratio

Compares total liabilities to unrestricted net assets.

**Formula:**

    der = total_liabilities / unrestricted_net_assets

**Definitional Range**

Bounded below at zero when liabilities are non-negative and unrestricted
net assets are positive. Unbounded above when unrestricted net assets
are very small. Values below zero occur when unrestricted net assets are
negative (accumulated deficits exceed unrestricted equity), an acute
financial distress signal. The ratio is undefined (NA) when unrestricted
net assets equal zero.

**Benchmarks and rules of thumb**

- Values below 1.0 mean total liabilities are less than the unrestricted
  equity base – generally considered strong.

- Values between 1.0 and 3.0 indicate moderate leverage.

- Values above 5.0 indicate very high leverage relative to the equity
  base and are a common vulnerability threshold.

- Negative values indicate negative unrestricted net assets, which
  almost always signals significant financial stress.

**Calculated For:** 990 filers only.

## Usage

``` r
get_debt_equity_ratio( df,
  debt   = c( "F9_10_LIAB_TOT_EOY",       "F9_01_NAFB_LIAB_TOT_EOY"    ),
  equity = "F9_10_NAFB_UNRESTRICT_EOY",
  restricted_net_assets = "F9_10_NAFB_RESTRICT_EOY",
  total_net_assets      = "F9_10_NAFB_TOT_EOY",
  winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- debt:

  Total liabilities, EOY. Accepts one or two column names; if two are
  provided they are coalesced with the 990 value taking priority.

- equity:

  Unrestricted net assets, EOY. Accepts one or two column names; if two
  are provided they are coalesced with the first taking priority.

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

    - `debt_equity`   - debt to equity ratio (raw)
    - `debt_equity_w` - winsorized version
    - `debt_equity_z` - standardized z-score (based on winsorized values)
    - `debt_equity_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The debt to equity ratio compares total obligations to the
organization's equity cushion - its unrestricted net assets. In the
nonprofit context, unrestricted net assets represent the accumulated
surplus that the organization controls without donor restriction: the
purest measure of its financial equity. A high ratio signals that
liabilities substantially exceed the equity base, meaning a relatively
small revenue shortfall could impair the ability to meet obligations.

This ratio is closely related to
[`get_debt_assets_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_debt_assets_ratio.md)
(DAR), but uses unrestricted net assets as the denominator instead of
total assets. It is a stricter solvency measure because unrestricted net
assets are typically much smaller than total assets, and because
restricted assets cannot be used to pay general obligations.

### Formula variations and their sources

The commercial debt/equity ratio uses total debt / shareholders' equity.
The nonprofit adaptation substitutes unrestricted net assets for equity,
following Tuckman & Chang (1991) and the argument that only unrestricted
resources represent true organizational equity available to cover
obligations.

An alternative uses total net assets in the denominator (restricted +
unrestricted), which is more generous and less analytically precise; see
[`get_debt_netassets_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_debt_netassets_ratio.md).
A third variant uses only long-term debt in the numerator. This
implementation uses total liabilities / unrestricted net assets as the
most commonly cited nonprofit version.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Bowman, W. (2011). Financial capacity and sustainability of ordinary
  nonprofits. *Nonprofit Management and Leadership*, 22(1), 37-51.

### Variables used:

- `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (`debt`)

- `F9_01_NAFB_LIAB_TOT_EOY`: Total liabilities from Part I line 21
  (`debt`, fallback)

- `F9_10_NAFB_UNRESTRICT_EOY`: Unrestricted net assets, EOY (`equity`)

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

d <- get_debt_equity_ratio( df = dat10k )
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1,305 case(s) use total net assets
#>    :: Equity equal to zero :: 170 case(s) replaced with NaN
head( d[ , c( "debt_equity", "debt_equity_w", "debt_equity_z", "debt_equity_p" ) ] )
#>   debt_equity debt_equity_w debt_equity_z debt_equity_p
#> 1          NA            NA            NA            NA
#> 2   0.0000000     0.0000000    -0.6744908             8
#> 3          NA            NA            NA            NA
#> 4   0.1295450     0.1295450     0.3782937            67
#> 5          NA            NA            NA            NA
#> 6   0.2364865     0.2364865     0.5293368            74
```
