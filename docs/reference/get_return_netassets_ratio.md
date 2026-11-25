# Return on Net Assets

Net surplus or deficit as a share of beginning-of-year net assets.

**Formula:**

    rona = revenues_less_expenses / net_assets_boy

**Definitional Range**

Unbounded in both directions. A value of 0 means break-even. The typical
range for nonprofits is approximately \[-0.30, 0.30\]. The ratio is
undefined (NA) when beginning net assets equal zero. Extreme values
occur when BOY net assets are near zero (small denominator) rather than
when the surplus itself is large.

**Benchmarks and rules of thumb**

- Small positive values (0.02-0.10) are considered healthy.

- Sustained negative RONA over multiple years is a financial
  vulnerability indicator (Greenlee & Trussel 2000).

- Most useful in trend analysis and within-subsector comparisons.

**Calculated For:** 990 + 990EZ filers.

## Usage

``` r
get_return_netassets_ratio( df,
  revenues_less_expenses = "F9_01_EXP_REV_LESS_EXP_CY",
  net_assets_boy         = c( "F9_10_NAFB_TOT_BOY", "F9_01_NAFB_TOT_BOY" ),
  winsorize = 0.98 ,
  range     = "np",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- revenues_less_expenses:

  Revenues less expenses (net surplus or deficit) for the current year.
  Accepts one or two column names; if two are provided they are
  coalesced with the 990 value taking priority over 990EZ.

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

    - `return_netassets`   - return on net assets (raw)
    - `return_netassets_w` - winsorized version
    - `return_netassets_z` - standardized z-score (based on winsorized values)
    - `return_netassets_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

Return on net assets (RONA) measures how effectively the organization
used its equity base to generate a surplus. By using beginning-of-year
net assets as the denominator, it expresses the annual surplus as a
percentage return on the organizational equity in place at the start of
the year - analogous to return on equity (ROE) in commercial finance.

RONA is closely related to
[`get_netassets_growth_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_netassets_growth_ratio.md):
both measure year-over-year equity change, but RONA uses the reported
revenues-less-expenses figure while the growth ratio uses the direct
balance sheet comparison (EOY - BOY). The two can differ when there are
other net asset adjustments (Form 990 Part XI line 9; Form 990-EZ Part I
line 20).

### Formula variations and their sources

Revenues less expenses / net assets BOY. Using beginning-of-year net
assets in the denominator (rather than ending or average) avoids
circular dependency: the ending value is determined partly by the
surplus being measured. This is the standard approach in the nonprofit
literature (Greenlee & Trussel 2000).

An alternative uses average net assets ((BOY + EOY)/2), which smooths
distortions from large mid-year transactions, but requires two balance
sheet fields and is not standard in the 990-based literature.

### Canonical citations

- Greenlee, J.S. & Trussel, J.M. (2000). Predicting the financial
  vulnerability of charitable organizations. *Nonprofit Management and
  Leadership*, 11(2), 199-210.

- Keating, E.K., Fischer, M., Gordon, T.P. & Greenlee, J. (2005).
  Assessing financial vulnerability in the nonprofit sector. *Harvard
  Business School Working Paper 04-016*.

- Nonprofit Finance Fund. *State of the Nonprofit Sector Survey*
  (annual).

### Variables used:

- `F9_01_EXP_REV_LESS_EXP_CY`: Revenues less expenses, current year
  (`revenues_less_expenses`)

- `F9_10_NAFB_TOT_BOY`: Total net assets, BOY (`net_assets_boy`, 990)

- `F9_01_NAFB_TOT_BOY`: Net assets from Part I, BOY (`net_assets_boy`,
  990EZ fallback)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_return_netassets_ratio( df = dat10k )
#>    :: Beginning net assets equal to zero :: 591 case(s) replaced with NaN
head( d[ , c( "return_netassets", "return_netassets_w", "return_netassets_z", "return_netassets_p" ) ] )
#>   return_netassets return_netassets_w return_netassets_z return_netassets_p
#> 1              NaN                NaN                 NA                 NA
#> 2      -0.03539004        -0.03539004         -0.4445135                 33
#> 3      -0.63185913        -0.63185913         -1.6012620                  6
#> 4      -0.35553963        -0.35553963         -1.2504821                 11
#> 5              NaN                NaN                 NA                 NA
#> 6      -0.33632287        -0.33632287         -1.2269591                 11
```
