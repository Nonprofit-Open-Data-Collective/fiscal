# Related Organizations Revenue Ratio

Revenue from related organizations as a share of total revenue.

**Formula:**

    revenue_reltdorgs = related_org_revenue / total_revenue

**Definitional Range**

Bounded \[0, 1\]. Zero means none of this revenue type was received; one
means this channel accounted for all total revenue.

**Calculated For:** 990 filers only.

## Usage

``` r
get_revenue_reltdorgs_ratio( df,
  related_org_revenue       = "F9_08_REV_CONTR_RLTD_ORG",
  total_revenue             = "F9_08_REV_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- related_org_revenue:

  Contributions from related organizations.

- total_revenue:

  Total revenue.

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

- `revenue_reltdorgs` (raw ratio)

- `revenue_reltdorgs_w` (winsorized)

- `revenue_reltdorgs_z` (z-score)

- `revenue_reltdorgs_p` (percentile rank, 1-100)

## Details

### Revenue Reltdorgs Ratio - Revenue composition measure

Formula: related organization revenue / total revenue. Bounded \[0, 1\].

Revenue from related organizations (subsidiaries, affiliates, supporting
organizations) may indicate resource sharing within a nonprofit family
rather than arm's-length fundraising. High values warrant review of
organizational structure and transfer pricing.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in
  nonprofit organizations. *Journal of Public Administration Research
  and Theory*, 19(4), 947-966.

### Variables used:

- `F9_08_REV_CONTR_RLTD_ORG`: Numerator (`related_org_revenue`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_revenue_reltdorgs_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "revenue_reltdorgs", "revenue_reltdorgs_w", "revenue_reltdorgs_z", "revenue_reltdorgs_p" ) ] )
#>   revenue_reltdorgs revenue_reltdorgs_w revenue_reltdorgs_z revenue_reltdorgs_p
#> 1                NA                  NA                  NA                  NA
#> 2                 0                   0            -1.39498                   1
#> 3                NA                  NA                  NA                  NA
#> 4                 0                   0            -1.39498                   1
#> 5                NA                  NA                  NA                  NA
#> 6               NaN                 NaN                 NaN                  NA
```
