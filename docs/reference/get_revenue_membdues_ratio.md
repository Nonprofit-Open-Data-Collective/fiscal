# Membership Dues Revenue Ratio

Membership dues as a share of total revenue.

**Formula:**

    revenue_membdues = membership_dues / total_revenue

**Definitional Range**

Bounded \[0, 1\]. Zero means none of this revenue type was received; one
means this channel accounted for all total revenue.

**Calculated For:** 990 filers only.

## Usage

``` r
get_revenue_membdues_ratio( df,
  membership_dues           = "F9_08_REV_CONTR_MEMBSHIP_DUE",
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

- membership_dues:

  Membership dues received.

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

- `revenue_membdues` (raw ratio)

- `revenue_membdues_w` (winsorized)

- `revenue_membdues_z` (z-score)

- `revenue_membdues_p` (percentile rank, 1-100)

## Details

### Revenue Membdues Ratio - Revenue composition measure

Formula: membership dues / total revenue. Bounded \[0, 1\].

Membership dues provide relatively predictable recurring revenue tied to
member retention. A high ratio indicates a membership-model organization
whose financial health depends on maintaining and growing the member
base.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in
  nonprofit organizations. *Journal of Public Administration Research
  and Theory*, 19(4), 947-966.

### Variables used:

- `F9_08_REV_CONTR_MEMBSHIP_DUE`: Numerator (`membership_dues`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_revenue_membdues_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "revenue_membdues", "revenue_membdues_w", "revenue_membdues_z", "revenue_membdues_p" ) ] )
#>   revenue_membdues revenue_membdues_w revenue_membdues_z revenue_membdues_p
#> 1               NA                 NA                 NA                 NA
#> 2                0                  0          -2.221119                  1
#> 3               NA                 NA                 NA                 NA
#> 4                0                  0          -2.221119                  1
#> 5               NA                 NA                 NA                 NA
#> 6              NaN                NaN                NaN                 NA
```
