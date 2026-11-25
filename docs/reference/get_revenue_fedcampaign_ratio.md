# Federated Campaign Revenue Ratio

Federated campaign contributions as a share of total revenue.

**Formula:**

    revenue_fedcampaign = federated_campaigns / total_revenue

**Definitional Range**

Bounded \[0, 1\]. Zero means none of this revenue type was received; one
means this channel accounted for all total revenue.

**Calculated For:** 990 filers only.

## Usage

``` r
get_revenue_fedcampaign_ratio( df,
  federated_campaigns = "F9_08_REV_CONTR_FED_CAMP",
  total_revenue       = "F9_08_REV_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- federated_campaigns:

  Federated campaign contributions.

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

- `revenue_fedcampaign` (raw ratio)

- `revenue_fedcampaign_w` (winsorized)

- `revenue_fedcampaign_z` (z-score)

- `revenue_fedcampaign_p` (percentile rank, 1-100)

## Details

### Revenue Fedcampaign Ratio - Revenue composition measure

Formula: federated campaign contributions / total revenue. Bounded \[0,
1\].

Federated campaigns (United Way, Combined Federal Campaign) are a
specific channel of collective giving. Organizations heavily funded
through federated campaigns face dependency on a single intermediary.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in
  nonprofit organizations. *Journal of Public Administration Research
  and Theory*, 19(4), 947-966.

### Variables used:

- `F9_08_REV_CONTR_FED_CAMP`: Numerator (`federated_campaigns`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

**Variables used:**

- `F9_08_REV_CONTR_FED_CAMP`: Federated campaign contributions
  (`federated_campaigns`, 990)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`, 990)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_revenue_fedcampaign_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "revenue_fedcampaign", "revenue_fedcampaign_w", "revenue_fedcampaign_z", "revenue_fedcampaign_p" ) ] )
#>   revenue_fedcampaign revenue_fedcampaign_w revenue_fedcampaign_z
#> 1                  NA                    NA                    NA
#> 2                   0                     0             -1.282415
#> 3                  NA                    NA                    NA
#> 4                   0                     0             -1.282415
#> 5                  NA                    NA                    NA
#> 6                 NaN                   NaN                   NaN
#>   revenue_fedcampaign_p
#> 1                    NA
#> 2                     1
#> 3                    NA
#> 4                     1
#> 5                    NA
#> 6                    NA
```
