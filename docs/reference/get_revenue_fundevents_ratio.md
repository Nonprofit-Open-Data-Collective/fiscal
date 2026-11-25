# Fundraising Events Revenue Ratio

Fundraising event contributions as a share of total revenue.

**Formula:**

    revenue_fundevents = fundraising_event_revenue / total_revenue

**Definitional Range**

Bounded \[0, 1\]. Zero means none of this revenue type was received; one
means this channel accounted for all total revenue.

**Calculated For:** 990 filers only.

## Usage

``` r
get_revenue_fundevents_ratio( df,
  fundraising_event_revenue = "F9_08_REV_CONTR_FUNDR_EVNT",
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

- fundraising_event_revenue:

  Contributions received through fundraising events (Part VIII line 1c).

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

- `revenue_fundevents` (raw ratio)

- `revenue_fundevents_w` (winsorized)

- `revenue_fundevents_z` (z-score)

- `revenue_fundevents_p` (percentile rank, 1-100)

## Details

### Revenue Fundevents Ratio - Revenue composition measure

Formula: fundraising event contributions / total revenue. Bounded \[0,
1\].

Fundraising events (galas, walks, auctions) generate contributed revenue
but typically carry high direct expenses. This ratio measures the
contribution portion of event receipts (Part VIII line 1c) as a share of
total revenue; gross event income (line 8a) and net event income after
direct expenses (line 8c) are reported elsewhere.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in
  nonprofit organizations. *Journal of Public Administration Research
  and Theory*, 19(4), 947-966.

### Variables used:

- `F9_08_REV_CONTR_FUNDR_EVNT`: Numerator (`fundraising_event_revenue`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_revenue_fundevents_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "revenue_fundevents", "revenue_fundevents_w", "revenue_fundevents_z", "revenue_fundevents_p" ) ] )
#>   revenue_fundevents revenue_fundevents_w revenue_fundevents_z
#> 1                 NA                   NA                   NA
#> 2                  0                    0            -2.497596
#> 3                 NA                   NA                   NA
#> 4                  0                    0            -2.497596
#> 5                 NA                   NA                   NA
#> 6                NaN                  NaN                  NaN
#>   revenue_fundevents_p
#> 1                   NA
#> 2                    1
#> 3                   NA
#> 4                    1
#> 5                   NA
#> 6                   NA
```
