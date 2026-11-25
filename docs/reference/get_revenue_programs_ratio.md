# Program Service Revenue Ratio

Program service revenue as a share of total revenue.

**Formula:**

    revenue_programs = program_service_revenue / total_revenue

**Definitional Range**

Bounded \[0, 1\]. Zero means none of this revenue type was received; one
means this channel accounted for all total revenue.

**Calculated For:** 990 filers only.

## Usage

``` r
get_revenue_programs_ratio( df,
  program_service_revenue   = "F9_08_REV_PROG_TOT_TOT",
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

- program_service_revenue:

  Total program service revenue.

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

- `revenue_programs` (raw ratio)

- `revenue_programs_w` (winsorized)

- `revenue_programs_z` (z-score)

- `revenue_programs_p` (percentile rank, 1-100)

## Details

### Revenue Programs Ratio - Revenue composition measure

Formula: program service revenue / total revenue. Bounded \[0, 1\].

Pure program service share, distinct from get_earned_income_ratio which
also includes dues, royalties, and misc.

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Carroll, D.A. & Stater, K.J. (2009). Revenue diversification in
  nonprofit organizations. *Journal of Public Administration Research
  and Theory*, 19(4), 947-966.

### Variables used:

- `F9_08_REV_PROG_TOT_TOT`: Numerator (`program_service_revenue`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_revenue_programs_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "revenue_programs", "revenue_programs_w", "revenue_programs_z", "revenue_programs_p" ) ] )
#>   revenue_programs revenue_programs_w revenue_programs_z revenue_programs_p
#> 1               NA                 NA                 NA                 NA
#> 2        0.0000000          0.0000000         -2.4574007                  1
#> 3               NA                 NA                 NA                 NA
#> 4        0.2944863          0.2944863         -0.3315452                 63
#> 5               NA                 NA                 NA                 NA
#> 6              NaN                NaN                NaN                 NA
```
