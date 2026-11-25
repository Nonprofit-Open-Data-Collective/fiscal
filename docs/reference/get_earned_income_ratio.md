# Earned Income Dependency Ratio

Share of total revenue derived from earned (non-donation) income
sources.

**Formula:**

    eidr = earned_revenue / total_revenue

    earned_revenue = program_service_revenue + royalties + other_revenue

**Definitional Range**

Bounded \[0, 1\]. Organizations heavily reliant on philanthropy show
values near zero; fee-based service providers may show values above
0.90.

**Benchmarks and rules of thumb**

- No universal threshold – context matters: an advocacy organization is
  expected to show a low ratio; a hospital is expected to show a high
  one.

- A low ratio combined with high donation dependence indicates
  philanthropic concentration risk.

**Calculated For:** 990 filers only.

## Usage

``` r
get_earned_income_ratio( df,
  program_service_rev = "F9_08_REV_PROG_TOT_TOT",
  royalties           = "F9_08_REV_OTH_ROY_TOT",
  other_revenue       = "F9_08_REV_MISC_OTH_TOT",
  total_revenue       = "F9_08_REV_TOT_TOT",
  numerator = NULL, denominator = NULL, winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE,
  membership_dues = NULL )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- program_service_rev:

  Program service revenue.

- royalties:

  Royalties.

- other_revenue:

  Other miscellaneous revenue.

- total_revenue:

  Total revenue.

- numerator:

  Optional. A pre-aggregated column for earned revenue. When supplied,
  `program_service_rev`, `royalties`, `other_revenue` are ignored; it is
  an error to also set one of them explicitly.

- denominator:

  Optional. A pre-aggregated column for total revenue. When supplied,
  `total_revenue` is ignored; it is an error to also set it explicitly.

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

- membership_dues:

  Deprecated and ignored. Membership dues reported on Part VIII line 1b
  are already part of total contributions (line 1h), so counting them as
  earned income double counts them against
  [`get_donations_revenue_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_donations_revenue_ratio.md).
  Supplying it raises a warning.

## Value

Object of class `data.frame`: the original dataframe appended with four
new columns:

    - `earned_income`   - earned income dependency ratio (raw)
    - `earned_income_w` - winsorized version
    - `earned_income_z` - standardized z-score (based on winsorized values)
    - `earned_income_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The earned income dependency ratio measures the combined share of
revenue from program services, royalties, and miscellaneous revenue -
the sources that flow from the organization's own activities rather than
from voluntary contributions. It is the revenue-side complement to
[`get_donations_revenue_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_donations_revenue_ratio.md),
and a broader version of
[`get_self_sufficiency_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_self_sufficiency_ratio.md),
which compares program revenue to total expenses.

Organizations with high earned income ratios are often considered more
financially resilient because earned revenue is tied to service delivery
rather than donor preferences, though it also creates exposure to market
competition and customer retention challenges.

### Formula variations and their sources

(Program service revenue + royalties + other miscellaneous revenue) /
total revenue (Part VIII lines 2g + 5 + 11d / line 12A). This broad
definition of earned income follows several studies (Young 2007). A
narrower version uses only program service revenue (see
[`get_revenue_programs_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_revenue_programs_ratio.md)).

Membership dues are excluded. On the Form 990, dues that are gifts are
reported on Part VIII line 1b and are already included in total
contributions (line 1h), which
[`get_donations_revenue_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_donations_revenue_ratio.md)
counts; dues paid in exchange for member services are reported as
program service revenue (line 2) and are counted here. Earlier versions
added line 1b to earned income, so `earned_income` and `donations_rev`
could sum to more than 1.

### Canonical citations

- Young, D.R. (2007). *Financing Nonprofits*. AltaMira Press.

- Weisbrod, B.A. (1998). The nonprofit mission and its financing.
  *Journal of Policy Analysis and Management*, 17(2), 165-174.

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

### Variables used:

- `F9_08_REV_PROG_TOT_TOT`: Program service revenue
  (`program_service_rev`)

- `F9_08_REV_OTH_ROY_TOT`: Royalties (`royalties`)

- `F9_08_REV_MISC_OTH_TOT`: Other miscellaneous revenue
  (`other_revenue`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_earned_income_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "earned_income", "earned_income_w", "earned_income_z", "earned_income_p" ) ] )
#>   earned_income earned_income_w earned_income_z earned_income_p
#> 1            NA              NA              NA              NA
#> 2     0.0000000       0.0000000      -2.5082626               1
#> 3            NA              NA              NA              NA
#> 4     0.2944863       0.2944863      -0.3062983              63
#> 5            NA              NA              NA              NA
#> 6           NaN             NaN             NaN              NA
```
