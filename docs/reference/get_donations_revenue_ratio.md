# Donation/Grant Dependence Ratio

Measures reliance on contributions and fundraising as a share of total
revenue.

**Formula:**

    dgdr = donation_revenue / total_revenue

    donation_revenue = contributions + fundraising_revenue

**Definitional Range**

Bounded \[0, 1\]. Values near 1.0 characterize traditional charitable
organizations with minimal earned income; values near 0 characterize
fee-based service providers.

**Benchmarks and rules of thumb**

- Values above 0.70-0.80 warrant monitoring of donor concentration and
  retention.

- Organizations above 0.90 have almost no earned income buffer if
  philanthropic support declines.

**Calculated For:** 990 filers only.

## Usage

``` r
get_donations_revenue_ratio( df,
  contributions        = "F9_08_REV_CONTR_TOT",
  fundraising_revenue  = "F9_08_REV_OTH_FUNDR_NET_TOT",
  total_revenue        = "F9_08_REV_TOT_TOT",
  numerator = NULL, denominator = NULL, winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- contributions:

  Total contributions, EOY.

- fundraising_revenue:

  Net fundraising event revenue.

- total_revenue:

  Total revenue.

- numerator:

  Optional. A pre-aggregated column name for donation revenue. When
  supplied, `contributions`, `fundraising_revenue` are ignored; it is an
  error to also set one of them explicitly.

- denominator:

  Optional. A pre-aggregated column name for the denominator. When
  supplied, `total_revenue` is ignored; it is an error to also set it
  explicitly.

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

    - `donations_rev`   - donation/grant dependence ratio (raw)
    - `donations_rev_w` - winsorized version
    - `donations_rev_z` - standardized z-score (based on winsorized values)
    - `donations_rev_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The donations and grant dependence ratio measures the combined share of
total revenue from contributions (individual donations, foundation
grants, corporate giving, federated campaigns) and net fundraising event
income. It captures the overall dependence on philanthropic and
voluntary support.

A high ratio indicates the organization relies heavily on the goodwill
of donors and the fundraising environment rather than earned or
contractual income. This creates vulnerability to donor fatigue,
economic downturns (when charitable giving declines), and changes in
donor priorities.

### Formula variations and their sources

(Total contributions + net fundraising event revenue) / total revenue
(Part VIII lines 1h + 8c / line 12A). Government grants are included in
total contributions (line 1h) in this formulation. For a pure private
philanthropy measure that excludes government grants, combine this ratio
with
[`get_grants_govt_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_grants_govt_ratio.md).

### Canonical citations

- Chang, C.F. & Tuckman, H.P. (1994). Revenue diversification among
  nonprofits. *VOLUNTAS*, 5(3), 273-290.

- Froelich, K.A. (1999). Diversification of revenue strategies.
  *Nonprofit and Voluntary Sector Quarterly*, 28(3), 246-268.

### Variables used:

- `F9_08_REV_CONTR_TOT`: Total contributions (`contributions`)

- `F9_08_REV_OTH_FUNDR_NET_TOT`: Net fundraising event revenue
  (`fundraising_revenue`)

- `F9_08_REV_TOT_TOT`: Total revenue (`total_revenue`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_donations_revenue_ratio( df = dat10k )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
head( d[ , c( "donations_rev", "donations_rev_w", "donations_rev_z", "donations_rev_p" ) ] )
#>   donations_rev donations_rev_w donations_rev_z donations_rev_p
#> 1            NA              NA              NA              NA
#> 2     1.0000000       1.0000000       1.4633229              86
#> 3            NA              NA              NA              NA
#> 4     0.7055137       0.7055137      -0.2128906              48
#> 5            NA              NA              NA              NA
#> 6           NaN             NaN             NaN              NA
```
