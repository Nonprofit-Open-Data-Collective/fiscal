# Administrative Overhead Ratio

Share of total expenses devoted to management and general
administration.

**Formula:**

    aer = management_expenses / total_expenses

**Definitional Range**

Bounded \[0, 1\]. In combination with program and fundraising ratios,
all three sum to 1.0 by construction. The empirical range for most
nonprofits is approximately \[0.05, 0.35\].

**Benchmarks and rules of thumb**

- Charity Navigator targets management and general expenses at or below
  15% of total expenses for a favorable score.

- Values below 5% may indicate underinvestment in governance systems.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_admin_ratio( df,
  mgmt_expenses  = "F9_09_EXP_TOT_MGMT",
  total_expenses = "F9_09_EXP_TOT_TOT",
  winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- mgmt_expenses:

  Management and general expenses.

- total_expenses:

  Total functional expenses.

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

    - `expenses_admin`   - administrative overhead ratio (raw)
    - `expenses_admin_w` - winsorized version
    - `expenses_admin_z` - standardized z-score (based on winsorized values)
    - `expenses_admin_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The administrative overhead ratio measures what fraction of total
expenses is devoted to management and general administration - the
governance, compliance, financial management, and executive functions of
the organization. It is the management component of the broader overhead
ratio
([`get_overhead_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_overhead_ratio.md)),
which adds fundraising expenses.

A high administrative ratio may indicate organizational complexity,
compliance burden, or inefficiency; a very low ratio may indicate
underinvestment in governance and financial systems. The appropriate
level depends heavily on organizational size and mission type.

### Formula variations and their sources

Management and general expenses (Part IX line 25C) / total functional
expenses (line 25A). This is the most common operationalization. An
alternative includes unallocated costs or uses a broader definition of
overhead, but the Part IX column breakdown (program, management,
fundraising) is the standard basis.

### Canonical citations

- Lecy, J.D. & Searing, E.A. (2015). Anatomy of the nonprofit starvation
  cycle. *Nonprofit and Voluntary Sector Quarterly*, 44(3), 539-563.

- Overhead Myth Campaign (GuideStar, BBB Wise Giving, Charity Navigator,
  2013).

- Nunnenkamp, P. & Ohler, H. (2012). Throwing foreign aid at HIV/AIDS in
  developing countries: Missing the target? *World Development*, 40(10),
  1978-1994. - Uses administrative ratios in cross-organizational
  comparisons.

### Variables used:

- `F9_09_EXP_TOT_MGMT`: Management and general expenses
  (`mgmt_expenses`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_expenses_admin_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_admin", "expenses_admin_w", "expenses_admin_z", "expenses_admin_p" ) ] )
#>   expenses_admin expenses_admin_w expenses_admin_z expenses_admin_p
#> 1             NA               NA               NA               NA
#> 2     0.00000000       0.00000000       -3.9478780                1
#> 3             NA               NA               NA               NA
#> 4     0.09831695       0.09831695       -0.2227096               51
#> 5             NA               NA               NA               NA
#> 6     1.00000000       1.00000000        3.4533971               99
```
