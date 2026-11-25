# Overhead Ratio

Combined management and fundraising expenses as a share of total
expenses.

**Formula:**

    overhead = ( mgmt_expenses + fundraising_expenses ) / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Empirical range approximately \[0.05, 0.50\] for most
operating nonprofits.

**Benchmarks and rules of thumb**

- **25% or less**: Charity Navigator threshold for a full efficiency
  score.

- **35% or less**: BBB Wise Giving Alliance standard.

- Low overhead can indicate underinvestment in organizational capacity
  (the "starvation cycle") rather than genuine efficiency.

**Calculated For:** 990 filers only.

## Usage

``` r
get_overhead_ratio( df,
  mgmt_expenses             = "F9_09_EXP_TOT_MGMT",
  total_expenses            = "F9_09_EXP_TOT_TOT",
  fundraising_expenses      = "F9_09_EXP_TOT_FUNDR",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- mgmt_expenses:

  Management and general expenses.

- total_expenses:

  Total functional expenses.

- fundraising_expenses:

  Fundraising expenses.

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

The original `data.frame` with four columns appended: `overhead`,
`overhead_w`, `overhead_z`, `overhead_p`.

## Details

### Primary uses and key insights

The overhead ratio measures the combined share of total expenses devoted
to management and fundraising. It is the standard definition used in the
nonprofit accountability literature and by charity watchdog ratings. The
overhead ratio complements
[`get_program_expenses_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_program_expenses_ratio.md):
the two sum to 1.0. However, its use as a primary quality indicator has
been widely criticized.

### Formula variations and their sources

(Management and general + Fundraising) / Total expenses (Part IX lines
25C + 25D / 25A). The administrative-only version
([`get_expenses_admin_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_expenses_admin_ratio.md))
excludes fundraising.

### Canonical citations

- Charity Navigator. *Financial Health Methodology*.
  charitynavigator.org.

  - Uses overhead ratio as a primary rating factor (target: overhead 25%
    or less).

- Lecy, J.D. & Searing, E.A. (2015). Anatomy of the nonprofit starvation
  cycle. *Nonprofit and Voluntary Sector Quarterly*, 44(3), 539-563.

- Hager, M.A. & Flack, T. (2004). The pros and cons of financial
  efficiency standards. *Nonprofit Overhead Cost Project Brief 5*. Urban
  Institute.

### Variables used:

- `F9_09_EXP_TOT_MGMT`: Management and general expenses
  (`mgmt_expenses`)

- `F9_09_EXP_TOT_FUNDR`: Fundraising expenses (`fundraising_expenses`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_overhead_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "overhead", "overhead_w", "overhead_z", "overhead_p" ) ] )
#>    overhead overhead_w overhead_z overhead_p
#> 1        NA         NA         NA         NA
#> 2 0.0000000  0.0000000 -3.9940816          1
#> 3        NA         NA         NA         NA
#> 4 0.1408102  0.1408102 -0.0828619         55
#> 5        NA         NA         NA         NA
#> 6 1.0000000  1.0000000  3.2363746         98
```
