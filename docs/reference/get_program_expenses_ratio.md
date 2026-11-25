# Program Expense Ratio

Share of total expenses devoted to program services.

**Formula:**

    per = program_expenses / total_expenses

**Definitional Range**

Bounded zero to one. Zero means no spending on programs (all overhead);
one means all spending is on programs (no administration or
fundraising). In practice, values above 0.95 are unusual and may reflect
accounting misallocation. The empirical range for most operating
nonprofits is approximately 0.50 to 0.95.

**Benchmarks and rules of thumb**

- **\>= 75%**: Charity Navigator threshold for a favorable efficiency
  score.

- **\>= 65%**: BBB Wise Giving Alliance minimum standard.

- **Below 50%**: Commonly flagged as a concern, though legitimate for
  startup-phase organizations.

- Caution: high ratios achieved by underinvesting in administration (the
  "starvation cycle") can signal long-term organizational weakness (Lecy
  & Searing 2015).

**Calculated For:** 990 filers only.

## Usage

``` r
get_program_expenses_ratio( df,
  program_expenses = "F9_09_EXP_TOT_PROG",
  total_expenses   = c( "F9_09_EXP_TOT_TOT", "F9_01_EXP_TOT_CY" ),
  winsorize = 0.98 ,
  range     = "zo",
  sanitize  = TRUE,
  summarize = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- program_expenses:

  Program service expenses.

- total_expenses:

  Total functional expenses. Accepts one or two column names; if two are
  provided they are coalesced with the 990 value taking priority over
  990EZ.

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

    - `prog_exp`   - program expense ratio (raw)
    - `prog_exp_w` - winsorized version
    - `prog_exp_z` - standardized z-score (based on winsorized values)
    - `prog_exp_p` - percentile rank (1-100)

## Details

### Primary uses and key insights

The program expense ratio is arguably the most widely cited single
metric in nonprofit financial analysis. It measures what fraction of
total expenses is devoted directly to mission-related program services,
as opposed to management, administration, and fundraising. It is the
primary basis for charity rating systems (Charity Navigator, Better
Business Bureau Wise Giving Alliance) and is commonly used by donors,
funders, and board members as a shorthand efficiency measure.

However, it is also one of the most criticized metrics in the literature
because it can be gamed (by misallocating shared costs to programs),
because overhead is necessary for organizational effectiveness, and
because the "ideal" program ratio varies substantially by mission type
and operating model.

### Formula variations and their sources

Program service expenses / total functional expenses (Part IX line 25B /
line 25A). For 990EZ filers, Part I total expenses is used as the
denominator fallback; however, the 990EZ does not separately report
program expenses, so this ratio is primarily meaningful for full-990
filers.

An alternative formulation uses total revenue rather than total expenses
in the denominator, but the expense-to-expense version is standard and
avoids distortions from surplus or deficit years.

### Canonical citations

- Tuckman, H.P. & Chang, C.F. (1991). A methodology for measuring the
  financial vulnerability of charitable nonprofit organizations.
  *Nonprofit and Voluntary Sector Quarterly*, 20(4), 445-460.

- Charity Navigator. *Financial Health Methodology*.
  charitynavigator.org.

  - Uses program expense ratio as a primary rating component (target: 75
    percent or more).

- Lecy, J.D. & Searing, E.A. (2015). Anatomy of the nonprofit starvation
  cycle. *Nonprofit and Voluntary Sector Quarterly*, 44(3), 539-563. -
  Critical analysis of program expense ratio benchmarks and their
  perverse incentives.

- Overhead Myth Campaign (GuideStar, BBB Wise Giving, Charity Navigator,
  2013).

  - Industry statement cautioning against overreliance on overhead
    ratios.

### Variables used:

- `F9_09_EXP_TOT_PROG`: Program service expenses (`program_expenses`,
  990)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`, 990)

- `F9_01_EXP_TOT_CY`: Total expenses from Part I (`total_expenses`,
  990EZ fallback)

## Examples

``` r
library( fiscal )
data( dat10k )

d <- get_program_expenses_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 134 case(s) replaced with NaN
head( d[ , c( "prog_exp", "prog_exp_w", "prog_exp_z", "prog_exp_p" ) ] )
#>    prog_exp prog_exp_w  prog_exp_z prog_exp_p
#> 1        NA         NA          NA         NA
#> 2 1.0000000  1.0000000  3.97694160         88
#> 3        NA         NA          NA         NA
#> 4 0.8591898  0.8591898  0.08541827         50
#> 5        NA         NA          NA         NA
#> 6 0.0000000  0.0000000 -3.25372824          1
```
