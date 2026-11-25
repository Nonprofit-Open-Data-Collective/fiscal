# Compensation Expense Ratio

Total compensation and employee-related expenses as a share of total
functional expenses.

**Formula:**

    expenses_compensation = ( officer_comp + disqualified_comp + other_salaries
                              + pension_contributions + other_employee_benefits
                              + payroll_taxes ) / total_expenses

**Definitional Range**

Bounded \[0, 1\]. Most service-delivery nonprofits fall in the \[0.50,
0.85\] range. Grant-making organizations may show ratios below 0.20.

**Benchmarks and rules of thumb**

- No universal benchmark – labor intensity varies fundamentally by
  mission type.

- Human services, health, and education nonprofits typically show ratios
  above 0.60; grant-making organizations may show ratios below 0.30.

- A sharp year-over-year increase may indicate revenue contraction
  rather than compensation growth.

**Calculated For:** 990 filers only.

## Usage

``` r
get_expenses_compensation_ratio( df,
  officer_comp           = "F9_09_EXP_COMP_DTK_TOT",
  disqualified_comp      = "F9_09_EXP_COMP_DSQ_PERS_TOT",
  other_salaries         = "F9_09_EXP_OTH_SAL_WAGE_TOT",
  pension_contributions  = "F9_09_EXP_PENSION_CONTR_TOT",
  other_employee_benefits = "F9_09_EXP_OTH_EMPL_BEN_TOT",
  payroll_taxes          = "F9_09_EXP_PAYROLL_TAX_TOT",
  total_expenses         = "F9_09_EXP_TOT_TOT",
  winsorize  = 0.98 ,
  range     = "zo",
  sanitize   = TRUE,
  summarize  = FALSE )
```

## Arguments

- df:

  A `data.frame` containing the fields required for computing the
  metric.

- officer_comp:

  Compensation of current officers, directors, and key employees
  (total).

- disqualified_comp:

  Compensation of disqualified persons (total).

- other_salaries:

  Other salaries and wages (total).

- pension_contributions:

  Pension plan accruals and contributions (total).

- other_employee_benefits:

  Other employee benefits (total).

- payroll_taxes:

  Payroll taxes (total).

- total_expenses:

  Total functional expenses.

- winsorize:

  Winsorization proportion between 0 and 1 (default `0.98`).

- range:

  Character string specifying the theoretical range of the ratio, used
  to set winsorization bounds. Default `"zo"`. Options: `"np"` (negative
  to positive), `"zp"` (zero to positive), `"zo"` (zero to one), `"nz"`
  (negative to zero), or a custom `"lo;hi"` pair (e.g. `"0;10"`).

- sanitize:

  Logical (default `TRUE`).

- summarize:

  Logical (default `FALSE`).

## Value

The original `data.frame` with four new columns:

- `expenses_compensation` (raw ratio)

- `expenses_compensation_w` (winsorized)

- `expenses_compensation_z` (z-score)

- `expenses_compensation_p` (percentile rank, 1-100)

## Details

### Primary uses and key insights

The compensation expense ratio measures the share of total functional
expenses devoted to all forms of employee compensation: salaries,
benefits, payroll taxes, and pension contributions. For most
service-delivering nonprofits this is the largest single expense
category (60-80\\ indicator of labor intensity and human capital
investment.

### Formula variations and their sources

Sum of six Part IX compensation fields (lines 5-10, Column A) / total
expenses (line 25A): officer compensation, disqualified person
compensation, other salaries, pension contributions, other employee
benefits, and payroll taxes.

### Canonical citations

- Frumkin, P. & Keating, E.K. (2001). The price of doing good. *Policy
  and Society*, 20(4), 94-112.

- Leete, L. (2001). Whither the nonprofit wage differential? *Journal of
  Labor Economics*, 19(1), 136-170.

### Variables used:

- `F9_09_EXP_COMP_DTK_TOT`: Officer/director compensation
  (`officer_comp`)

- `F9_09_EXP_COMP_DSQ_PERS_TOT`: Disqualified person compensation
  (`disqualified_comp`)

- `F9_09_EXP_OTH_SAL_WAGE_TOT`: Other salaries and wages
  (`other_salaries`)

- `F9_09_EXP_PENSION_CONTR_TOT`: Pension contributions
  (`pension_contributions`)

- `F9_09_EXP_OTH_EMPL_BEN_TOT`: Other employee benefits
  (`other_employee_benefits`)

- `F9_09_EXP_PAYROLL_TAX_TOT`: Payroll taxes (`payroll_taxes`)

- `F9_09_EXP_TOT_TOT`: Total functional expenses (`total_expenses`)

## Examples

``` r
library( fiscal )
data( dat10k )
d <- get_expenses_compensation_ratio( df = dat10k )
#>    :: Total expenses equal to zero :: 56 case(s) replaced with NaN
head( d[ , c( "expenses_compensation", "expenses_compensation_w", "expenses_compensation_z", "expenses_compensation_p" ) ] )
#>   expenses_compensation expenses_compensation_w expenses_compensation_z
#> 1                    NA                      NA                      NA
#> 2             0.0000000               0.0000000               -3.390575
#> 3                    NA                      NA                      NA
#> 4             0.1882692               0.1882692               -1.080686
#> 5                    NA                      NA                      NA
#> 6             0.0000000               0.0000000               -3.390575
#>   expenses_compensation_p
#> 1                      NA
#> 2                       1
#> 3                      NA
#> 4                      47
#> 5                      NA
#> 6                       1
```
