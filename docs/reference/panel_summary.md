# Print a panel composition summary from appended classification columns

Produces a year-by-type count table from a data frame that already has
`panel_*` columns appended by
`panel_composition(append_classification = TRUE)`. Useful for quickly
re-checking composition after imputation or other panel manipulations
without re-running the full classification.

## Usage

``` r
panel_summary(df, time = "TAX_YEAR", id = "EIN2", print_table = TRUE)
```

## Arguments

- df:

  A data frame with `panel_type` and the `time` column present.

- time:

  Name of the time variable column. Default `"TAX_YEAR"`.

- id:

  Name of the organization identifier column. Default `"EIN2"`.

- print_table:

  Logical. If `TRUE`, print the formatted table. Default `TRUE`.

## Value

A data frame with one row per panel year and one column per panel type,
invisibly. Same structure as the summary table returned by
[`panel_composition()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_composition.md).

## See also

[`panel_composition()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_composition.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- panel_composition(d, append_classification = TRUE)
d <- panel_impute(d)
panel_summary(d)
} # }
```
