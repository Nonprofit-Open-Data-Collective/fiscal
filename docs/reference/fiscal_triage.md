# Triage candidate fiscal-health indicators

Converts
[`fiscal_diagnostics()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_diagnostics.md)
results into an auditable recommendation. Standalone indicators are
excluded from dimension estimation but retained in the final measurement
system. Recommendations can be overridden explicitly.

## Usage

``` r
fiscal_triage(
  diagnostics,
  keep_from_redundant = c("higher_kmo", "higher_mean_abs_cor", "first"),
  overrides = NULL
)
```

## Arguments

- diagnostics:

  A `fiscal_diagnostics` object.

- keep_from_redundant:

  Rule for choosing a provisional representative from a connected
  redundancy group.

- overrides:

  Optional named character vector assigning indicators to `"retain"`,
  `"standalone"`, `"noise"`, `"redundant"`, or `"review"`.

## Value

An object of class `fiscal_triage` containing the decision table,
redundancy groups, dimension candidates, standalone indicators, excluded
indicators, and thresholds inherited from the diagnostics.
