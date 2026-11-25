# Fiscal metric registry

Returns the canonical relationship between calculation functions, output
prefixes, conceptual categories, and score direction. `direction` is
`"higher"`, `"lower"`, or `"context"`; context-dependent composition
measures should not be included in a directional score without an
explicit scoring rule.

## Usage

``` r
fiscal_metrics()
```

## Value

A data frame with one row per fiscal metric.
