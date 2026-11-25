# Fit a reusable fiscal-health measurement model

Converts reviewed candidate dimensions into a reusable scoring
specification. The fitted object stores indicator orientation, centers,
scales, missing-value treatment, dimension coefficients, and
dimension-score standardization parameters. It can therefore be applied
unchanged to later panel years by a scoring function.

## Usage

``` r
fiscal_health_fit(
  data,
  dimensions,
  dimension_names = NULL,
  scoring = "standardized_mean",
  standalone = NULL,
  indicator_direction = NULL,
  missing = c("median", "complete"),
  min_indicators = 2L,
  fm = "minres",
  strict_direction = FALSE
)
```

## Arguments

- data:

  Reference data used to estimate the measurement model.

- dimensions:

  A `fiscal_dimensions` object or a named list whose elements are
  character vectors of indicators.

- dimension_names:

  Optional named character vector mapping discovered dimension names to
  substantive names.

- scoring:

  Dimension scoring method. Supply one value for every dimension or a
  named vector. Supported methods are `"standardized_mean"`, `"pca"`,
  and `"factor"`.

- standalone:

  Optional character vector of standalone indicators. The default uses
  standalones carried by a `fiscal_dimensions` object.

- indicator_direction:

  Optional named vector overriding indicator directions with `"higher"`,
  `"lower"`, or `"context"`. Other directions are inferred from
  [`fiscal_metrics()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_metrics.md)
  when possible.

- missing:

  Training-data missing-value rule: `"median"` or `"complete"`. Median
  replacement here is statistical missing-value treatment, not e-file
  data normalization.

- min_indicators:

  Minimum number of indicators required in a modeled dimension.

- fm:

  Factoring method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) for factor
  scores.

- strict_direction:

  If `TRUE`, error when an indicator is context dependent or absent from
  the metric registry without an explicit override.

## Value

An object of class `fiscal_health_fit` containing dimension and
standalone scoring specifications, metadata, warnings, and the source
discovery object.
