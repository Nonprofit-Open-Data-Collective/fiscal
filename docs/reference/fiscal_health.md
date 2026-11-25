# Fit, score, and optionally aggregate fiscal health

A convenience wrapper around
[`fiscal_health_fit()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_fit.md),
[`fiscal_health_score()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_score.md),
[`fiscal_weights()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_weights.md),
and
[`fiscal_health_index()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_index.md).
It deliberately does not automate diagnostics, triage, or dimension
discovery because those stages require review and substantive judgment.

## Usage

``` r
fiscal_health(
  data,
  dimensions = NULL,
  fit = NULL,
  weights = NULL,
  index = TRUE,
  fit_args = list(),
  score_args = list(),
  weight_args = list(),
  index_args = list()
)
```

## Arguments

- data:

  A reference or scoring data frame.

- dimensions:

  A reviewed `fiscal_dimensions` object or named list of indicator
  groups. Required when `fit` is not supplied.

- fit:

  An existing `fiscal_health_fit` object for scoring new data.

- weights:

  A `fiscal_weights` object, a named numeric vector of custom weights,
  or `NULL` for equal weights.

- index:

  Construct the composite fiscal-health index.

- fit_args:

  Named list of additional arguments for
  [`fiscal_health_fit()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_fit.md).
  Used only when fitting a new model.

- score_args:

  Named list of additional arguments for
  [`fiscal_health_score()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_score.md).

- weight_args:

  Named list of additional arguments for
  [`fiscal_weights()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_weights.md)
  when `weights` is not already a `fiscal_weights` object.

- index_args:

  Named list of additional arguments for
  [`fiscal_health_index()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_index.md).

## Value

An object of class `fiscal_health_result` with `data`, `fit`, `weights`,
and `audit` components.
