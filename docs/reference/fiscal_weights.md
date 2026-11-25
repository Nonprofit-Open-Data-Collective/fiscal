# Define weights for a composite fiscal-health index

Creates a validated weighting specification for already-scored
dimensions and standalone indicators. It does not discover dimensions or
fit a measurement model.

## Usage

``` r
fiscal_weights(
  fit = NULL,
  weights = NULL,
  include_standalone = TRUE,
  normalize = TRUE,
  missing = c("reweight", "complete"),
  min_coverage = 1,
  name = NULL
)
```

## Arguments

- fit:

  Optional `fiscal_health_fit` object identifying the available measures
  and their types.

- weights:

  Optional named numeric vector of custom weights. If omitted, all
  included measures receive equal weight.

- include_standalone:

  Include standalone indicators from `fit` in an automatically generated
  or validated specification.

- normalize:

  Normalize weights to sum to one.

- missing:

  Composite-index missing-score rule: `"reweight"` reallocates weight
  across available scores; `"complete"` requires every positively
  weighted score.

- min_coverage:

  Minimum weighted coverage required to report the index.

- name:

  Optional descriptive name for the weighting specification.

## Value

An object of class `fiscal_weights`.
