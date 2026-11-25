# Apply a fitted fiscal-health measurement model

Scores reference or new data using the transformations and coefficients
stored by
[`fiscal_health_fit()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_fit.md).
No model parameters are re-estimated.

## Usage

``` r
fiscal_health_score(
  data,
  fit,
  missing = c("fit", "median", "complete"),
  min_coverage = 0,
  prefix = "fh_",
  include_standalone = TRUE,
  include_coverage = TRUE,
  append = TRUE,
  overwrite = FALSE
)
```

## Arguments

- data:

  A data frame containing every indicator required by `fit`.

- fit:

  A `fiscal_health_fit` object.

- missing:

  Missing-value rule. `"fit"` reuses the rule selected during fitting;
  `"median"` uses the reference-sample medians stored in `fit`;
  `"complete"` returns a missing dimension score if any indicator in
  that dimension is missing.

- min_coverage:

  Minimum proportion of a dimension's source indicators that must be
  observed before a score is reported. This applies even when reference
  medians are used.

- prefix:

  Prefix applied to generated score columns.

- include_standalone:

  Include standardized standalone indicators.

- include_coverage:

  Add an observation-level coverage column for every generated measure.

- append:

  If `TRUE`, append scores to `data`; otherwise return score and
  coverage columns only.

- overwrite:

  Allow generated columns to replace existing columns.

## Value

A data frame of class `fiscal_health_scores`. Its attributes retain the
fitted model, scoring audit, and generated column names.
