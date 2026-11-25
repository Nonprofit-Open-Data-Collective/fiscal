# Construct a composite fiscal-health index

Aggregates scores produced by
[`fiscal_health_score()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_score.md)
using a
[`fiscal_weights()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_weights.md)
specification.

## Usage

``` r
fiscal_health_index(
  data,
  weights,
  score_prefix = "fh_",
  index_name = "fiscal_health",
  coverage_name = paste0(index_name, "_coverage"),
  append = TRUE,
  overwrite = FALSE
)
```

## Arguments

- data:

  A data frame, normally returned by
  [`fiscal_health_score()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_health_score.md).

- weights:

  A `fiscal_weights` object.

- score_prefix:

  Prefix used on the component score columns.

- index_name:

  Name of the composite index column.

- coverage_name:

  Name of its weighted-coverage column.

- append:

  If `TRUE`, append the index to `data`; otherwise return only the index
  and coverage columns.

- overwrite:

  Allow replacement of existing output columns.

## Value

A data frame containing the composite index and weighted coverage.
