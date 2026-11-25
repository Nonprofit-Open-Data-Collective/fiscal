# Discover candidate fiscal-health dimensions

Uses PCA or exploratory factor analysis to propose empirical dimensions
from indicators retained by
[`fiscal_triage()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/fiscal_triage.md).
This function discovers and describes candidate groupings; it does not
name dimensions, orient scores, or construct a composite fiscal-health
index.

## Usage

``` r
fiscal_dimensions(
  data,
  triage = NULL,
  variables = NULL,
  method = c("pca", "efa"),
  nfactors = NULL,
  rotate = "oblimin",
  fm = "minres",
  missing = c("complete", "median"),
  primary_loading = 0.4,
  cross_loading = 0.3,
  min_indicators = 2L,
  parallel_iter = 100L,
  seed = NULL
)
```

## Arguments

- data:

  A data frame containing the candidate indicators.

- triage:

  An optional `fiscal_triage` object. When supplied, only its
  `selected_for_dimensions` indicators enter the model and its
  standalone indicators are carried into the result.

- variables:

  Optional character vector of indicators. Supply either `triage` or
  `variables`, not both.

- method:

  Dimension-discovery method: principal components analysis (`"pca"`) or
  exploratory factor analysis (`"efa"`).

- nfactors:

  Number of dimensions. If `NULL`, parallel analysis is used.

- rotate:

  Rotation passed to
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
  or [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html).

- fm:

  Factoring method used when `method = "efa"`.

- missing:

  Missing-data rule. `"complete"` uses complete rows; `"median"`
  replaces missing values with variable medians for this model.

- primary_loading:

  Minimum absolute loading for assignment to a candidate dimension.

- cross_loading:

  Minimum absolute secondary loading used to flag a cross-loading
  indicator.

- min_indicators:

  Minimum assigned indicators for a proposed cluster to be treated as a
  multi-indicator dimension rather than a singleton.

- parallel_iter:

  Number of simulated datasets used by parallel analysis when `nfactors`
  is `NULL`.

- seed:

  Optional seed for reproducible parallel analysis.

## Value

An object of class `fiscal_dimensions` containing the fitted model,
loading and assignment tables, proposed clusters, singleton and
unassigned indicators, triage standalones, and the complete modeling
specification.
