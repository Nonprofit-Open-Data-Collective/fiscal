# Summarize panel composition over time

Compatibility wrapper around panel990's classification verbs.
Classification uses the `persistent`, `entrant`, `exit`, `transient`,
and `empty` boundary types and the independent `seamless`/`segmented`
spell dimension.

## Usage

``` r
panel_composition(
  df,
  time = "TAX_YEAR",
  id = "EIN2",
  append_classification = FALSE,
  return_classification = TRUE,
  print_table = TRUE
)
```

## Arguments

- df:

  A panel data frame.

- time:

  Name of the panel-time column.

- id:

  Name of the panel-ID column.

- append_classification:

  Append classification fields to every row (delegates to
  [`panel990::panel_label()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_label.html)).

- return_classification:

  Return the per-ID classification.

- print_table:

  Print the year-by-type summary (delegates to
  [`panel990::panel_describe()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_describe.html)).

## Value

An enriched panel, a per-ID classification, or (invisibly) the input.
