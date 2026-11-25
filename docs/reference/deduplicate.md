# Select one filing per organization-year

Compatibility wrapper around
[`panel990::panel_deduplicate()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_deduplicate.html).

## Usage

``` r
deduplicate(
  df,
  by_id = "EIN2",
  by_year = "TAX_YEAR",
  col_group = "RETURN_GROUP_X",
  col_partial = "RETURN_PARTIAL_X",
  col_amended = "RETURN_AMENDED_X",
  col_stamp = "RETURN_TIME_STAMP",
  verbose = TRUE
)
```

## Arguments

- df:

  Filing data frame.

- by_id:

  Organization identifier column.

- by_year:

  Filing-year column.

- col_group:

  Group-return flag column.

- col_partial:

  Partial-return flag column.

- col_amended:

  Amended-return flag column.

- col_stamp:

  Filing timestamp column.

- verbose:

  Print a summary.

## Value

A data frame with one filing per organization-year.
