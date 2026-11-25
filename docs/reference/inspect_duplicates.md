# Inspect duplicate filings before deduplication

Diagnoses duplicated organization-year filings using the same ranking
logic as
[`deduplicate()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/deduplicate.md).
For each duplicated `by_id` x `by_year` group, the function identifies
the row that would be retained and classifies each candidate filing by
filing type.

Filing types are:

- `"normal"`: not amended, not group, not partial

- `"partial"`: partial only

- `"amended"`: amended only

- `"group"`: group only

- `"amended_partial"`: amended + partial

- `"amended_group"`: amended + group

- `"group_partial"`: group + partial

- `"amended_group_partial"`: amended + group + partial

The retained filing is indicated separately by the logical column
`retained`.

## Usage

``` r
inspect_duplicates(
  df,
  by_id = "EIN2",
  by_year = "TAX_YEAR",
  col_group = "RETURN_GROUP_X",
  col_partial = "RETURN_PARTIAL_X",
  col_amended = "RETURN_AMENDED_X",
  col_stamp = "RETURN_TIME_STAMP",
  return_candidates = FALSE
)
```

## Arguments

- df:

  A `data.frame` containing IRS 990 e-file data.

- by_id:

  Name of the organization identifier column. Default `"EIN2"`.

- by_year:

  Name of the tax year column. Default `"TAX_YEAR"`.

- col_group:

  Name of the group return flag column. Default `"RETURN_GROUP_X"`.

- col_partial:

  Name of the partial-year return flag column. Default
  `"RETURN_PARTIAL_X"`.

- col_amended:

  Name of the amended return flag column. Default `"RETURN_AMENDED_X"`.

- col_stamp:

  Name of the filing timestamp column. Default `"RETURN_TIME_STAMP"`.

- return_candidates:

  Logical. If `TRUE`, return the full duplicate candidate table with
  ranking columns. Default is `FALSE`.

## Value

A list with:

- summary_types:

  Count of duplicated candidate rows by filing type, with total, kept,
  and dropped counts.

- summary_sizes:

  Count of duplicated org-years by number of filings.

- summary_size_by_type:

  Count of duplicated candidate rows by number of filings and filing
  type, with total, kept, and dropped counts.

- examples:

  One representative duplicated org-year for each number of filings,
  including all candidate rows in that example group.

- candidates:

  Optional full duplicate candidate table, returned only if
  `return_candidates = TRUE`.
