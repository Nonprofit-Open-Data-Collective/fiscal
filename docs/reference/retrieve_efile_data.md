# Retrieve and merge IRS 990 efile tables for one tax year

Compatibility wrapper around
[`panel990::panelize()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panelize.html).
It returns a data frame and attaches structured download, table, join,
and BMF diagnostics.

## Usage

``` r
retrieve_efile_data(
  year,
  tables = c("P00", "P01", "P08", "P09", "P10"),
  include_bmf = TRUE,
  bmf_vars = .BMF_VARS,
  join_1xm = FALSE,
  efile_root = NULL,
  format = NULL,
  bmf_url = .BMF_URL,
  timeout = 300,
  retry_max = 3L,
  path = "efdata",
  keep_files = TRUE,
  verbose = TRUE,
  backend = "memory",
  cache = NULL,
  filters = NULL,
  columns = NULL,
  collision = "error",
  overwrite = FALSE
)
```

## Arguments

- year:

  One tax year.

- tables:

  Aliases or literal canonical table names.

- include_bmf:

  Attach current geocoded BMF fields.

- bmf_vars:

  Native BMF fields to retain; `NULL` retains all available.

- join_1xm:

  Join one-to-many and supplemental tables.

- efile_root:

  Efile URL or local source directory. `NULL` (default) reads panel990's
  current release (see
  [`panel990::efile_version()`](https://nonprofit-open-data-collective.github.io/panel990/reference/efile_version.html)).

- format:

  Source file format, `"parquet"` or `"csv"`. `NULL` (default) uses
  panel990's default: parquet when a reader is installed, else CSV.

- bmf_url:

  BMF URL, path, or data frame.

- timeout:

  Download timeout.

- retry_max:

  Download attempts.

- path:

  Retained CSV cache directory.

- keep_files:

  Retain downloaded CSVs. When false, use a temporary cache.

- verbose:

  Print progress.

- backend:

  `"memory"` or `"duckdb"`.

- cache:

  Optional explicit cache mode: `"retain"`, `"temporary"`, or `"none"`.
  By default it is derived from `keep_files`.

- filters:

  Optional named source filters.

- columns:

  Optional source fields to retain.

- collision:

  Non-key collision policy.

- overwrite:

  Replace cached files.

## Value

A data frame with structured retrieval attributes.
