# Retrieve a multi-year IRS 990 efile panel

Fiscal convenience wrapper around
[`panel990::panelize()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panelize.html),
optionally attaching the current geocoded BMF master.

## Usage

``` r
get_panel(
  years,
  tables = c("P00", "P01", "P08", "P09", "P10", "P11", "P12", "A01"),
  include_bmf = TRUE,
  efile_root = NULL,
  format = NULL,
  bmf_url = .BMF_URL,
  timeout = 300,
  retry_max = 3L,
  verbose = TRUE,
  backend = "memory",
  path = "efdata",
  cache = "retain",
  filters = NULL,
  columns = NULL,
  include_many = FALSE,
  collision = "error",
  overwrite = FALSE
)
```

## Arguments

- years:

  Tax years.

- tables:

  Aliases or literal table names.

- include_bmf:

  Attach BMF fields.

- efile_root:

  Efile URL or local fixture directory. `NULL` (default) reads
  panel990's current release (see
  [`panel990::efile_version()`](https://nonprofit-open-data-collective.github.io/panel990/reference/efile_version.html)).

- format:

  Source file format, `"parquet"` or `"csv"`. `NULL` (default) uses
  panel990's default: parquet when a reader is installed, else CSV.

- bmf_url:

  BMF URL, local path, or in-memory data frame.

- timeout:

  Download timeout.

- retry_max:

  Download attempts.

- verbose:

  Print progress.

- backend:

  Retrieval backend; `"memory"` or `"duckdb"`.

- path:

  Retained CSV cache directory.

- cache:

  `"retain"` or `"temporary"`.

- filters:

  Optional named source filters.

- columns:

  Optional fields to retain.

- include_many:

  Join one-to-many and supplemental tables.

- collision:

  Non-key collision policy.

- overwrite:

  Replace cached CSVs.

## Value

A data frame with retrieval manifests attached as attributes.
