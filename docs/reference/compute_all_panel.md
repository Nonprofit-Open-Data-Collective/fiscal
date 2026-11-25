# Compute all fiscal health metrics year-by-year across a panel

A panel-safe wrapper around
[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
that splits the data by year, computes every fiscal health metric
independently within each year's slice, then stacks the results. This
ensures that winsorization bounds, normalization parameters, and
percentile ranks are all estimated from the cross-sectional distribution
of a **single year** rather than from the pooled multi-year
distribution.

**Why year-by-year computation matters:**

[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
passes each raw ratio vector through
[`apply_transformations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_transformations.md),
which fits winsorization bounds and a normalization model on the data it
receives. On a pooled panel those distributions are a mixture of N
years, so:

- A ratio at the 95th percentile in 2021 may rank at the 80th percentile
  in the pooled distribution if 2022—2024 values are higher.

- Winsorization clips to pooled extremes, not year-specific extremes.

- Z-scores are centred and scaled on pooled moments.

Splitting by year before computing avoids all three problems: every
`_w`, `_z`, and `_p` column is interpretable as a within-year percentile
or standardised value, exactly as it would be for a single-year call to
[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md).

## Usage

``` r
compute_all_panel(
  df,
  year = "TAX_YEAR",
  metrics = c("ratio", "w", "z", "p"),
  append_to_df = TRUE,
  winsorize = 0.98,
  verbose = TRUE
)
```

## Arguments

- df:

  A `data.frame` containing IRS 990 efile financial fields for multiple
  tax years. Must contain the column named by `year`.

- year:

  Name of the column that identifies the tax year. Default `"TAX_YEAR"`.

- metrics:

  Character vector of metric variants to return. Any combination of
  `"ratio"`, `"w"`, `"z"`, `"p"`. Default: all four.

- append_to_df:

  Logical. If `TRUE` (default), returns the original `df` with metric
  columns appended. If `FALSE`, returns only `.IDVARS` identifier
  columns plus the selected metric columns.

- winsorize:

  Winsorization proportion (default `0.98`).

- verbose:

  Logical. If `TRUE` (default), prints per-year progress from
  [`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md).

## Value

A `data.frame` with the same row order as `df` and the same metric
columns that
[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
would produce, but with `_w` / `_z` / `_p` values computed within each
year's cross-sectional distribution. Rows whose `year` is `NA` are kept,
with `NA` metric columns.

## Details

### Implementation

Internally the function:

1.  Validates that `year` is present and that the panel contains more
    than one year (a single-year panel works but a warning is issued
    since
    [`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
    would give identical results).

2.  Splits the row indices of `df` by year with base
    [`split()`](https://rdrr.io/r/base/split.html). Rows with a missing
    year are set aside with a warning.

3.  Calls
    [`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
    on each year's slice with `verbose` passed through so per-year
    messages are visible (or suppressed) as requested. If a year fails,
    its rows are kept with `NA` metric columns.

4.  Tags each result with the original row positions, stacks the
    per-year results (and any rows without a year) with
    [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html),
    which fills columns missing from some years with `NA`, and restores
    the input row order.

### Panel size considerations

On large panels (e.g. 5 years – 50 000 orgs) the per-year loop runs
[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
five times on ~50 000-row slices rather than once on a 250 000-row
frame. Each slice call is faster and uses less peak memory; the
[`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
stack is O(N) with no copies.

## See also

[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md),
[`get_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_panel.md),
[`deduplicate()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/deduplicate.md),
[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md)

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- get_panel( years = 2019:2022 )
panel <- deduplicate( panel )

# Year-safe metric computation
panel_ratios <- compute_all_panel( panel )

# Compare: without year splitting (pooled --- incorrect for _w/_z/_p)
panel_ratios_pooled <- compute_all( panel )

# Check that percentile ranks are within-year:
library( data.table )
setDT( panel_ratios )
panel_ratios[ , range(debt_assets_p), by = TAX_YEAR ]
# All years should show 1 to 100.
} # }
```
