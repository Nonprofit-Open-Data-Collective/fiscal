# Smooth financial variables within panel organizations

Compatibility wrapper around
[`panel990::panel_smooth()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_smooth.html).
Fiscal retains the `PZ`, `PC`, and `ALL` field shortcuts.

## Usage

``` r
panel_smooth(
  df,
  vars = "PZ",
  window = 3,
  time = "TAX_YEAR",
  id = "EIN2",
  weights = "equal",
  verbose = TRUE,
  engine = "memory",
  align = "center"
)
```

## Arguments

- df:

  Panel data frame.

- vars:

  `"PZ"`, `"PC"`, `"ALL"`, or numeric field names.

- window:

  Rolling-window width: odd for `align = "center"`, any positive integer
  otherwise.

- time:

  Panel-time column.

- id:

  Panel-ID column.

- weights:

  Weighting mode.

- verbose:

  Print progress.

- engine:

  Must currently be `"memory"`.

- align:

  Window alignment, passed to
  [`panel990::panel_smooth()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_smooth.html):
  `"center"` (default) averages the rows centred on each year; `"right"`
  is a trailing window of calendar years `t - (window - 1)` through `t`
  that never uses future values or bridges filing gaps; `"left"` is the
  forward-looking mirror.

## Value

Smoothed panel data.
