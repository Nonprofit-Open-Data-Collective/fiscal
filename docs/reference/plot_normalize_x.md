# Plot raw and normalized versions of a numeric vector

Diagnostic companion to
[`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md).
Plots side-by-side histograms of the raw and normalized distributions.

## Usage

``` r
plot_normalize_x(
  x,
  range = NULL,
  normalize = TRUE,
  breaks = 40,
  col_raw = "steelblue",
  col_norm = "firebrick",
  border = "white",
  main_raw = "Raw",
  main_norm = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector (raw input, or already-normalized if
  `normalize = FALSE`).

- range:

  Character range code passed to
  [`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md)
  when `normalize = TRUE`. Required unless `normalize = FALSE`.

- normalize:

  Logical. If `TRUE` (default), normalize `x` inside the function. If
  `FALSE`, treat `x` as already normalized.

- breaks:

  Number of histogram breaks. Default `40`.

- col_raw:

  Fill color for the raw histogram. Default `"steelblue"`.

- col_norm:

  Fill color for the normalized histogram. Default `"firebrick"`.

- border:

  Bar border color. Default `"white"`.

- main_raw:

  Title for the raw panel. Default `"Raw"`.

- main_norm:

  Optional title for the normalized panel. If `NULL`, uses the
  `transform_label` attribute when available.

- ...:

  Additional arguments passed to
  [`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md)
  when `normalize = TRUE`.

## Value

Invisibly returns a list with elements `raw` and `normalized`.

## See also

[`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md),
[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md)
