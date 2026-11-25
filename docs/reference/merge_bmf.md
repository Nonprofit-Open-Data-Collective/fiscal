# Merge current geocoded BMF fields onto a panel

Compatibility wrapper around
[`panel990::bmf_merge()`](https://nonprofit-open-data-collective.github.io/panel990/reference/bmf_merge.html).

## Usage

``` r
merge_bmf(df, bmf_path = .BMF_URL, timeout = 300, verbose = TRUE)
```

## Arguments

- df:

  Panel data containing `EIN2`.

- bmf_path:

  BMF URL, local CSV path, or data frame.

- timeout:

  Retained for compatibility; URL timeout uses the active R download
  settings.

- verbose:

  Print progress.

## Value

Panel rows with native BMF fields and diagnostics attached.
