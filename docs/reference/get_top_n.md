# Compute the upper winsorization threshold for a given range specification

Compute the upper winsorization threshold for a given range
specification

## Usage

``` r
get_top_n(x, range, winsorize = 0.98, offset = 0.001)
```

## Arguments

- x:

  Numeric vector (NAs are ignored).

- range:

  Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or `"lo;hi"`).

- winsorize:

  Winsorization proportion (default `0.98`).

- offset:

  Sentinel offset for fixed bounds (default `0.001`).

## Value

A single numeric threshold value, or `NA` when no upper clipping
applies.
