# Winsorize a numeric vector according to a range specification

Clips `x` to `[bottom, top]` bounds determined by `range` and
`winsorize`, flags which observations were reassigned to a sentinel
bound, and returns a list suitable for downstream normalization.

## Usage

``` r
winsorize_x(x, range, winsorize = 0.98, offset = 0.001)
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

A named list with elements:

- `x_w` — winsorized vector

- `top`, `bottom` — the computed bounds

- `is_top_sentinel`, `is_bottom_sentinel`, `is_sentinel` — logical
  vectors

- `support` — parsed range specification list

- `winsorize`, `offset` — echoed inputs

## Examples

``` r
x <- c( NA, rnorm(200), 100, -100 )

# winsorize with symmetric np bounds
w <- winsorize_x( x, range = "np", winsorize = 0.98 )
range( w$x_w, na.rm = TRUE )
#> [1] -2.289676  2.480132
sum( w$is_sentinel )
#> [1] 6
```
