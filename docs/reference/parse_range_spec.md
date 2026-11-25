# Parse a range specification string into a support list

Parse a range specification string into a support list

## Usage

``` r
parse_range_spec(range, offset = 0.001)
```

## Arguments

- range:

  Character string: `"np"`, `"zp"`, `"zo"`, `"nz"`, or `"lo;hi"`.

- offset:

  Numeric sentinel offset applied to fixed bounds. Default `0.001`.

## Value

A named list with elements `code`, `lo`, `hi`, `sentinel_lo`,
`sentinel_hi`, and `offset`.
