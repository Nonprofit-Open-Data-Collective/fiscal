# Resolve one or two candidate column names to a single numeric vector

Given one or two candidate column names and a data frame, returns a
single numeric vector. If both columns are present, they are coalesced
with the first (990 PC) taking priority and the second (990-EZ) filling
in where the first is `NA`.

## Usage

``` r
resolve_col(dat, cols)
```

## Arguments

- dat:

  A `data.frame`.

- cols:

  Character vector of one or two column names.

## Value

A numeric vector of length `nrow(dat)`.
