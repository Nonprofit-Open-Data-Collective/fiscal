# Detect 990-EZ filer rows in an efile dataset

Returns a logical vector marking rows that belong to 990-EZ filers. Uses
the `RETURN_TYPE` column when present; otherwise infers filer type from
field availability (rows with Part I data but missing Part VIII total
revenue are treated as 990-EZ filers).

## Usage

``` r
detect_ez_rows(df)
```

## Arguments

- df:

  A `data.frame` containing efile data.

## Value

A logical vector of length `nrow(df)`.

## Examples

``` r
data( dat10k )
table( detect_ez_rows( dat10k ) )
#> 
#> FALSE  TRUE 
#>  6051  3949 
```
