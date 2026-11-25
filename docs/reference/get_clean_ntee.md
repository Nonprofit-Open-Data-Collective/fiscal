# Normalize legacy NTEE codes

Converts raw NTEE codes into a normalized 3-character form used by all
downstream classification functions. Handles padding, blank codes,
specialty nonprofits (digits 2-3 of 01-19), and 5-character legacy
codes.

## Usage

``` r
get_clean_ntee(x)
```

## Arguments

- x:

  Character vector of raw NTEE codes.

## Value

A character vector of normalized 3-character NTEE codes.

## Examples

``` r
get_clean_ntee( c("B29", "B0129", "B8443", "E0521", "", "A115") )
#> [1] "B29" "B29" "B84" "E21" "Z99" "A50"
```
