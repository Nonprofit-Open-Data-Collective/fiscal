# Derive top-level NTEE industry classification

Maps raw NTEE codes to their 3-letter high-level industry group. Special
overrides: B40-B43/B50 ? UNI (Universities), E20-E24 ? HOS (Hospitals).

## Usage

``` r
get_industry(x)
```

## Arguments

- x:

  Character vector of NTEE codes (raw or cleaned).

## Value

Character vector of 3-letter industry codes.

## Examples

``` r
get_industry( c("B29", "B8443", "E0521", "Q12", "Z99") )
#> [1] "EDU" "EDU" "HOS" "IFA" "UNU"
```
