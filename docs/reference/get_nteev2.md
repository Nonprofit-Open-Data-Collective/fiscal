# Generate full NTEE v2 code

Combines the normalized NTEE code, industry group, and organization type
into the three-part structure: `[INDUSTRY]-[NTEE]-[ORGTYPE]`.

## Usage

``` r
get_nteev2(x)
```

## Arguments

- x:

  Character vector of raw NTEE codes.

## Value

Character vector of NTEE v2 codes (e.g., `"EDU-B29-RG"`).

## Examples

``` r
get_nteev2( c("B29", "B8443", "E0521", "B01", "B0129") )
#> [1] "EDU-B29-RG" "EDU-B84-RG" "HOS-E21-RP" "EDU-B00-AA" "EDU-B29-AA"
```
