# Convert NTEE V2 codes to NTMAJ12 major group labels

Convert NTEE V2 codes to NTMAJ12 major group labels

## Usage

``` r
get_ntmaj12(nteev2)
```

## Arguments

- nteev2:

  Character vector of NTEE V2 codes.

## Value

Factor vector with 12 levels (ARTS, EDUCATION, HIGHER ED, etc.).

## Examples

``` r
get_ntmaj12( c("HOS-E05-RG", "EDU-B29-RG") )
#> [1] HOSPITALS {HOS} EDUCATION {EDU}
#> 12 Levels: ARTS {ART} EDUCATION {EDU} HIGHER ED {UNI} ... HUMAN SERVICES {HMS}
```
