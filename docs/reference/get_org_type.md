# Classify nonprofit organization type from NTEE code

Determines the functional type of the organization (regular, advocacy,
research, funding, etc.) based on digits 2-3 of the NTEE code.

## Usage

``` r
get_org_type(x)
```

## Arguments

- x:

  Character vector of NTEE codes.

## Value

Character vector of organization type codes: AA (alliance), MT
(management/technical), PA (planning/advocacy), RP (research), MS
(single-org support), MM (multi-org support), NS (general nonprofit
support), RG (regular program organization).

## Examples

``` r
get_org_type( c("B01", "B02", "B05", "B11", "B29") )
#> [1] "AA" "MT" "RP" "MS" "RG"
```
