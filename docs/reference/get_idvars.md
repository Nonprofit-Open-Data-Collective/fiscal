# Return the list of efile record identifier variables

Returns the character vector of column names that identify a filing
record across all efile tables. These are retained alongside financial
fields in the working subset inside every `get_*()` function.

## Usage

``` r
get_idvars()
```

## Value

A character vector of column names.

## Examples

``` r
get_idvars()
#>  [1] "EIN2"                  "OBJECTID"              "ORG_EIN"              
#>  [4] "ORG_NAME_L1"           "ORG_NAME_L2"           "RETURN_AMENDED_X"     
#>  [7] "RETURN_GROUP_X"        "RETURN_PARTIAL_X"      "RETURN_TAXPER_DAYS"   
#> [10] "RETURN_TIME_STAMP"     "RETURN_TYPE"           "TAX_PERIOD_BEGIN_DATE"
#> [13] "TAX_PERIOD_END_DATE"   "TAX_YEAR"              "URL"                  
#> [16] "VERSION"              
```
