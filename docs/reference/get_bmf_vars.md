# Return the list of default BMF variables

Returns the character vector of BMF-derived and BMF-retained variables
appended by default when
[`retrieve_efile_data()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/retrieve_efile_data.md)
is called with `include_bmf = TRUE`.

## Usage

``` r
get_bmf_vars()
```

## Value

A character vector of column names.

## Details

These include native normalized NTEE classifications, BMF administrative
fields, recent financial amounts, geocoded location and quality fields,
and source/vintage provenance from the current BMF master.

## Examples

``` r
get_bmf_vars()
#>  [1] "EIN2"                               "org_name_display"                  
#>  [3] "ntee_code_clean"                    "ntee_code_major_group"             
#>  [5] "nteev2"                             "nteev2_subsector"                  
#>  [7] "nteev2_org_type"                    "subsection_code"                   
#>  [9] "foundation_code"                    "foundation_code_definition"        
#> [11] "ruling_date"                        "ruling_date_is_missing"            
#> [13] "ruling_year"                        "filing_requirement_code"           
#> [15] "filing_requirement_code_definition" "asset_amount"                      
#> [17] "income_amount"                      "revenue_amount"                    
#> [19] "org_addr_state"                     "org_addr_zip5"                     
#> [21] "geo_is_geocoded"                    "geo_lat"                           
#> [23] "geo_lon"                            "geo_state_abbr"                    
#> [25] "geo_county"                         "geo_metro_area"                    
#> [27] "geo_score"                          "geo_status"                        
#> [29] "bmf_source"                         "bmf_vintage_ym"                    
```
