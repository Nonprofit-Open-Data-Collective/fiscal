# Return the vector of 990 + 990-EZ financial field names (PZ scope)

Returns financial fields that appear on both the full Form 990 and the
990-EZ (Part I summary fields). Zero-imputation via
[`sanitize_financials()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/sanitize_financials.md)
is applied to these fields for all filers.

## Usage

``` r
get_pz_fields()
```

## Value

A character vector of column names.

## Examples

``` r
get_pz_fields()
#>  [1] "F9_01_EXP_BEN_PAID_MEMB_CY"      "F9_01_EXP_GRANT_SIMILAR_CY"     
#>  [3] "F9_01_EXP_REV_LESS_EXP_CY"       "F9_01_EXP_SAL_ETC_CY"           
#>  [5] "F9_01_EXP_TOT_CY"                "F9_01_NAFB_ASSET_TOT_BOY"       
#>  [7] "F9_01_NAFB_LIAB_TOT_BOY"         "F9_01_NAFB_TOT_BOY"             
#>  [9] "F9_01_NAFB_TOT_EOY"              "F9_01_REV_INVEST_TOT_CY"        
#> [11] "F9_01_REV_PROG_TOT_CY"           "F9_01_REV_TOT_CY"               
#> [13] "F9_08_REV_CONTR_MEMBSHIP_DUE"    "F9_08_REV_MISC_TOT_TOT"         
#> [15] "F9_08_REV_OTH_EVNT_GRO"          "F9_08_REV_OTH_FUNDR_EVNT_1"     
#> [17] "F9_08_REV_OTH_GAMING"            "F9_08_REV_OTH_INV_COST_GOODS"   
#> [19] "F9_08_REV_OTH_INV_GRO_SALE"      "F9_08_REV_OTH_INV_NET_TOT"      
#> [21] "F9_08_REV_OTH_INVEST_INCOME_TOT" "F9_08_REV_OTH_SALE_GAIN_NET_TOT"
#> [23] "F9_09_EXP_OCCUPANCY_TOT"         "F9_10_ASSET_LAND_BLDG_NET_BOY"  
#> [25] "F9_10_ASSET_LAND_BLDG_NET_EOY"   "F9_10_ASSET_TOT_BOY"            
#> [27] "F9_10_ASSET_TOT_EOY"             "F9_10_LIAB_TOT_BOY"             
#> [29] "F9_10_LIAB_TOT_EOY"              "F9_10_NAFB_TOT_BOY"             
#> [31] "F9_10_NAFB_TOT_EOY"              "F9_11_RECO_OTH_CHANGE_NAFB"     
```
