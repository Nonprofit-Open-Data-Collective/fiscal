library( tidyverse )
library( data.table )
library( fiscal )

###
###  HELPER FUNCTIONS FOR STANDARDIZING BMF FIELDS 
###


format_ein <- function( x, to="id" ) {
  
  if( to == "id" )
  {
    x <- stringr::str_pad( x, 9, side="left", pad="0" )
    sub1 <- substr( x, 1, 2 )
    sub2 <- substr( x, 3, 9 )
    ein  <- paste0( "EIN-", sub1, "-", sub2 ) 
    return(ein)
  }
  
  if( to == "n" )
  {
    x <- gsub( "[^0-9]", "", x )
    return( x )
  }
  
}



#' Normalize legacy NTEE codes
#'
#' Converts legacy NTEE codes into a normalized form for consistent
#' classification by downstream functions. Handles padding, blank codes,
#' and special cases for specialty nonprofits (01–19) and subsector
#' overrides where digits 4–5 encode different industries (e.g., UNI, HOS).
#'
#' @param x Character vector of NTEE codes.
#'
#' @details
#' Rules:
#' \itemize{
#'   \item If `nchar(x) == 3`: use as-is (e.g., `"B29"` ? `"B29"`).
#'   \item If `nchar(x) == 4`: pad with `"0"` on the right (e.g., `"A115"` ? `"A1150"`).
#'   \item If `nchar(x) == 5`: specialty orgs (`01–19`) take the last two digits (`"B0129"` ? `"B29"`);
#'         other codes retain the first three (`"B8443"` ? `"B84"`).
#'   \item Blank or missing ? `"Z99"`.
#' }
#'
#' @return A character vector of normalized 3-character NTEE codes.
#' @examples
#' get_clean_ntee(c("B29", "B0129", "B8443", "E0521", "", "A115"))
#' @export
get_clean_ntee <- function(x) {
  x <- toupper(trimws(x))
  x[is.na(x) | x == ""] <- "Z99"
  
  # Pad 4-character codes (A115 -> A1150)
  x[nchar(x) == 4] <- stringr::str_pad(x[nchar(x) == 4], width = 5, side = "right", pad = "0")
  
  letter    <- substr(x, 1, 1)
  first_two <- substr(x, 2, 3)
  last_two  <- substr(x, 4, 5)
  
  # Handle non-numeric first_two
  first_two_num <- suppressWarnings(as.numeric(first_two))
  first_two_num[is.na(first_two_num)] <- 99
  
  ntee <- substr(x, 1, 3)  # default (e.g. B29)
  ntee45 <- paste0(letter, last_two)
  ntee00 <- paste0(letter, "00")
  
  # Specialty nonprofits
  specialty3 <- first_two_num <= 19 & nchar(x) == 3
  specialty5 <- first_two_num <= 19 & nchar(x) == 5
  
  ntee[specialty3] <- ntee00[specialty3]
  ntee[specialty5] <- ntee45[specialty5]
  
  # Edge case: digits 4–5 encode industry groups (UNI, HOS)
  # Keep default first 3 chars for NTEE; industry will interpret the last two.
  ntee
}


#' Derive top-level NTEE industry classification
#'
#' Maps normalized NTEE codes to their high-level industry group.
#'
#' @param x Character vector of NTEE codes (raw or cleaned).
#'
#' @details
#' Special overrides:
#' \itemize{
#'   \item `"B40"–"B43"`, `"B50"` ? `"UNI"` (Universities)
#'   \item `"E20"–"E24"` ? `"HOS"` (Hospitals)
#' }
#' Then classifies by first letter:
#' \itemize{
#'   \item A = ART, B = EDU, C–D = ENV, E–H = HEL,
#'   I–P = HMS, Q = IFA, R–W = PSB, X = REL, Y = MMB, Z = UNU
#' }
#'
#' @return Character vector of 3-letter industry codes.
#' @examples
#' get_industry(c("B29", "B8443", "E0521", "Q12", "Z99"))
#' @export
get_industry <- function(x) {
  x <- toupper(trimws(x))
  x[is.na(x) | x == ""] <- "Z99"
  
  # Normalize code: pad and derive clean_ntee for industry lookup
  x[nchar(x) == 4] <- stringr::str_pad(x[nchar(x) == 4], width = 5, side = "right", pad = "0")
  letter   <- substr(x, 1, 1)
  last_two <- substr(x, 4, 5)
  ntee_ind <- substr(x, 1, 3)
  ntee_ind[nchar(x) == 5] <- paste0(letter[nchar(x) == 5], last_two[nchar(x) == 5])
  
  # Vectorized classification
  dplyr::case_when(
    ntee_ind %in% c("B40", "B41", "B42", "B43", "B50") ~ "UNI",
    ntee_ind %in% c("E20", "E21", "E22", "E24")       ~ "HOS",
    substr(ntee_ind, 1, 1) == "A" ~ "ART",
    substr(ntee_ind, 1, 1) == "B" ~ "EDU",
    substr(ntee_ind, 1, 1) %in% c("C", "D") ~ "ENV",
    substr(ntee_ind, 1, 1) %in% c("E", "F", "G", "H") ~ "HEL",
    substr(ntee_ind, 1, 1) %in% LETTERS[9:16] ~ "HMS", # I–P
    substr(ntee_ind, 1, 1) == "Q" ~ "IFA",
    substr(ntee_ind, 1, 1) %in% c("R","S","T","U","V","W") ~ "PSB",
    substr(ntee_ind, 1, 1) == "X" ~ "REL",
    substr(ntee_ind, 1, 1) == "Y" ~ "MMB",
    substr(ntee_ind, 1, 1) == "Z" ~ "UNU",
    TRUE ~ "UNU"
  )
}


#' Classify nonprofit organization type
#'
#' Determines whether the code represents a regular nonprofit or
#' a specialized entity (advocacy, research, funding, etc.).
#'
#' @param x Character vector of NTEE codes.
#' @return Character vector of organization type codes (AA, MT, PA, RP, MS, MM, NS, or RG).
#' @examples
#' get_org_type(c("B01", "B02", "B05", "B11", "B29"))
#' @export
get_org_type <- function(x) {
  x <- toupper(trimws(x))
  x[x == "" | is.na(x)] <- "Z99"
  two <- suppressWarnings(as.numeric(substr(x, 2, 3)))
  
  dplyr::case_when(
    two == 1  ~ "AA",
    two == 2  ~ "MT",
    two == 3  ~ "PA",
    two == 5  ~ "RP",
    two == 11 ~ "MS",
    two == 12 ~ "MM",
    two == 19 ~ "NS",
    TRUE      ~ "RG"
  )
}


#' Generate full NTEE v2 code
#'
#' Combines normalized NTEE, industry, and organization type
#' into the standardized three-part structure: `[INDUSTRY]-[NTEE]-[ORGTYPE]`.
#'
#' @param x Character vector of NTEE codes.
#'
#' @return Character vector of NTEE v2 codes.
#' @examples
#' get_nteev2(c("B29", "B8443", "E0521", "B01", "B0129"))
#' @export
get_nteev2 <- function(x) {
  ntee     <- get_clean_ntee(x)
  industry <- get_industry(x)
  org_type <- get_org_type(x)
  paste0(industry, "-", ntee, "-", org_type)
}





#' Convert NTEE V2 Codes to NTMAJ12 Major Groups with Labels
#'
#' Translates NTEE V2 codes (e.g., `"HOS-E05-RG"`, `"EDU-B29-RG"`, `"HOS-E05-RG"`) 
#' into their corresponding NTMAJ12 major group factor levels with descriptive labels 
#' (e.g., `"ARTS"`, `"EDUCATION"`, `"RELIGION"`).
#'
#' This function is useful when you want to collapse detailed NTEE V2 codes 
#' into the 12 major nonprofit categories used by NCCS in the NTMAJ12 classification system.
#'
#' @param nteev2 A character vector of NTEE V2 codes (typically the first three characters
#'   of an NTEE code). Codes that are not recognized will be returned as `NA`.
#'
#' @return A factor vector with 12 levels corresponding to the NTMAJ12 categories:
#'   `"ARTS"`, `"EDUCATION"`, `"HIGHER EDU"`, `"ENVIRONMENT"`, `"HEALTH"`, 
#'   `"HOSPITALS"`, `"INTERNATIONAL"`, `"PUBLIC BENEFIT"`, `"MEMBERSHIP"`, 
#'   `"UNCLASSIFIED"`, `"RELIGION"`, `"HUMAN SERVICES"`.
#'
#' @examples
#' get_ntmaj12(c("HOS-E05-RG", "EDU-B29-RG", "HOS-E05-RG"))
#' #> [1] ARTS  EDUCATION  HOSPITALS
#' #> Levels: ARTS EDUCATION HIGHER ED ENVIRONMENT HEALTH 
#' #>         HOSPITALS HUMAN SERVICES MEMBERSHIP INTERNATIONAL   
#' #>         UNCLASSIFIED PUBLIC BENEFIT RELIGION 
#'
#' @seealso
#' \code{\link{get_industry}} for extracting NTMAJ12 from a regular NTEE code.
#'
#' @export
get_ntmaj12 <- function( nteev2 ){
 
  ind <- 
    c("ART" = "ARTS {ART}",
      "EDU" = "EDUCATION {EDU}",
      "UNI" = "HIGHER ED {UNI}",
      "ENV" = "ENVIRONMENT {ENV}",
      "HEL" = "HEALTH {HEL}",
      "HOS" = "HOSPITALS {HOS}",
      "IFA" = "INTERNATIONAL {IFA}",
      "PSB" = "PUBLIC BENEFIT  {PSB}",
      "MMB" = "MEMBERSHIP {MMB}",
      "UNU" = "UNCLASSIFIED {UNU}",
      "REL" = "RELIGION {REL}",
      "HMS" = "HUMAN SERVICES {HMS}" )

  x <- substr( nteev2, 1, 3 )

  f <- 
    factor( x, 
            levels = names(ind), 
            labels = ind)

  return(f)
}





###
###  GET EFILE TABLES 
###

root <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"
YEAR <- 2021

fn1  <- paste0( "F9-P00-T00-HEADER-", YEAR, ".CSV" )
fn2  <- paste0( "F9-P01-T00-SUMMARY-", YEAR, ".CSV" )
fn3  <- paste0( "F9-P08-T00-REVENUE-", YEAR, ".CSV" )
fn4  <- paste0( "F9-P09-T00-EXPENSES-", YEAR, ".CSV" )
fn5  <- paste0( "F9-P10-T00-BALANCE-SHEET-", YEAR, ".CSV" )

path1 <- paste0( "data/", fn1 )
path2 <- paste0( "data/", fn2 )
path3 <- paste0( "data/", fn3 )
path4 <- paste0( "data/", fn4 )
path5 <- paste0( "data/", fn5 )

download.file( paste0( root, fn1 ), destfile=path1 )
download.file( paste0( root, fn2 ), destfile=path2 )
download.file( paste0( root, fn3 ), destfile=path3 )
download.file( paste0( root, fn4 ), destfile=path4 )
download.file( paste0( root, fn5 ), destfile=path5 )


###
###  GET BMF FIELDS 
###

nccs_s3  <- "https://nccsdata.s3.us-east-1.amazonaws.com/"
bmf_url  <- "bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv"
bmf_path <- "data/UNIFIED_BMF_V1.2.csv"

download.file( paste0( nccs_s3, bmf_url ), destfile="data/UNIFIED_BMF_V1.2.csv" )


###
###  LOAD DATA 
###

d1 <- data.table::fread( path1 ) |> unique()
d2 <- data.table::fread( path2 ) |> unique()
d3 <- data.table::fread( path3 ) |> unique()
d4 <- data.table::fread( path4 ) |> unique()
d5 <- data.table::fread( path5 ) |> unique()

bmf <- data.table::fread( bmf_path ) |> unique()

bmf2 <- 
  bmf %>%
  arrange( EIN, desc(ORG_RULING_DATE) ) %>%
  filter( ! duplicated(EIN) )

ntee <- bmf2$NTEE_NCCS
ntee[ ntee == "" | is.na(ntee) ] <- bmf2$NTEE_IRS[ ntee == "" | is.na(ntee) ]

bmf2$NTEE_NCCS <- get_clean_ntee( ntee )
bmf2$NTEEV2 <- get_nteev2( ntee )
bmf2$NTMAJ12 <- get_industry( ntee )
bmf2$NTEE_ORG_TYPE <- get_org_type( ntee )


keep <- 
c("EIN2", "NTEE_NCCS", "NTEEV2", "NTMAJ12", "NTEE_ORG_TYPE",
"CENSUS_CBSA_FIPS", "CENSUS_CBSA_NAME", 
"CENSUS_BLOCK_FIPS", "CENSUS_URBAN_AREA", 
"CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME", 
"BMF_SUBSECTION_CODE", "BMF_FOUNDATION_CODE",
"ORG_RULING_YEAR",
"F990_TOTAL_REVENUE_RECENT", "F990_TOTAL_INCOME_RECENT", 
"F990_TOTAL_ASSETS_RECENT", "F990_TOTAL_EXPENSES_RECENT" )

bmf2 <- select( bmf2, all_of(keep) )






### 
###  COMBINE FILES
###

dim( d1 )

df <- merge( d1, d2, all=TRUE )
df <- merge( df, d3, all=TRUE )
df <- merge( df, d4, all=TRUE )
df <- merge( df, d5, all=TRUE )

dim( df )

df <- merge( df, bmf2, by="EIN2", all.x=TRUE )

dim( df )


fwrite( df, "DATA-2021.CSV" )



###
###  CREATE SAMPLE FOR PACKAGE TESTING
###

set.seed(123)

ds <- 
  df %>%
  filter( ! ( NTEE_NCCS == "" | is.na(NTEE_NCCS) ) ) %>%
  filter( F9_01_REV_TOT_CY >= 0 ) %>%
  sample_n( 10000 )


v1 <- 
c("EIN2", "NTEE_NCCS", "NTEEV2", "NTMAJ12", "NTEE_ORG_TYPE",
"CENSUS_CBSA_FIPS", "CENSUS_CBSA_NAME", 
"CENSUS_BLOCK_FIPS", "CENSUS_URBAN_AREA", 
"CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME", 
"BMF_SUBSECTION_CODE", "BMF_FOUNDATION_CODE",
"ORG_RULING_YEAR",
"F990_TOTAL_REVENUE_RECENT", "F990_TOTAL_INCOME_RECENT", 
"F990_TOTAL_ASSETS_RECENT", "F990_TOTAL_EXPENSES_RECENT" )

v2 <- 
c("EIN2", "OBJECTID", "ORG_EIN", "ORG_NAME_L1", "ORG_NAME_L2", 
"RETURN_AMENDED_X", "RETURN_GROUP_X", "RETURN_PARTIAL_X", "RETURN_TAXPER_DAYS", 
"RETURN_TIME_STAMP", "RETURN_TYPE", "TAX_PERIOD_BEGIN_DATE", 
"TAX_PERIOD_END_DATE", "TAX_YEAR", "URL", "VERSION",  
"F9_00_BUILD_TIME_STAMP", "F9_00_NAME_ORG_CTRL", "F9_00_RETURN_TIME_STAMP", 
"F9_00_RETURN_TYPE", "F9_00_TAX_PERIOD_BEGIN_DATE", "F9_00_TAX_PERIOD_END_DATE", 
"F9_00_TAX_YEAR", "F9_00_RETURN_GROUP_X", "F9_00_EXEMPT_STAT_4947A1_X", 
"F9_00_EXEMPT_STAT_501C_X", "F9_00_EXEMPT_STAT_501C3_X", 
"F9_00_TYPE_ORG_OTH_DESC", "F9_00_TYPE_ORG_ASSOC_X", "F9_00_TYPE_ORG_CORP_X", 
"F9_00_TYPE_ORG_OTH_X", "F9_00_TYPE_ORG_TRUST_X", "F9_00_YEAR_FORMATION")

v3 <- c(
  "F9_01_EXP_REV_LESS_EXP_CY",
  "F9_01_EXP_TOT_CY",
  "F9_01_NAFB_ASSET_TOT_EOY",
  "F9_01_NAFB_LIAB_TOT_EOY",
  "F9_01_NAFB_TOT_BOY",
  "F9_01_NAFB_TOT_EOY",
  "F9_10_NAFB_UNRESTRICT_EOY",
  "F9_01_REV_PROG_TOT_CY",
  "F9_01_REV_TOT_CY",
  "F9_08_REV_CONTR_GOVT_GRANT",
  "F9_08_REV_CONTR_MEMBSHIP_DUE",
  "F9_08_REV_CONTR_TOT",
  "F9_08_REV_MISC_OTH_TOT",
  "F9_08_REV_OTH_FUNDR_NET_TOT",
  "F9_08_REV_OTH_INVEST_BOND_TOT",
  "F9_08_REV_OTH_INVEST_INCOME_TOT",
  "F9_08_REV_OTH_RENT_GRO_PERS",
  "F9_08_REV_OTH_ROY_TOT",
  "F9_08_REV_OTH_SALE_ASSET_OTH",
  "F9_08_REV_PROG_TOT_TOT",
  "F9_08_REV_TOT_TOT",
  "F9_09_EXP_DEPREC_TOT",
  "F9_09_EXP_TOT_FUNDR",
  "F9_09_EXP_TOT_MGMT",
  "F9_09_EXP_TOT_PROG",
  "F9_09_EXP_TOT_TOT",
  "F9_10_ASSET_ACC_NET_EOY",
  "F9_10_ASSET_CASH_BOY",
  "F9_10_ASSET_CASH_EOY",
  "F9_10_ASSET_EXP_PREPAID_EOY",
  "F9_10_ASSET_INV_SALE_EOY",
  "F9_10_ASSET_LAND_BLDG_DEPREC",
  "F9_10_ASSET_LAND_BLDG_NET_EOY",
  "F9_10_ASSET_PLEDGE_NET_EOY",
  "F9_10_ASSET_SAVING_EOY",
  "F9_10_ASSET_TOT_EOY",
  "F9_10_LIAB_ACC_PAYABLE_EOY",
  "F9_10_LIAB_GRANT_PAYABLE_EOY",
  "F9_10_LIAB_MTG_NOTE_EOY",
  "F9_10_LIAB_TOT_EOY",
  "F9_10_NAFB_TOT_BOY",
  "F9_10_NAFB_TOT_EOY",
  "F9_10_NAFB_UNRESTRICT_BOY",
  "F9_10_NAFB_UNRESTRICT_EOY"
)

keep2 <- unique( c(v1,v2,v3) )

dat10k <- select( ds, all_of(keep2) )

usethis::use_data( dat10k, overwrite = TRUE, compress = "xz" )






