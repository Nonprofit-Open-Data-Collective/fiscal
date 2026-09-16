#' IRS 990 Efile Dataset - 2021 Tax Year (10,000 Organizations)
#'
#' A random sample of 10,000 nonprofit filings from the 2021 IRS 990 efile
#' database, combining financial data from Form 990 Parts I, VIII, IX, and X with
#' organizational metadata from the IRS Business Master File (BMF).
#'
#' @format A `data.table` (and `data.frame`) with 10,000 rows and 93 variables:
#'   6,284 Form 990 filers and 3,716 Form 990-EZ filers, identified by
#'   `RETURN_TYPE`. Dollar amounts are whole dollars stored as plain R numbers
#'   (double or integer). Form line references are to the 2021 Form 990, with
#'   the equivalent 990-EZ line where 990-EZ filers report the field.
#'   Variables are organized into the following groups:
#'
#' ## BMF classification, geography, and summary financials
#'
#' - `EIN2`: Employer Identification Number, formatted `"EIN-XX-XXXXXXX"`
#' - `NTEE_NCCS`: Normalized 3-character NTEE code (e.g. `"B94"`); the
#'   NCCS NTEE code, or the IRS NTEE code where NCCS has none, cleaned with
#'   [get_clean_ntee()]
#' - `NTEEV2`: NTEE Version 2 code, `[INDUSTRY]-[NTEE]-[ORGTYPE]`
#'   (e.g. `"EDU-B94-RG"`); see [get_nteev2()]
#' - `NTMAJ12`: 3-letter major industry group (`ART`, `EDU`, `UNI`, `ENV`,
#'   `HEL`, `HOS`, `IFA`, `PSB`, `MMB`, `UNU`, `REL`, `HMS`); see
#'   [get_industry()]
#' - `NTEE_ORG_TYPE`: Organization type code (`RG`, `AA`, `MT`, `PA`, `RP`,
#'   `MS`, `MM`, `NS`); see [get_org_type()]
#' - `CENSUS_CBSA_FIPS`: Core-Based Statistical Area FIPS code (integer;
#'   `NA` outside a CBSA)
#' - `CENSUS_CBSA_NAME`: CBSA name (metro/micro area; `""` outside a CBSA)
#' - `CENSUS_BLOCK_FIPS`: 15-digit Census block FIPS code, stored as a
#'   number, so codes for states with FIPS below 10 lose their leading zero
#' - `CENSUS_URBAN_AREA`: `"U"` (urban), `"R"` (rural), or `""`
#' - `CENSUS_STATE_ABBR`: Two-letter state abbreviation
#' - `CENSUS_COUNTY_NAME`: County name
#' - `BMF_SUBSECTION_CODE`: IRS 501(c) subsection code (e.g. `3` = 501(c)(3))
#' - `BMF_FOUNDATION_CODE`: IRS foundation code
#' - `ORG_RULING_YEAR`: Year of the IRS ruling granting tax-exempt status
#' - `F990_TOTAL_REVENUE_RECENT`: Most recent total revenue from BMF
#' - `F990_TOTAL_INCOME_RECENT`: Most recent total income from BMF
#' - `F990_TOTAL_ASSETS_RECENT`: Most recent total assets from BMF
#' - `F990_TOTAL_EXPENSES_RECENT`: Most recent total expenses from BMF
#'
#' ## Return header fields (Part 00)
#'
#' - `OBJECTID`: Efile object identifier (`"OID-"` plus the IRS object ID);
#'   unique per filing
#' - `ORG_EIN`: Organization EIN as an integer (leading zeros dropped)
#' - `ORG_NAME_L1`: Organization name (line 1)
#' - `ORG_NAME_L2`: Organization name (line 2; usually `""`)
#' - `RETURN_AMENDED_X`: Amended return indicator (logical)
#' - `RETURN_GROUP_X`: Group return indicator (logical)
#' - `RETURN_PARTIAL_X`: Partial-year return indicator (logical)
#' - `RETURN_TAXPER_DAYS`: Length of tax period in days
#' - `RETURN_TIME_STAMP`: Date and time the return was created (`POSIXct`)
#' - `RETURN_TYPE`: Form type filed: `"990"` or `"990EZ"`
#' - `TAX_PERIOD_BEGIN_DATE`: Start of tax period (`IDate`)
#' - `TAX_PERIOD_END_DATE`: End of tax period (`IDate`)
#' - `TAX_YEAR`: Tax year as reported on the return (`2021` for every row)
#' - `URL`: URL of the source XML filing on AWS
#' - `VERSION`: IRS efile schema version (e.g. `"2021v4.1"`)
#' - `F9_00_BUILD_TIME_STAMP`: IRS build timestamp (`POSIXct`)
#' - `F9_00_NAME_ORG_CTRL`: IRS name control (up to four characters)
#' - `F9_00_RETURN_TIME_STAMP`: Return creation timestamp; same as
#'   `RETURN_TIME_STAMP`
#' - `F9_00_RETURN_TYPE`: Return type from the XML header; same as `RETURN_TYPE`
#' - `F9_00_TAX_PERIOD_BEGIN_DATE`: Tax period start date; same as
#'   `TAX_PERIOD_BEGIN_DATE`
#' - `F9_00_TAX_PERIOD_END_DATE`: Tax period end date; same as
#'   `TAX_PERIOD_END_DATE`
#' - `F9_00_TAX_YEAR`: Tax year; same as `TAX_YEAR`
#' - `F9_00_RETURN_GROUP_X`: Group return flag as reported in the XML
#'   (`"0"`, `"1"`, `"false"`, or `"true"`; `""` for 990-EZ filers)
#' - `F9_00_EXEMPT_STAT_4947A1_X`: 4947(a)(1) trust indicator (`"X"` or `""`)
#' - `F9_00_EXEMPT_STAT_501C_X`: 501(c) exempt status indicator (`"X"` or `""`)
#' - `F9_00_EXEMPT_STAT_501C3_X`: 501(c)(3) indicator (`"X"` or `""`)
#' - `F9_00_TYPE_ORG_OTH_DESC`: Other form-of-organization description
#' - `F9_00_TYPE_ORG_ASSOC_X`: Association indicator (`"X"` or `""`)
#' - `F9_00_TYPE_ORG_CORP_X`: Corporation indicator (`"X"` or `""`)
#' - `F9_00_TYPE_ORG_OTH_X`: Other form-of-organization indicator (`"X"` or `""`)
#' - `F9_00_TYPE_ORG_TRUST_X`: Trust indicator (`"X"` or `""`)
#' - `F9_00_YEAR_FORMATION`: Year of formation (990 filers only)
#'
#' ## Part I - Summary
#'
#' - `F9_01_EXP_REV_LESS_EXP_CY`: Revenue less expenses, current year
#'   (Part I, line 19; 990-EZ line 18)
#' - `F9_01_EXP_TOT_CY`: Total expenses, current year (Part I, line 18;
#'   990-EZ line 17)
#' - `F9_01_NAFB_ASSET_TOT_EOY`: Total assets, EOY (Part I, line 20;
#'   990 filers only)
#' - `F9_01_NAFB_LIAB_TOT_EOY`: Total liabilities, EOY (Part I, line 21;
#'   990 filers only)
#' - `F9_01_NAFB_TOT_BOY`: Net assets or fund balances, BOY (Part I, line 22;
#'   990-EZ line 19)
#' - `F9_01_NAFB_TOT_EOY`: Net assets or fund balances, EOY (Part I, line 22;
#'   990-EZ line 21)
#' - `F9_01_REV_PROG_TOT_CY`: Program service revenue, current year
#'   (Part I, line 9; 990-EZ line 2)
#' - `F9_01_REV_TOT_CY`: Total revenue, current year (Part I, line 12;
#'   990-EZ line 9)
#'
#' ## Part VIII - Revenue
#'
#' - `F9_08_REV_CONTR_GOVT_GRANT`: Government grants (Part VIII, line 1e)
#' - `F9_08_REV_CONTR_MEMBSHIP_DUE`: Membership dues (Part VIII, line 1b;
#'   990-EZ Part I, line 3)
#' - `F9_08_REV_CONTR_TOT`: Total contributions (Part VIII, line 1h)
#' - `F9_08_REV_MISC_OTH_TOT`: All other miscellaneous revenue
#'   (Part VIII, line 11d, column A)
#' - `F9_08_REV_OTH_FUNDR_NET_TOT`: Net income or (loss) from fundraising
#'   events (Part VIII, line 8c)
#' - `F9_08_REV_OTH_INVEST_BOND_TOT`: Income from investment of tax-exempt
#'   bond proceeds (Part VIII, line 4)
#' - `F9_08_REV_OTH_INVEST_INCOME_TOT`: Investment income (Part VIII, line 3;
#'   990-EZ Part I, line 4)
#' - `F9_08_REV_OTH_RENT_GRO_PERS`: Gross rents, personal property
#'   (Part VIII, line 6a, column ii)
#' - `F9_08_REV_OTH_ROY_TOT`: Royalties (Part VIII, line 5)
#' - `F9_08_REV_OTH_SALE_ASSET_OTH`: Gross amount from sales of assets other
#'   than inventory, non-securities (Part VIII, line 7a, column ii)
#' - `F9_08_REV_PROG_TOT_TOT`: Total program service revenue (Part VIII, line 2g)
#' - `F9_08_REV_TOT_TOT`: Total revenue (Part VIII, line 12, column A)
#'
#' ## Part IX - Expenses
#'
#' - `F9_09_EXP_DEPREC_TOT`: Depreciation, depletion, and amortization
#'   (Part IX, line 22, column A)
#' - `F9_09_EXP_TOT_FUNDR`: Fundraising expenses (Part IX, line 25, column D)
#' - `F9_09_EXP_TOT_MGMT`: Management and general expenses
#'   (Part IX, line 25, column C)
#' - `F9_09_EXP_TOT_PROG`: Program service expenses (Part IX, line 25, column B)
#' - `F9_09_EXP_TOT_TOT`: Total functional expenses (Part IX, line 25, column A)
#'
#' ## Part X - Balance Sheet
#'
#' - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY (Part X, line 4)
#' - `F9_10_ASSET_CASH_BOY`: Cash, non-interest-bearing, BOY (Part X, line 1)
#' - `F9_10_ASSET_CASH_EOY`: Cash, non-interest-bearing, EOY (Part X, line 1)
#' - `F9_10_ASSET_EXP_PREPAID_EOY`: Prepaid expenses and deferred charges,
#'   EOY (Part X, line 9)
#' - `F9_10_ASSET_INV_SALE_EOY`: Inventories for sale or use, EOY
#'   (Part X, line 8)
#' - `F9_10_ASSET_LAND_BLDG_DEPREC`: Accumulated depreciation on land,
#'   buildings, and equipment (Part X, line 10b)
#' - `F9_10_ASSET_LAND_BLDG_NET_EOY`: Land, buildings, and equipment, net
#'   of depreciation, EOY (Part X, line 10c)
#' - `F9_10_ASSET_PLEDGE_NET_EOY`: Pledges and grants receivable, net, EOY
#'   (Part X, line 3)
#' - `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY
#'   (Part X, line 2)
#' - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (Part X, line 16;
#'   990-EZ Part II, line 25)
#' - `F9_10_LIAB_ACC_PAYABLE_EOY`: Accounts payable and accrued expenses,
#'   EOY (Part X, line 17)
#' - `F9_10_LIAB_GRANT_PAYABLE_EOY`: Grants and similar amounts payable,
#'   EOY (Part X, line 18)
#' - `F9_10_LIAB_MTG_NOTE_EOY`: Secured mortgages and notes payable to
#'   unrelated third parties, EOY (Part X, line 23)
#' - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (Part X, line 26;
#'   990-EZ Part II, line 26)
#' - `F9_10_NAFB_TOT_BOY`: Total net assets or fund balances, BOY
#'   (Part X, line 32; 990-EZ Part II, line 27)
#' - `F9_10_NAFB_TOT_EOY`: Total net assets or fund balances, EOY
#'   (Part X, line 32; 990-EZ Part II, line 27)
#' - `F9_10_NAFB_UNRESTRICT_BOY`: Net assets without donor restrictions
#'   (formerly "unrestricted"), BOY (Part X, line 27)
#' - `F9_10_NAFB_UNRESTRICT_EOY`: Net assets without donor restrictions
#'   (formerly "unrestricted"), EOY (Part X, line 27)
#'
#' @details
#' The dataset was assembled by merging five tables from the NCCS efile
#' database (version 2.1, 2021 tax year): `F9-P00-T00-HEADER`,
#' `F9-P01-T00-SUMMARY`, `F9-P08-T00-REVENUE`, `F9-P09-T00-EXPENSES`, and
#' `F9-P10-T00-BALANCE-SHEET`. BMF fields (NTEE codes, geographic identifiers,
#' and organizational metadata) were merged on EIN from the NCCS Unified BMF
#' (v1.2). The sample was drawn from filings with an NTEE code and non-negative
#' total revenue (`F9_01_REV_TOT_CY >= 0`). Each row is a filing: four
#' organizations appear twice, once with an original and once with an amended
#' return.
#'
#' Most Part VIII, IX, and X fields exist only on the full Form 990 and are `NA`
#' for 990-EZ filers. The exceptions are fields that also appear on the 990-EZ
#' (total assets, total liabilities, net assets, membership dues, and investment
#' income), which are populated for both forms. Conversely, the Part I totals
#' for assets and liabilities (`F9_01_NAFB_ASSET_TOT_EOY`,
#' `F9_01_NAFB_LIAB_TOT_EOY`) are populated only for 990 filers; use
#' `F9_10_ASSET_TOT_EOY` and `F9_10_LIAB_TOT_EOY` for both. Use
#' [sanitize_financials()] to impute zero for genuine zero-value fields before
#' computing ratios.
#'
#' The efile tables are produced with the `irs990efile` R package. See
#' \url{https://github.com/Nonprofit-Open-Data-Collective/irs990efile} for details.
#' The build script is `data-raw/create-dat10k.R`.
#'
#' @source IRS 990 efile data (2021 tax year) via the Nonprofit Open Data Collective.
#'   \url{https://nonprofit-open-data-collective.github.io/irs990efile/}
#'
#' @examples
#' data( dat10k )
#' dim( dat10k )
#' table( dat10k$RETURN_TYPE )
#'
#' # Compute the government grant ratio for all organizations
#' d <- get_grants_govt_ratio( df = dat10k )
#' summary( d$grants_govt )
#'
#' # Sanitize first, then compute a batch of ratios
#' dat_clean <- sanitize_financials( dat10k )
#' dat_clean  <- get_grants_govt_ratio( dat_clean )
#' dat_clean  <- get_program_expenses_ratio( dat_clean )
#' dat_clean  <- get_debt_assets_ratio( dat_clean )
"dat10k"
