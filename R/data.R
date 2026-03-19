#' IRS 990 Efile Dataset — 2021 Tax Year (10,000 Organizations)
#'
#' A dataset of 10,000 nonprofit organizations drawn from the 2021 IRS 990 efile
#' database, combining financial data from Form 990 Parts I, VIII, IX, and X with
#' organizational metadata from the IRS Business Master File (BMF).
#'
#' @format A data frame with 10,000 rows and 98 variables. The dataset includes
#'   both full 990 filers and 990-EZ filers; the \code{RETURN_TYPE} column
#'   identifies which form each organization filed. Variables are organized into
#'   the following groups:
#'
#'   \strong{BMF and organizational identifiers:}
#'   \describe{
#'     \item{EIN2}{Employer Identification Number (9-digit character)}
#'     \item{NTEE_NCCS}{NTEE activity code assigned by NCCS}
#'     \item{NTEEV2}{NTEE Version 2 code}
#'     \item{NTMAJ12}{NTEE major group (12-category classification)}
#'     \item{NTEE_ORG_TYPE}{Broad organization type from NTEE}
#'     \item{CENSUS_CBSA_FIPS}{Core-Based Statistical Area FIPS code}
#'     \item{CENSUS_CBSA_NAME}{CBSA name (metro/micro area)}
#'     \item{CENSUS_BLOCK_FIPS}{Census block FIPS code}
#'     \item{CENSUS_URBAN_AREA}{Urban area classification}
#'     \item{CENSUS_STATE_ABBR}{Two-letter state abbreviation}
#'     \item{CENSUS_COUNTY_NAME}{County name}
#'     \item{BMF_SUBSECTION_CODE}{IRS 501(c) subsection code}
#'     \item{BMF_FOUNDATION_CODE}{IRS foundation type code}
#'     \item{ORG_RULING_YEAR}{Year the organization received tax-exempt status}
#'     \item{F990_TOTAL_REVENUE_RECENT}{Most recent total revenue from BMF}
#'     \item{F990_TOTAL_INCOME_RECENT}{Most recent total income from BMF}
#'     \item{F990_TOTAL_ASSETS_RECENT}{Most recent total assets from BMF}
#'     \item{F990_TOTAL_EXPENSES_RECENT}{Most recent total expenses from BMF}
#'     \item{OBJECTID}{Internal record identifier}
#'     \item{ORG_EIN}{Organization EIN}
#'     \item{ORG_NAME_L1}{Organization name (line 1)}
#'     \item{ORG_NAME_L2}{Organization name (line 2, if present)}
#'   }
#'
#'   \strong{Return header fields (Part 00):}
#'   \describe{
#'     \item{RETURN_AMENDED_X}{Amended return indicator}
#'     \item{RETURN_GROUP_X}{Group return indicator}
#'     \item{RETURN_PARTIAL_X}{Partial return indicator}
#'     \item{RETURN_TAXPER_DAYS}{Length of tax period in days}
#'     \item{RETURN_TIME_STAMP}{Return timestamp}
#'     \item{RETURN_TYPE}{Form type filed: \code{"990"} or \code{"990EZ"}}
#'     \item{TAX_PERIOD_BEGIN_DATE}{Start of tax period}
#'     \item{TAX_PERIOD_END_DATE}{End of tax period}
#'     \item{TAX_YEAR}{Tax year (fiscal year start)}
#'     \item{URL}{URL of the source XML filing on AWS}
#'     \item{VERSION}{990 form schema version}
#'     \item{F9_00_BUILD_TIME_STAMP}{IRS build timestamp}
#'     \item{F9_00_NAME_ORG_CTRL}{Organization name control text}
#'     \item{F9_00_RETURN_TIME_STAMP}{Return creation timestamp}
#'     \item{F9_00_RETURN_TYPE}{Return type from XML header}
#'     \item{F9_00_TAX_PERIOD_BEGIN_DATE}{Tax period start date}
#'     \item{F9_00_TAX_PERIOD_END_DATE}{Tax period end date}
#'     \item{F9_00_TAX_YEAR}{Tax year}
#'     \item{F9_00_RETURN_GROUP_X}{Group return flag}
#'     \item{F9_00_EXEMPT_STAT_4947A1_X}{4947(a)(1) trust indicator}
#'     \item{F9_00_EXEMPT_STAT_501C_X}{501(c) exempt status indicator}
#'     \item{F9_00_EXEMPT_STAT_501C3_X}{501(c)(3) indicator}
#'     \item{F9_00_TYPE_ORG_OTH_DESC}{Other organization type description}
#'     \item{F9_00_TYPE_ORG_ASSOC_X}{Association type indicator}
#'     \item{F9_00_TYPE_ORG_CORP_X}{Corporation type indicator}
#'     \item{F9_00_TYPE_ORG_OTH_X}{Other type indicator}
#'     \item{F9_00_TYPE_ORG_TRUST_X}{Trust type indicator}
#'     \item{F9_00_YEAR_FORMATION}{Year of formation}
#'   }
#'
#'   \strong{Part I — Summary (scope: 990 + 990EZ):}
#'   \describe{
#'     \item{F9_01_EXP_REV_LESS_EXP_CY}{Revenues less expenses, current year (Part I, line 19)}
#'     \item{F9_01_EXP_TOT_CY}{Total expenses, current year (Part I, line 17)}
#'     \item{F9_01_NAFB_ASSET_TOT_EOY}{Total assets, EOY (Part I)}
#'     \item{F9_01_NAFB_LIAB_TOT_EOY}{Total liabilities, EOY (Part I)}
#'     \item{F9_01_NAFB_TOT_BOY}{Net assets or fund balances, BOY (Part I, line 22)}
#'     \item{F9_01_NAFB_TOT_EOY}{Net assets or fund balances, EOY (Part I, line 22)}
#'     \item{F9_01_REV_PROG_TOT_CY}{Program service revenue, current year (Part I)}
#'     \item{F9_01_REV_TOT_CY}{Total revenue, current year (Part I, line 12)}
#'   }
#'
#'   \strong{Part VIII — Revenue (scope: 990 only):}
#'   \describe{
#'     \item{F9_08_REV_CONTR_GOVT_GRANT}{Government grants (Part VIII, line 1e)}
#'     \item{F9_08_REV_CONTR_MEMBSHIP_DUE}{Membership dues (Part VIII, line 1b)}
#'     \item{F9_08_REV_CONTR_TOT}{Total contributions (Part VIII, line 1h)}
#'     \item{F9_08_REV_MISC_OTH_TOT}{Other miscellaneous revenue (Part VIII, line 11e)}
#'     \item{F9_08_REV_OTH_FUNDR_NET_TOT}{Net fundraising income (Part VIII, line 8c)}
#'     \item{F9_08_REV_OTH_INVEST_BOND_TOT}{Bond investment income (Part VIII, line 4)}
#'     \item{F9_08_REV_OTH_INVEST_INCOME_TOT}{Investment income (Part VIII, line 3)}
#'     \item{F9_08_REV_OTH_RENT_GRO_PERS}{Gross rents — personal property (Part VIII, line 6a)}
#'     \item{F9_08_REV_OTH_ROY_TOT}{Royalties (Part VIII, line 5)}
#'     \item{F9_08_REV_OTH_SALE_ASSET_OTH}{Net gain from asset sales (Part VIII, line 7d)}
#'     \item{F9_08_REV_PROG_TOT_TOT}{Program service revenue total (Part VIII, line 2g)}
#'     \item{F9_08_REV_TOT_TOT}{Total revenue (Part VIII, line 12A)}
#'   }
#'
#'   \strong{Part IX — Expenses (scope: 990 only):}
#'   \describe{
#'     \item{F9_09_EXP_DEPREC_TOT}{Depreciation, depletion, and amortization (Part IX, line 22A)}
#'     \item{F9_09_EXP_TOT_FUNDR}{Fundraising expenses (Part IX, line 25D)}
#'     \item{F9_09_EXP_TOT_MGMT}{Management and general expenses (Part IX, line 25C)}
#'     \item{F9_09_EXP_TOT_PROG}{Program service expenses (Part IX, line 25B)}
#'     \item{F9_09_EXP_TOT_TOT}{Total functional expenses (Part IX, line 25A)}
#'   }
#'
#'   \strong{Part X — Balance Sheet (scope: 990 only):}
#'   \describe{
#'     \item{F9_10_ASSET_ACC_NET_EOY}{Accounts receivable, net, EOY (Part X, line 4B)}
#'     \item{F9_10_ASSET_CASH_BOY}{Cash, beginning of year (Part X, line 1A)}
#'     \item{F9_10_ASSET_CASH_EOY}{Cash, end of year (Part X, line 1B)}
#'     \item{F9_10_ASSET_EXP_PREPAID_EOY}{Prepaid expenses and deferred charges, EOY (Part X, line 9B)}
#'     \item{F9_10_ASSET_INV_SALE_EOY}{Investments held for sale, EOY (Part X, line 8B)}
#'     \item{F9_10_ASSET_LAND_BLDG_DEPREC}{Net land, buildings, and equipment — depreciated (Part X, line 10C)}
#'     \item{F9_10_ASSET_LAND_BLDG_NET_EOY}{Net land, buildings, and equipment, EOY (Part X, line 10C)}
#'     \item{F9_10_ASSET_PLEDGE_NET_EOY}{Net pledges and grants receivable, EOY (Part X, line 3B)}
#'     \item{F9_10_ASSET_SAVING_EOY}{Savings and temporary cash investments, EOY (Part X, line 2B)}
#'     \item{F9_10_ASSET_TOT_EOY}{Total assets, EOY (Part X, line 16B)}
#'     \item{F9_10_LIAB_ACC_PAYABLE_EOY}{Accounts payable and accrued expenses, EOY (Part X, line 17B)}
#'     \item{F9_10_LIAB_GRANT_PAYABLE_EOY}{Grants and similar amounts payable, EOY (Part X, line 18B)}
#'     \item{F9_10_LIAB_MTG_NOTE_EOY}{Mortgages and notes payable, EOY (Part X, line 23B)}
#'     \item{F9_10_LIAB_TOT_EOY}{Total liabilities, EOY (Part X, line 26B)}
#'     \item{F9_10_NAFB_TOT_BOY}{Total net assets, beginning of year (Part X, line 33A)}
#'     \item{F9_10_NAFB_TOT_EOY}{Total net assets, end of year (Part X, line 33B)}
#'     \item{F9_10_NAFB_UNRESTRICT_BOY}{Unrestricted net assets, beginning of year (Part X, line 27A)}
#'     \item{F9_10_NAFB_UNRESTRICT_EOY}{Unrestricted net assets, end of year (Part X, line 27B)}
#'   }
#'
#' @details
#' The dataset was assembled by merging five efile tables from the 2021 tax year:
#' \code{F9-P00-T00-HEADER}, \code{F9-P01-T00-SUMMARY}, \code{F9-P08-T00-REVENUE},
#' \code{F9-P09-T00-EXPENSES}, and \code{F9-P10-T00-BALANCE-SHEET}. BMF fields
#' (NTEE codes, geographic identifiers, and organizational metadata) were merged on
#' EIN. The sample includes a mix of full 990 and 990EZ filers; see \code{RETURN_TYPE}
#' to distinguish them.
#'
#' Financial fields from Part VIII, IX, and X are only populated for full 990 filers.
#' 990EZ filers will have \code{NA} in those columns but will have values in the
#' Part I summary fields (\code{F9_01_*}). Use \code{\link{sanitize_financials}} to
#' impute zero for genuine zero-value fields before computing ratios.
#'
#' The dataset is built using the \code{irs990efile} R package. See
#' \url{https://github.com/Nonprofit-Open-Data-Collective/irs990efile} for details.
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
