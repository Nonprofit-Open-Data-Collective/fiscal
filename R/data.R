#' IRS 990 Efile Dataset - 2023 Tax Year (10,000 Organizations)
#'
#' A random sample of 10,000 nonprofit organizations drawn from the 2023 IRS 990
#' efile database, combining financial data from Form 990 Parts I, VIII, IX, and X
#' with organizational metadata from the IRS Business Master File (BMF). The
#' sample carries every efile field that the `get_*()` metric functions use by
#' default, so [compute_all()] runs the full battery offline.
#'
#' @format A data frame with 10,000 rows. The dataset includes both full 990
#'   filers and 990-EZ filers; the `RETURN_TYPE` column identifies which form each
#'   organization filed. Dollar amounts are whole dollars stored as plain doubles
#'   (never bit64 `integer64`). Variables are organized into the following groups:
#'
#' ## Organization identifiers
#'
#' - `EIN2`: Employer Identification Number (`"EIN-XX-XXXXXXX"`)
#' - `OBJECTID`: Efile record identifier
#' - `ORG_EIN`: Organization EIN (integer)
#' - `ORG_NAME_L1`: Organization name (line 1)
#' - `ORG_NAME_L2`: Organization name (line 2, if present)
#'
#' ## BMF classification and geography
#'
#' As returned by `retrieve_efile_data( include_bmf = TRUE )`.
#'
#' - `ntee_code_clean`: Normalized three-character NTEE code
#' - `ntee_code_major_group`: NTEE major group label
#' - `nteev2`: NTEE version 2 code, `[SUBSECTOR]-[NTEE]-[ORGTYPE]`
#' - `nteev2_subsector`: NTEE v2 subsector (e.g. `"HEL"`, `"EDU"`, `"UNU"`)
#' - `nteev2_org_type`: NTEE v2 organization type (e.g. `"RG"` for regular)
#' - `subsection_code`: IRS 501(c) subsection code
#' - `foundation_code`: IRS foundation type code
#' - `ruling_year`: Year of the IRS exemption ruling
#' - `filing_requirement_code`: IRS 990 filing requirement code
#' - `asset_amount`, `income_amount`, `revenue_amount`: Most recent asset,
#'   income, and revenue amounts reported to the BMF (stored as character)
#' - `geo_state_abbr`: Two-letter state abbreviation
#' - `geo_county`: County name
#' - `geo_metro_area`: Metropolitan area name (blank outside metro areas)
#' - `bmf_vintage_ym`: BMF release used for the merge (`"YYYY-MM"`)
#'
#' ## Return header fields (Part 00)
#'
#' - `RETURN_AMENDED_X`: Amended return indicator
#' - `RETURN_GROUP_X`: Group return indicator
#' - `RETURN_PARTIAL_X`: Partial return indicator
#' - `RETURN_TAXPER_DAYS`: Length of tax period in days
#' - `RETURN_TIME_STAMP`: Return timestamp
#' - `RETURN_TYPE`: Form type filed: `"990"` or `"990EZ"`
#' - `TAX_PERIOD_BEGIN_DATE`: Start of tax period
#' - `TAX_PERIOD_END_DATE`: End of tax period
#' - `TAX_YEAR`: Tax year (fiscal year start)
#' - `URL`: URL of the source XML filing on AWS
#' - `VERSION`: 990 form schema version
#' - `F9_00_EXEMPT_STAT_4947A1_X`: 4947(a)(1) trust indicator
#' - `F9_00_EXEMPT_STAT_501C_X`: 501(c) exempt status indicator
#' - `F9_00_EXEMPT_STAT_501C3_X`: 501(c)(3) indicator
#' - `F9_00_TYPE_ORG_ASSOC_X`: Association type indicator
#' - `F9_00_TYPE_ORG_CORP_X`: Corporation type indicator
#' - `F9_00_TYPE_ORG_OTH_X`: Other type indicator
#' - `F9_00_TYPE_ORG_TRUST_X`: Trust type indicator
#' - `F9_00_YEAR_FORMATION`: Year of formation
#'
#' ## Part I - Summary
#'
#' - `F9_01_EXP_REV_LESS_EXP_CY`:
#'   Revenues less expenses, current year (Part I, line 19)
#' - `F9_01_EXP_TOT_CY`: Total expenses, current year (Part I, line 18)
#' - `F9_01_NAFB_ASSET_TOT_EOY`: Total assets, EOY (Part I, line 20)
#' - `F9_01_NAFB_LIAB_TOT_EOY`: Total liabilities, EOY (Part I, line 21)
#' - `F9_01_NAFB_TOT_BOY`: Net assets or fund balances, BOY (Part I, line 22)
#' - `F9_01_NAFB_TOT_EOY`: Net assets or fund balances, EOY (Part I, line 22)
#' - `F9_01_REV_PROG_TOT_CY`: Program service revenue, current year (Part I, line 9)
#' - `F9_01_REV_TOT_CY`: Total revenue, current year (Part I, line 12)
#'
#' ## Part VIII - Revenue
#'
#' - `F9_08_REV_CONTR_FED_CAMP`: Federated campaigns (Part VIII, line 1a)
#' - `F9_08_REV_CONTR_MEMBSHIP_DUE`:
#'   Membership dues (Part VIII, line 1b)
#' - `F9_08_REV_CONTR_FUNDR_EVNT`: Fundraising events (Part VIII, line 1c)
#' - `F9_08_REV_CONTR_RLTD_ORG`: Related organizations (Part VIII, line 1d)
#' - `F9_08_REV_CONTR_GOVT_GRANT`:
#'   Government grants (Part VIII, line 1e)
#' - `F9_08_REV_CONTR_TOT`: Total contributions (Part VIII, line 1h)
#' - `F9_08_REV_PROG_TOT_TOT`: Program service revenue total (Part VIII, line 2g)
#' - `F9_08_REV_OTH_INVEST_INCOME_TOT`:
#'   Investment income (Part VIII, line 3)
#' - `F9_08_REV_OTH_INVEST_BOND_TOT`:
#'   Income from investment of tax-exempt bond proceeds (Part VIII, line 4)
#' - `F9_08_REV_OTH_ROY_TOT`: Royalties (Part VIII, line 5)
#' - `F9_08_REV_OTH_RENT_NET_TOT`: Net rental income (Part VIII, line 6d)
#' - `F9_08_REV_OTH_SALE_GAIN_NET_TOT`:
#'   Net gain or (loss) from sales of assets other than inventory (Part VIII, line 7d)
#' - `F9_08_REV_OTH_FUNDR_NET_TOT`:
#'   Net income from fundraising events (Part VIII, line 8c)
#' - `F9_08_REV_MISC_OTH_TOT`: Other miscellaneous revenue (Part VIII, line 11d)
#' - `F9_08_REV_TOT_TOT`: Total revenue (Part VIII, line 12A)
#'
#' ## Part IX - Expenses (column A, total, unless noted)
#'
#' - `F9_09_EXP_GRANT_US_ORG_TOT`: Grants to domestic organizations and governments (Part IX, line 1)
#' - `F9_09_EXP_GRANT_US_INDIV_TOT`: Grants and assistance to domestic individuals (Part IX, line 2)
#' - `F9_09_EXP_GRANT_FRGN_TOT`: Grants to foreign organizations, governments, and individuals (Part IX, line 3)
#' - `F9_09_EXP_BEN_PAID_MEMB_TOT`: Benefits paid to or for members (Part IX, line 4)
#' - `F9_09_EXP_COMP_DTK_TOT`: Compensation of current officers, directors, trustees, and key employees (Part IX, line 5)
#' - `F9_09_EXP_COMP_DSQ_PERS_TOT`: Compensation of disqualified persons (Part IX, line 6)
#' - `F9_09_EXP_OTH_SAL_WAGE_TOT`: Other salaries and wages (Part IX, line 7)
#' - `F9_09_EXP_PENSION_CONTR_TOT`: Pension plan accruals and contributions (Part IX, line 8)
#' - `F9_09_EXP_OTH_EMPL_BEN_TOT`: Other employee benefits (Part IX, line 9)
#' - `F9_09_EXP_PAYROLL_TAX_TOT`: Payroll taxes (Part IX, line 10)
#' - `F9_09_EXP_FEE_SVC_MGMT_TOT`: Fees for services - management (Part IX, line 11a)
#' - `F9_09_EXP_FEE_SVC_LEGAL_TOT`: Fees for services - legal (Part IX, line 11b)
#' - `F9_09_EXP_FEE_SVC_ACC_TOT`: Fees for services - accounting (Part IX, line 11c)
#' - `F9_09_EXP_FEE_SVC_LOB_TOT`: Fees for services - lobbying (Part IX, line 11d)
#' - `F9_09_EXP_FEE_SVC_FUNDR_TOT`: Professional fundraising services (Part IX, line 11e)
#' - `F9_09_EXP_FEE_SVC_INVEST_TOT`: Investment management fees (Part IX, line 11f)
#' - `F9_09_EXP_FEE_SVC_OTH_TOT`: Fees for services - other (Part IX, line 11g)
#' - `F9_09_EXP_PAY_AFFIL_TOT`: Payments to affiliates (Part IX, line 21)
#' - `F9_09_EXP_DEPREC_TOT`: Depreciation, depletion, and amortization (Part IX, line 22A)
#' - `F9_09_EXP_TOT_FUNDR`: Fundraising expenses (Part IX, line 25D)
#' - `F9_09_EXP_TOT_MGMT`: Management and general expenses (Part IX, line 25C)
#' - `F9_09_EXP_TOT_PROG`: Program service expenses (Part IX, line 25B)
#' - `F9_09_EXP_TOT_TOT`: Total functional expenses (Part IX, line 25A)
#'
#' ## Part X - Balance Sheet
#'
#' - `F9_10_ASSET_CASH_BOY`: Cash, beginning of year (Part X, line 1A)
#' - `F9_10_ASSET_CASH_EOY`: Cash, end of year (Part X, line 1B)
#' - `F9_10_ASSET_SAVING_EOY`: Savings and temporary cash investments, EOY (Part X, line 2B)
#' - `F9_10_ASSET_PLEDGE_NET_EOY`:
#'   Net pledges and grants receivable, EOY (Part X, line 3B)
#' - `F9_10_ASSET_ACC_NET_EOY`: Accounts receivable, net, EOY (Part X, line 4B)
#' - `F9_10_ASSET_INV_SALE_EOY`: Inventories for sale or use, EOY (Part X, line 8B)
#' - `F9_10_ASSET_EXP_PREPAID_EOY`:
#'   Prepaid expenses and deferred charges, EOY (Part X, line 9B)
#' - `F9_10_ASSET_LAND_BLDG`:
#'   Land, buildings, and equipment, cost or other basis (Part X, line 10a)
#' - `F9_10_ASSET_LAND_BLDG_NET_EOY`:
#'   Net land, buildings, and equipment, EOY (Part X, line 10C)
#' - `F9_10_ASSET_INVEST_SEC_EOY`:
#'   Investments - publicly traded securities, EOY (Part X, line 11B)
#' - `F9_10_ASSET_INVEST_SEC_OTH_EOY`:
#'   Investments - other securities, EOY (Part X, line 12B)
#' - `F9_10_ASSET_TOT_EOY`: Total assets, EOY (Part X, line 16B)
#' - `F9_10_LIAB_ACC_PAYABLE_EOY`:
#'   Accounts payable and accrued expenses, EOY (Part X, line 17B)
#' - `F9_10_LIAB_GRANT_PAYABLE_EOY`:
#'   Grants and similar amounts payable, EOY (Part X, line 18B)
#' - `F9_10_LIAB_MTG_NOTE_EOY`: Secured mortgages and notes payable, EOY (Part X, line 23B)
#' - `F9_10_LIAB_NOTE_UNSEC_EOY`: Unsecured notes and loans payable, EOY (Part X, line 24B)
#' - `F9_10_LIAB_TOT_EOY`: Total liabilities, EOY (Part X, line 26B)
#' - `F9_10_NAFB_FOLLOW_SFAS117_X`:
#'   Organization follows SFAS 117 / ASC 958 (Part X, check box above line 27)
#' - `F9_10_NAFB_NO_FOLLOW_SFAS117_X`:
#'   Organization does not follow SFAS 117 / ASC 958 (Part X, check box above line 30)
#' - `F9_10_NAFB_UNRESTRICT_BOY`:
#'   Net assets without donor restrictions, beginning of year (Part X, line 27A)
#' - `F9_10_NAFB_UNRESTRICT_EOY`:
#'   Net assets without donor restrictions, end of year (Part X, line 27B)
#' - `F9_10_NAFB_RESTRICT_EOY`:
#'   Net assets with donor restrictions, end of year (Part X, line 28B)
#' - `F9_10_NAFB_CAP_STCK_EOY`:
#'   Capital stock or trust principal, or current funds, EOY (Part X, line 30B)
#' - `F9_10_NAFB_CAP_SURPLUS_EOY`:
#'   Paid-in or capital surplus, EOY (Part X, line 31B)
#' - `F9_10_NAFB_EARNING_RETAINED_EOY`:
#'   Retained earnings, endowment, accumulated income, or other funds, EOY (Part X, line 32B)
#' - `F9_10_NAFB_TOT_BOY`: Total net assets, beginning of year (Part X, line 33A)
#' - `F9_10_NAFB_TOT_EOY`: Total net assets, end of year (Part X, line 33B)
#'
#' @details
#' The dataset was built from the 2023 tax-year efile tables
#' (`F9-P00-T00-HEADER`, `F9-P01-T00-SUMMARY`, `F9-P08-T00-REVENUE`,
#' `F9-P09-T00-EXPENSES`, and `F9-P10-T00-BALANCE-SHEET`) with BMF fields
#' (NTEE codes, geographic identifiers, and organizational metadata) merged on
#' EIN, using [retrieve_efile_data()]. Filings were deduplicated to one per
#' organization with [deduplicate()] before 10,000 organizations were sampled at
#' random from filings with non-negative total revenue and a usable NTEE code. The
#' BMF marks organizations without one as `"UNDEFINED"`, `"INVALID"`, or `"Z99"`;
#' those are excluded, so every row has a valid `ntee_code_clean` and no
#' `nteev2_subsector` is `"UNU"`. The sample includes a mix of full 990 and 990EZ
#' filers; see `RETURN_TYPE` to distinguish them.
#'
#' Most Part VIII, IX, and X detail exists only on the full 990 (PC scope; see
#' [get_pc_fields()]), so 990EZ filers have `NA` in those columns. Part I summary
#' fields (`F9_01_*`) and the lines that have a 990EZ counterpart, such as total
#' assets and total net assets (PZ scope; see [get_pz_fields()]), are populated
#' for both forms. Use [sanitize_financials()] to impute zero for genuine
#' zero-value fields before computing ratios.
#'
#' @source IRS 990 efile data (2023 tax year) via the Nonprofit Open Data Collective.
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
