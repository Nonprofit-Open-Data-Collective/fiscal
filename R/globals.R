# Panel classification vocabulary (mirrors panel990): boundary type and the
# independent spell dimension.
.PANEL_TYPES <- c("persistent", "entrant", "exit", "transient", "empty")
.PANEL_SPELL_BALANCE <- c("seamless", "segmented")

# ---- ID variable list ----
# Columns that identify a filing record across all efile tables.
# Used by every get_*() function to retain record linkage variables
# in the working subset (dt) alongside the financial fields (vars).

.IDVARS <- c(
  "EIN2", "OBJECTID", "ORG_EIN", "ORG_NAME_L1",
  "ORG_NAME_L2", "RETURN_AMENDED_X", "RETURN_GROUP_X",
  "RETURN_PARTIAL_X", "RETURN_TAXPER_DAYS", "RETURN_TIME_STAMP",
  "RETURN_TYPE", "TAX_PERIOD_BEGIN_DATE", "TAX_PERIOD_END_DATE",
  "TAX_YEAR", "URL", "VERSION"
)

#' Return the list of efile record identifier variables
#'
#' Returns the character vector of column names that identify a filing record
#' across all efile tables. These are retained alongside financial fields in
#' the working subset inside every `get_*()` function.
#'
#' @return A character vector of column names.
#' @examples
#' get_idvars()
#' @export
get_idvars <- function() .IDVARS


.BMF_URL <- panel990::bmf_url()
.BMF_VARS <- panel990::bmf_vars()

#' Return the list of default BMF variables
#'
#' Returns the character vector of BMF-derived and BMF-retained variables
#' appended by default when `retrieve_efile_data()` is called with
#' `include_bmf = TRUE`.
#'
#' These include native normalized NTEE classifications, BMF administrative
#' fields, recent financial amounts, geocoded location and quality fields, and
#' source/vintage provenance from the current BMF master.
#'
#' @return A character vector of column names.
#' @examples
#' get_bmf_vars()
#' @export
get_bmf_vars <- function() .BMF_VARS

# ---- Field scope maps ----
# Financial-field scope is sourced from panel990's concordance rather than a
# hardcoded list, so fiscal and panel990 share a single source of truth. The
# scope semantics used by sanitize_financials() are unchanged:
#   PC_FIELDS: present only on the full 990 (zeroed for non-990EZ filers).
#   PZ_FIELDS: present on both 990 and 990EZ (zeroed for all filers).
# The "core" set restricts to the primary financial statements (Part I and
# Parts VIII-XI), where a blank on the filed form unambiguously means zero.
.PC_FIELDS <- panel990::financial_fields("core", scope = "PC")
.PZ_FIELDS <- panel990::financial_fields("core", scope = "PZ")

#' Return the vector of 990-only financial field names (PC scope)
#'
#' Returns financial fields that appear only on the full Form 990
#' (Parts VIII, IX, and X). These fields are not present on the 990-EZ.
#' Used by [sanitize_financials()] to restrict zero-imputation to
#' full 990 filers.
#'
#' @return A character vector of column names.
#' @examples
#' get_pc_fields()
#' @export
get_pc_fields <- function() .PC_FIELDS

#' Return the vector of 990 + 990-EZ financial field names (PZ scope)
#'
#' Returns financial fields that appear on both the full Form 990 and the
#' 990-EZ (Part I summary fields). Zero-imputation via
#' [sanitize_financials()] is applied to these fields for all filers.
#'
#' @return A character vector of column names.
#' @examples
#' get_pz_fields()
#' @export
get_pz_fields <- function() .PZ_FIELDS


# -- Full validated table name registry ---------------------------------------
# Source: NODC irs-efile-master-concordance-file/concordance.csv
# T00 = one-to-one | T01/T02 = one-to-many | T99 = supplemental text

.VALID_TABLES <- c(
  # Form 990 / 990EZ core
  "F9-P00-T00-HEADER",
  "F9-P01-T00-SUMMARY",
  "F9-P01-T00-SUMMARY-EZ",
  "F9-P02-T00-SIGNATURE",
  "F9-P03-T00-MISSION",
  "F9-P03-T00-PROGRAM-ONE",
  "F9-P03-T00-PROGRAM-THREE",
  "F9-P03-T00-PROGRAM-TWO",
  "F9-P03-T00-PROGRAMS",
  "F9-P03-T01-PROGRAMS-OTHER",
  "F9-P03-T02-PROGRAMS-EZ",
  "F9-P04-T00-REQUIRED-SCHEDULES",
  "F9-P04-T00-REQUIRED-SCHEDULES-EZ",
  "F9-P05-T00-OTHER-IRS-FILING",
  "F9-P06-T00-GOVERNANCE",
  "F9-P06-T00-GOVERNANCE-EZ",
  "F9-P07-T00-DIR-TRUST-KEY",
  "F9-P07-T01-COMPENSATION",
  "F9-P07-T01-COMPENSATION-HCE-EZ",
  "F9-P07-T02-CONTRACTORS",
  "F9-P08-T00-REVENUE",
  "F9-P08-T01-REVENUE-PROGRAMS",
  "F9-P08-T02-REVENUE-MISC",
  "F9-P09-T00-EXPENSES",
  "F9-P09-T01-EXPENSES-OTHER",
  "F9-P10-T00-BALANCE-SHEET",
  "F9-P11-T00-ASSETS",
  "F9-P12-T00-FINANCIAL-REPORTING",
  # Schedule A
  "SA-P00-T00-HEADER",
  "SA-P01-T00-PUBLIC-CHARITY-STATUS",
  "SA-P01-T01-PUBLIC-CHARITY-STATUS",
  "SA-P02-T00-SUPPORT_SCHEDULE_170",
  "SA-P03-T00-SUPPORT_SCHEDULE_509",
  "SA-P04-T00-SUPPORT-ORGS",
  "SA-P05-T00-SUPPORT-ORGS",
  "SA-P06-T99-SUPPLEMENTAL-INFO",
  # Schedule B
  "SB-P01-T01-CONTRIBUTORS",
  # Schedule C
  "SC-P01-T00-LOBBY",
  "SC-P01-T01-POLITICAL-ORGS-INFO",
  "SC-P02-T00-LOBBY",
  "SC-P03-T00-LOBBY",
  "SC-P04-T99-SUPPLEMENTAL-INFO",
  # Schedule D
  "SD-P01-T00-ORGS-DONOR-ADVISED-FUNDS-OTH",
  "SD-P02-T00-CONSERV-EASEMENTS",
  "SD-P03-T00-ORGS-COLLECT-ART-HIST-TREASURE-OTH",
  "SD-P04-T00-ESCROW-CUSTODIAL-ARRANGEMENTS",
  "SD-P05-T00-ENDOWMENT",
  "SD-P06-T00-LAND-BLDG-EQUIP",
  "SD-P07-T00-INVESTMENTS-SECURITIES",
  "SD-P07-T01-INVESTMENTS-OTH-DERIVATIVES",
  "SD-P07-T01-INVESTMENTS-OTH-EQUITY",
  "SD-P07-T01-INVESTMENTS-OTH-SECURITIES",
  "SD-P08-T00-INVESTMENTS-PROG-RLTD",
  "SD-P08-T01-INVESTMENTS-PROG-RLTD",
  "SD-P09-T00-OTH-ASSETS",
  "SD-P09-T01-OTH-ASSETS",
  "SD-P10-T00-OTH-LIABILITIES",
  "SD-P10-T01-OTH-LIABILITIES",
  "SD-P11-T00-RECONCILIATION-REVENUE",
  "SD-P12-T00-RECONCILIATION-EXPENSES",
  "SD-P13-T99-SUPPLEMENTAL-INFO",
  "SD-P99-T00-RECONCILIATION-NETASSETS",
  # Schedule E
  "SE-P01-T00-SCHOOLS",
  "SE-P02-T99-SUPPLEMENTAL-INFO",
  # Schedule F
  "SF-P01-T00-FRGN-ACTS",
  "SF-P01-T01-FRGN-ACTS-BY-REGION",
  "SF-P02-T00-FRGN-ORG-GRANTS",
  "SF-P02-T01-FRGN-ORG-GRANTS",
  "SF-P03-T01-FRGN-INDIV-GRANTS",
  "SF-P04-T00-FRGN-INTERESTS",
  "SF-P05-T99-EXPLANATION-TEXT",
  "SF-P99-T00-FRGN-ORG-GRANTS",
  # Schedule G
  "SG-P01-T00-FUNDRAISING-ACTS",
  "SG-P01-T01-FUNDRAISERS-INFO",
  "SG-P02-T00-FUNDRAISING-EVENTS",
  "SG-P02-T01-FUNDRAISING-EVENTS",
  "SG-P03-T00-GAMING",
  "SG-P04-T99-SUPPLEMENTAL-INFO",
  # Schedule H
  "SH-P01-T00-FAP-COMMUNITY-BENEFIT-POLICY",
  "SH-P02-T00-FAP-COMMUNITY-BENEFIT-POLICY",
  "SH-P03-T00-FAP-COMMUNITY-BENEFIT-POLICY",
  "SH-P04-T01-COMPANY-JOINT-VENTURES",
  "SH-P05-T00-FAP-COMMUNITY-BENEFIT-POLICY",
  "SH-P05-T01-HOSPITAL-FACILITY",
  "SH-P05-T02-NON-HOSPITAL-FACILITY",
  "SH-P05-T99-SUPPLEMENTAL-INFO",
  "SH-P06-T99-SUPPLEMENTAL-INFO",
  "SH-P99-T00-FAP-COMMUNITY-BENEFIT-POLICY",
  # Schedule I
  "SI-P01-T00-GRANTS-INFO",
  "SI-P02-T00-GRANTS-US-ORGS-GOVTS",
  "SI-P02-T01-GRANTS-US-ORGS-GOVTS",
  "SI-P03-T01-GRANTS-US-INDIV",
  "SI-P04-T99-SUPPLEMENTAL-INFO",
  "SI-P99-T00-GRANTS-US-ORGS-GOVTS",
  # Schedule J
  "SJ-P01-T00-COMPENSATION",
  "SJ-P02-T01-COMPENSATION-DTK",
  "SJ-P03-T99-SUPPLEMENTAL-INFO",
  # Schedule K
  "SK-P01-T01-BOND-ISSUES",
  "SK-P02-T01-BOND-PROCEEDS",
  "SK-P03-T01-BOND-PRIVATE-BIZ-USE",
  "SK-P04-T01-BOND-ARBITRAGE",
  "SK-P05-T01-PROCEDURE-CORRECTIVE-ACT",
  "SK-P06-T99-SUPPLEMENTAL-INFO",
  # Schedule L
  "SL-P01-T00-EXCESS-BENEFIT-TRANSAC",
  "SL-P01-T01-EXCESS-BENEFIT-TRANSAC",
  "SL-P02-T00-LOANS-INTERESTED-PERS",
  "SL-P02-T01-LOANS-INTERESTED-PERS",
  "SL-P03-T01-GRANTS-INTERESTED-PERS",
  "SL-P04-T01-BIZ-TRANSAC-INTERESTED-PERS",
  "SL-P05-T99-SUPPLEMENTAL-INFO",
  # Schedule M
  "SM-P01-T00-NONCASH-CONTRIBUTIONS",
  "SM-P01-T01-NONCASH-CONTRIBUTIONS",
  "SM-P02-T99-SUPPLEMENTAL-INFO",
  # Schedule N
  "SN-P01-T00-LIQUIDATION-TERMINATION-DISSOLUTION",
  "SN-P01-T01-LIQUIDATION-TERMINATION-DISSOLUTION",
  "SN-P02-T00-DISPOSITION-OF-ASSETS",
  "SN-P02-T01-DISPOSITION-OF-ASSETS",
  "SN-P03-T99-SUPPLEMENTAL-INFO",
  "SN-P99-T00-LIQUIDATION-TERMINATION-DISSOLUTION",
  # Schedule O
  "SO-T99-SUPPLEMENTAL-INFO",
  # Schedule R
  "SR-P01-T01-ID-DISREGARDED-ENTITIES",
  "SR-P02-T01-ID-RLTD-TAX-EXEMPED-ORGS",
  "SR-P03-T01-ID-RLTD-ORGS-TAXABLE-PARTNERSHIP",
  "SR-P04-T01-ID-RLTD-ORGS-TAXABLE-CORPORATION",
  "SR-P05-T00-TRANSACTIONS-RLTD-ORGS",
  "SR-P05-T01-TRANSACTIONS-RLTD-ORGS",
  "SR-P06-T01-UNRLTD-ORGS-TAXABLE-PARTNERSHIP",
  "SR-P07-T99-SUPPLEMENTAL-INFO"
)

# Classify by T-number
#' @keywords internal
#' @noRd
.table_tnum <- function(name) {
  m <- regmatches(name, regexpr("-T([0-9]+)(-|$)", name))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(gsub("-T([0-9]+)(-|$)", "\\1", m))
}

.TABLES_1X1  <- .VALID_TABLES[sapply(.VALID_TABLES, .table_tnum) == 0L]
.TABLES_1XM  <- .VALID_TABLES[sapply(.VALID_TABLES, .table_tnum) %in% 1:98]
.TABLES_SUPP <- .VALID_TABLES[sapply(.VALID_TABLES, .table_tnum) == 99L]
