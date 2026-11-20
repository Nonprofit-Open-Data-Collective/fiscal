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
