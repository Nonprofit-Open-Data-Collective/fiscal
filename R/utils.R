###---------------------------------------------------
###   PACKAGE UTILITIES
###---------------------------------------------------

#' @importFrom magrittr "%>%"

# ---- Operator ----

`%notin%` <- Negate( `%in%` )


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


# ---- Field scope maps ----
# These vectors define which financial fields belong to which form scope.
# PC_FIELDS: only present on the full 990 (Part X balance sheet, Part VIII/IX detail).
# PZ_FIELDS: present on both 990 and 990EZ (Part I summary fields).
# Used by sanitize_financials() and the sanitize= argument in individual functions.

.PC_FIELDS <- c(
  # Part VIII - revenue detail (990 only)
  "F9_08_REV_CONTR_GOVT_GRANT", "F9_08_REV_CONTR_MEMBSHIP_DUE",
  "F9_08_REV_CONTR_TOT", "F9_08_REV_MISC_OTH_TOT",
  "F9_08_REV_OTH_FUNDR_NET_TOT", "F9_08_REV_OTH_INVEST_BOND_TOT",
  "F9_08_REV_OTH_INVEST_INCOME_TOT", "F9_08_REV_OTH_RENT_GRO_PERS",
  "F9_08_REV_OTH_ROY_TOT", "F9_08_REV_OTH_SALE_ASSET_OTH",
  "F9_08_REV_PROG_TOT_TOT", "F9_08_REV_TOT_TOT",
  # Part IX - expense detail (990 only)
  "F9_09_EXP_DEPREC_TOT", "F9_09_EXP_TOT_FUNDR",
  "F9_09_EXP_TOT_MGMT", "F9_09_EXP_TOT_PROG", "F9_09_EXP_TOT_TOT",
  # Part X - balance sheet (990 only)
  "F9_10_ASSET_ACC_NET_EOY", "F9_10_ASSET_CASH_BOY", "F9_10_ASSET_CASH_EOY",
  "F9_10_ASSET_EXP_PREPAID_EOY", "F9_10_ASSET_INV_SALE_EOY",
  "F9_10_ASSET_LAND_BLDG_DEPREC", "F9_10_ASSET_LAND_BLDG_NET_EOY",
  "F9_10_ASSET_PLEDGE_NET_EOY", "F9_10_ASSET_SAVING_EOY", "F9_10_ASSET_TOT_EOY",
  "F9_10_LIAB_ACC_PAYABLE_EOY", "F9_10_LIAB_GRANT_PAYABLE_EOY",
  "F9_10_LIAB_MTG_NOTE_EOY", "F9_10_LIAB_TOT_EOY",
  "F9_10_NAFB_TOT_BOY", "F9_10_NAFB_TOT_EOY",
  "F9_10_NAFB_UNRESTRICT_BOY", "F9_10_NAFB_UNRESTRICT_EOY"
)

.PZ_FIELDS <- c(
  # Part I - summary (both 990 and 990EZ)
  "F9_01_EXP_REV_LESS_EXP_CY", "F9_01_EXP_TOT_CY",
  "F9_01_NAFB_ASSET_TOT_EOY", "F9_01_NAFB_LIAB_TOT_EOY",
  "F9_01_NAFB_TOT_BOY", "F9_01_NAFB_TOT_EOY",
  "F9_01_NAFB_UNRESTRICT_EOY", "F9_01_REV_PROG_TOT_CY", "F9_01_REV_TOT_CY"
)

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

#' Detect 990-EZ filer rows in an efile dataset
#'
#' Returns a logical vector marking rows that belong to 990-EZ filers.
#' Uses the `RETURN_TYPE` column when present; otherwise infers filer
#' type from field availability (rows with Part I data but missing Part VIII
#' total revenue are treated as 990-EZ filers).
#'
#' @param df A `data.frame` containing efile data.
#' @return A logical vector of length `nrow(df)`.
#' @examples
#' data( dat10k )
#' table( detect_ez_rows( dat10k ) )
#' @export
detect_ez_rows <- function( df ) {

  # Best case: RETURN_TYPE column is present
  if ( "RETURN_TYPE" %in% colnames( df ) ) {
    return( df[["RETURN_TYPE"]] %in% c( "990EZ", "990EZ-SHORT" ) )
  }

  # Fallback: rows that have Part I revenue but lack Part VIII total revenue
  # are almost certainly EZ filers
  has_part1  <- "F9_01_REV_TOT_CY"  %in% colnames( df )
  has_part8  <- "F9_08_REV_TOT_TOT" %in% colnames( df )

  if ( has_part1 && has_part8 ) {
    return( !is.na( df[["F9_01_REV_TOT_CY"]] ) & is.na( df[["F9_08_REV_TOT_TOT"]] ) )
  }

  # Cannot determine - assume all are 990 filers (conservative: no EZ masking)
  return( rep( FALSE, nrow( df ) ) )
}


#' Impute zero for NA values in financial fields, respecting form scope
#'
#' For a given set of column names and a data frame, imputes zero for NA
#' values in financial fields while respecting whether a field is available
#' to 990-EZ filers. PC-scope fields (990 only) are imputed only for rows
#' that are not 990-EZ filers. PZ-scope fields (990 + 990-EZ) are imputed
#' for all rows. Rows where every financial variable is NA are left untouched.
#'
#' @param dat A `data.frame` (the working subset, not the full dataset).
#' @param vars Character vector of financial column names to consider.
#' @param ez_rows Logical vector (length `nrow(dat)`) marking 990-EZ rows,
#'   as returned by [detect_ez_rows()].
#' @return The modified `data.frame` with zeros imputed where appropriate.
#' @examples
#' data( dat10k )
#' ez <- detect_ez_rows( dat10k )
#' dat_imp <- impute_zero( dat10k, vars = get_pc_fields(), ez_rows = ez )
#' @export
impute_zero <- function( dat, vars, ez_rows ) {

  pc_vars <- intersect( vars, .PC_FIELDS )
  pz_vars <- intersect( vars, .PZ_FIELDS )

  # All-NA guard: if every financial variable in dat is NA for a row,
  # that row is a non-filer or a record with no financial data - do not impute.
  fin_vars_present <- intersect( c( pc_vars, pz_vars ), colnames( dat ) )
  if ( length( fin_vars_present ) > 0 ) {
    all_na_rows <- rowSums( !is.na( dplyr::select( dat, dplyr::any_of( fin_vars_present ) ) ) ) == 0
  } else {
    all_na_rows <- rep( FALSE, nrow( dat ) )
  }

  # PZ fields: impute zero for all NA rows, except all-NA rows
  for ( v in pz_vars ) {
    if ( v %in% colnames( dat ) ) {
      impute_rows <- is.na( dat[[v]] ) & !all_na_rows
      dat[ impute_rows, v ] <- 0
    }
  }

  # PC fields: impute zero only for non-EZ filers, except all-NA rows
  for ( v in pc_vars ) {
    if ( v %in% colnames( dat ) ) {
      impute_rows <- is.na( dat[[v]] ) & !ez_rows & !all_na_rows
      dat[ impute_rows, v ] <- 0
    }
  }

  return( dat )
}


#' Coerce selected columns to numeric
#'
#' Converts specified columns in a data frame to numeric, with a guard
#' against non-digit content. Stops if letters are detected in any column;
#' warns if silent coercion was needed.
#'
#' @param d A `data.frame`.
#' @param vars Character vector of column names to coerce.
#' @return The data frame with the specified columns coerced to numeric.
#' @export
coerce_numeric <- function( d, vars ) {

  vars_present <- intersect( vars, colnames( d ) )
  d_sub        <- dplyr::select( d, dplyr::any_of( vars_present ) )

  n_numeric <- sum( sapply( d_sub, is.numeric ) )

  has_letters <- sum(
    sapply( d_sub, function(x)
      sum( stringr::str_detect( x, "^([A-Za-z\\s]*)$" ), na.rm = TRUE )
    ), na.rm = TRUE
  )

  if ( has_letters != 0 ) {
    stop( "Non-digit characters detected in at least one column. Ensure all variables contain only numeric values before calling this function." )
  }

  if ( n_numeric < length( vars_present ) ) {
    n_coerced <- length( vars_present ) - n_numeric
    warning( paste0( n_coerced, " column(s) were not numeric and have been coerced." ) )
    d[ , vars_present ] <- lapply( d_sub, function(x) as.numeric( as.character(x) ) )
  }

  return( d )
}


#' Resolve one or two candidate column names to a single numeric vector
#'
#' Given one or two candidate column names and a data frame, returns a single
#' numeric vector. If both columns are present, they are coalesced with the
#' first (990 PC) taking priority and the second (990-EZ) filling in where
#' the first is `NA`.
#'
#' @param dat A `data.frame`.
#' @param cols Character vector of one or two column names.
#' @return A numeric vector of length `nrow(dat)`.
#' @export
resolve_col <- function( dat, cols ) {

  present <- cols[ cols %in% colnames( dat ) ]

  if ( length( present ) == 0 ) {
    stop( paste0( "None of the specified columns were found in the data: ",
                  paste( cols, collapse = ", " ) ) )
  }

  if ( length( present ) == 1 ) {
    return( dat[[ present ]] )
  }

  # Two columns present: coalesce - first column (990-PC) takes priority,
  # filling in from the second (990-EZ) only where the first is NA.
  out <- dat[[ present[1] ]]
  out[ is.na( out ) ] <- dat[[ present[2] ]][ is.na( out ) ]
  return( out )
}


#' Winsorize a numeric vector and return raw, winsorized, z-score, and percentile versions
#'
#' @param x Numeric vector.
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`,
#'   which clips at the 1st and 99th percentiles).
#' @return A named list with elements `raw`, `winsorized`, `z`, `pctile`.
#' @export
winsorize_var <- function( x, winsorize = 0.98 ) {

  top.p    <- 1 - ( 1 - winsorize ) / 2
  bottom.p <-     ( 1 - winsorize ) / 2

  top    <- quantile( x, top.p,    na.rm = TRUE )
  bottom <- quantile( x, bottom.p, na.rm = TRUE )

  x.w <- x
  x.w[ x.w > top    ] <- top
  x.w[ x.w < bottom ] <- bottom

  x.z <- as.numeric( scale( x.w ) )
  x.p <- dplyr::ntile( x, 100 )

  return( list( raw = x, winsorized = x.w, z = x.z, pctile = x.p ) )
}


#' Validate numerator and denominator inputs for get_* functions
#'
#' Checks that `winsorize` is in `[0, 1]` and that numerator and
#' denominator arguments are not in an inconsistent NULL state.
#'
#' @param winsorize Numeric scalar.
#' @param num_args Numerator argument value (may be `NULL`).
#' @param den_args Denominator argument value (may be `NULL`).
#' @param num_name Name of numerator argument used in error messages.
#' @param den_name Name of denominator argument used in error messages.
#' @return Invisibly `NULL`; called for its side-effect of stopping on invalid input.
#' @export
validate_inputs <- function( winsorize, num_args, den_args,
                              num_name = "numerator", den_name = "denominator" ) {

  if ( winsorize > 1 | winsorize < 0 ) {
    stop( "winsorize must be between 0 and 1." )
  }

  if ( is.null( num_args ) & !is.null( den_args ) ) {
    stop( paste0( "The ", num_name, " argument is NULL but ", den_name, " is specified. Please supply both arguments." ) )
  }

  if ( !is.null( num_args ) & is.null( den_args ) ) {
    stop( paste0( "The ", den_name, " argument is NULL but ", num_name, " is specified. Please supply both arguments." ) )
  }

  if ( is.null( num_args ) & is.null( den_args ) ) {
    stop( "Both numerator and denominator arguments are NULL. Please supply column names or use the default arguments." )
  }
}


#' Sanitize Financial Variables in an IRS 990 Dataset
#'
#' @description
#' Impute zero for NA values in IRS 990 financial fields, respecting the distinction
#' between fields available to all filers (990 + 990EZ) and fields that only appear on
#' the full 990 form.
#'
#' @param df A `data.frame` containing IRS 990 efile financial fields. Should
#'   contain a `RETURN_TYPE` column (values `"990"` or `"990EZ"`) for
#'   accurate filer-type detection. If absent, filer type is inferred from field
#'   availability.
#' @param pz_vars Character vector of column names for fields present on both 990 and
#'   990EZ forms (scope PZ). NA values in these columns are imputed to zero for all
#'   rows. Defaults to the full set of PZ financial fields used by this package.
#' @param pc_vars Character vector of column names for fields present only on the full
#'   990 form (scope PC). NA values are imputed to zero only for rows identified as
#'   990 filers; 990EZ filer rows are left as NA. Defaults to the full set of PC
#'   financial fields used by this package.
#'
#' @return A `data.frame` identical in structure to `df`, with NA values
#'   replaced by zero in the applicable financial columns. The original `df` is
#'   not modified.
#'
#' @details
#' In IRS 990 efile data, organizations often leave financial line items blank when the
#' value is zero rather than explicitly reporting zero. These blanks are typically
#' encoded as `NA` in processed datasets. For ratio calculations this is
#' problematic: a nonprofit with no investment income genuinely has a zero in that
#' field, not a missing value.
#'
#' `sanitize_financials()` corrects this by imputing zero for NA values in
#' financial fields, subject to an important constraint: fields that only exist on the
#' full 990 form (Part VIII, IX, and X) should not be imputed to zero for 990EZ
#' filers, since those fields simply don't exist on the EZ form. Fields from Part I
#' (the summary section) appear on both forms and can be safely imputed for all filers.
#'
#' Filer type is determined from the `RETURN_TYPE` column when present
#' (`"990"` = full filer, `"990EZ"` = short-form filer). If `RETURN_TYPE`
#' is absent, filer type is inferred: rows with Part I data but missing Part VIII data
#' are treated as 990EZ filers.
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' # Sanitize the full dataset before computing ratios
#' dat_clean <- sanitize_financials( dat10k )
#'
#' # All ratios can then be computed without worrying about NA/zero ambiguity
#' dat_clean <- get_grants_govt_ratio( dat_clean )
#' dat_clean <- get_program_expenses_ratio( dat_clean )
#'
#' @export
sanitize_financials <- function( df,
                                  pz_vars = .PZ_FIELDS,
                                  pc_vars = .PC_FIELDS ) {

  dat     <- df
  ez_rows <- detect_ez_rows( dat )

  all_vars <- union( pz_vars, pc_vars )
  dat <- impute_zero( dat, vars = all_vars, ez_rows = ez_rows )

  return( dat )
}
