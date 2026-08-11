###---------------------------------------------------
###   PACKAGE UTILITIES
###---------------------------------------------------

#' @importFrom magrittr "%>%"

# ---- Operator ----

`%notin%` <- Negate( `%in%` )

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
  if ( length( vars_present ) == 0L ) return( d )

  n_coerced <- 0L

  for ( v in vars_present ) {
    x <- d[[ v ]]

    # bit64 integer64 (how data.table reads large efile integers): convert
    # through bit64's own character method so the 64-bit payload is not
    # reinterpreted as a tiny double. 990 line items are whole dollars, so
    # nothing is lost -- and this is silent, since it is not an error condition.
    if ( inherits( x, "integer64" ) ) {
      d[[ v ]] <- as.numeric( bit64::as.character.integer64( x ) )
      next
    }

    # Plain numeric (double or integer): already usable.
    if ( is.numeric( x ) ) next

    # Character / factor: only a genuine letter signals a real problem. Blank and
    # whitespace-only cells are missing values, not "non-digit characters".
    chr <- trimws( as.character( x ) )
    if ( any( grepl( "[A-Za-z]", chr ) ) )
      stop( "Non-numeric text detected in column '", v,
            "'. Ensure financial variables contain only numeric values before ",
            "calling this function." )
    d[[ v ]] <- suppressWarnings( as.numeric( chr ) )
    n_coerced <- n_coerced + 1L
  }

  if ( n_coerced > 0L )
    warning( paste0( n_coerced, " character column(s) were coerced to numeric." ) )

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


#' Apply winsorization, normalization, and percentile ranking to a ratio vector
#'
#' @description
#' `apply_transformations()` is the central post-computation step called by
#' every `get_*()` ratio function. It produces four versions of a ratio:
#'
#' - **raw** (`_raw`): the unmodified computed ratio.
#' - **winsorized** (`_w`): outliers clipped to bounds determined by the
#'   `range` argument and the `winsorize` proportion, via [winsorize_x()].
#' - **normalized** (`_z`): a distribution-appropriate transformation of the
#'   winsorized values. Parameters are fitted on the stable interior (non-NA,
#'   non-sentinel observations) via [find_best_normalization()], then scored
#'   on the full vector via [apply_normalization()], so sentinel pile-up at
#'   winsorization bounds does not distort the centering and spread estimates.
#' - **percentile** (`_p`): integer percentile rank (1-100) based on the raw
#'   values, via [dplyr::ntile()].
#'
#' @param x Numeric vector (the computed ratio, before any transformation).
#' @param winsorize Winsorization proportion between 0 and 1 (default `0.98`,
#'   which clips at the 1st and 99th percentiles for `"np"` range).
#' @param offset Sentinel offset applied to fixed bounds (default `0.001`).
#'   Observations clipped to a fixed bound are stored as `bound -- offset` so
#'   they remain identifiable in the `_w` column.
#' @param range Character string describing the theoretical range of the ratio.
#'   Controls how the lower and upper winsorization bounds are determined:
#'   \describe{
#'     \item{`"np"`}{Negative to positive (unbounded both directions). Winsorizes
#'       symmetrically at the `(1-winsorize)/2` and `1-(1-winsorize)/2`
#'       percentiles. Default behaviour.}
#'     \item{`"zp"`}{Zero to positive. The lower bound is fixed at `-offset`
#'       (flagging truncated-at-zero values) and the upper bound is the
#'       `winsorize` percentile of the full distribution.}
#'     \item{`"zo"`}{Zero to one. Both bounds are fixed (`-offset` and
#'       `1+offset`), flagging values outside `[0, 1]`. No percentile-based
#'       clipping is applied.}
#'     \item{`"nz"`}{Negative to zero. The upper bound is fixed at `+offset`
#'       and the lower bound is the `1-winsorize` percentile.}
#'     \item{`"lo;hi"`}{Custom numeric range, e.g. `"0;10"`. The lower bound
#'       is fixed at `lo - offset` and the upper bound at `hi + offset`.}
#'   }
#' @param normalize_type Transformation type override passed to
#'   [find_best_normalization()]. One of `NULL` (auto-detect), `"asinh"`,
#'   `"logit"`, `"rank_normal"`, or `"hurdle"`. Default `NULL`.
#'
#' @return A named list with elements `raw`, `winsorized`, `z`, `pctile`.
#'
#' @details
#' **Winsorization**
#'
#' Delegated entirely to [winsorize_x()], which handles all range codes,
#' computes sentinel flags, and returns the winsorized vector alongside
#' diagnostic metadata.
#'
#' **Normalization (`_z` column)**
#'
#' [find_best_normalization()] is called first to fit transformation
#' parameters (type, scale constant, center, spread) on the stable interior
#' of the winsorized distribution. [apply_normalization()] then scores the
#' full original vector using those fitted parameters. This two-step design
#' means the fitted model can be reused on new data if needed.
#'
#' **Percentile rank (`_p` column)**
#'
#' Integer percentile rank from 1 to 100 based on the raw (pre-winsorized)
#' values, computed with [dplyr::ntile()].
#'
#' @seealso [winsorize_x()], [find_best_normalization()],
#'   [apply_normalization()], [normalize_x()], [plot_normalize_x()]
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' # winsorize and normalize the debt-to-assets ratio
#' ratio <- dat10k$F9_10_LIAB_TOT_EOY / dat10k$F9_10_ASSET_TOT_EOY
#' out   <- apply_transformations( ratio, winsorize = 0.98, range = "zo" )
#' names( out )
#' summary( out$z )
#' @export
apply_transformations <- function( x, winsorize = 0.98, offset = 0.001,
                                   range = "np", normalize_type = NULL ) {

  # There is no distribution to fit when every value is missing. Return the
  # correctly shaped outputs so ratio functions preserve NA/NaN positions.
  if (!any(!is.na(x))) {
    missing_numeric <- rep(NA_real_, length(x))
    return(list(
      raw = x,
      winsorized = missing_numeric,
      z = missing_numeric,
      pctile = rep(NA_integer_, length(x))
    ))
  }

  # Winsorize once up front. With fewer than three usable observations there
  # is not enough information to fit a distributional transformation, but raw
  # ratios and winsorized values should still be returned.
  w <- winsorize_x(x = x, range = range, winsorize = winsorize, offset = offset)
  x.w <- w$x_w
  if (sum(!is.na(x.w)) < 3L) {
    return(list(
      raw = x,
      winsorized = x.w,
      z = rep(NA_real_, length(x)),
      pctile = dplyr::ntile(x, 100)
    ))
  }

  # ---- 1. fit normalization on stable interior ----
  # find_best_normalization() internally calls winsorize_x() to isolate
  # sentinels before fitting; verbose = FALSE suppresses the fit summary.
  fit <- find_best_normalization(
    x         = x,
    range     = range,
    vtype     = normalize_type,
    winsorize = winsorize,
    offset    = offset,
    verbose   = FALSE
  )

  # ---- 2. winsorize (re-use fitted settings for consistency) ----
  # ---- 3. score full vector using fitted parameters ----
  x.z <- apply_normalization( x = x, fit = fit, verbose = FALSE )

  # ---- 4. percentile rank on raw values ----
  x.p <- dplyr::ntile( x, 100 )

  return( list( raw = x, winsorized = x.w, z = x.z, pctile = x.p ) )
}


#' Backward-compatible alias for apply_transformations()
#'
#' @description
#' `winsorize_var()` is a deprecated alias retained for backward compatibility.
#' New code should use [apply_transformations()] directly.
#'
#' @inheritParams apply_transformations
#' @return Same as [apply_transformations()].
#' @export
winsorize_var <- function( x, winsorize = 0.98, offset = 0.001,
                           range = "np", normalize_type = NULL ) {
  apply_transformations( x, winsorize = winsorize, offset = offset,
                         range = range, normalize_type = normalize_type )
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
#' @param pz_vars Optional character vector of PZ-scope column names (present on
#'   both 990 and 990EZ). When supplied, the local scope-aware imputer is used
#'   with exactly these fields. When `NULL` (default), scope is resolved from
#'   panel990 via [panel990::panel_normalize()].
#' @param pc_vars Optional character vector of PC-scope column names (full 990
#'   only). When supplied, imputation is restricted to 990 filers for these
#'   fields. When `NULL` (default), scope is resolved from panel990.
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
#' As of the panel990 portage, the default path delegates to
#' [panel990::panel_normalize()], which applies the same form-scoped, non-filer-
#' protected zero imputation over the shared concordance-derived field scopes.
#' Supplying `pz_vars`/`pc_vars` explicitly falls back to the local imputer for
#' backward compatibility.
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
                                  pz_vars = NULL,
                                  pc_vars = NULL ) {

  # Default path: delegate to panel990's form-scoped normalizer so fiscal and
  # panel990 share one imputation engine and one field-scope source of truth.
  if ( is.null( pz_vars ) && is.null( pc_vars ) ) {
    return( panel990::panel_normalize( df, fields = "core", verbose = FALSE ) )
  }

  # Back-compat path: explicit field sets use the local scope-aware imputer.
  if ( is.null( pz_vars ) ) pz_vars <- .PZ_FIELDS
  if ( is.null( pc_vars ) ) pc_vars <- .PC_FIELDS
  ez_rows  <- detect_ez_rows( df )
  all_vars <- union( pz_vars, pc_vars )
  impute_zero( df, vars = all_vars, ez_rows = ez_rows )
}


# Parse RETURN_TIME_STAMP cleanly: 
 #' @keywords internal
 #' @noRd
.parse_stamp <- function( x ) {
  x_chr <- as.character( x )
  x_chr[ is.na( x_chr ) | trimws( x_chr ) == "" ] <- NA_character_
  x_chr <- trimws(x_chr)
  x_chr <- sub(" (UTC|GMT)$", "", x_chr, ignore.case = TRUE)

  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = "UTC")
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d"
  )

  for (fmt in formats) {
    missing <- is.na(out) & !is.na(x_chr)
    if (!any(missing)) break
    out[missing] <- suppressWarnings(as.POSIXct(x_chr[missing], format = fmt, tz = "UTC"))
  }

  out
}
