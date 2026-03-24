###---------------------------------------------------
###   PANEL DATA UTILITIES
###---------------------------------------------------


# ---- get_panel() -----------------------------------------------------

#' @title
#' Retrieve a multi-year IRS 990 efile panel dataset
#'
#' @description
#' Downloads individual years of IRS 990 efile data using
#' [retrieve_efile_data()], adds a `TAX_YEAR` column to each year's data
#' frame, then stacks them into a single long-format panel.
#'
#' **Calculated For:** 990 + 990EZ filers (determined by the underlying
#' efile tables requested).
#'
#' @param years Integer vector of tax years to retrieve (e.g.,
#'   `2019:2023`).
#' @param tables Character vector of efile table codes passed to
#'   [retrieve_efile_data()]. Default `c("P00","P01","P08","P09","P10")`.
#' @param include_bmf Logical. Attach BMF organizational metadata after all
#'   years are stacked. Default `TRUE`. The BMF is downloaded exactly once
#'   regardless of how many years are requested, then filtered to only the
#'   EINs present in the assembled panel before joining — keeping memory use
#'   proportional to the panel rather than to the full BMF.
#' @param efile_root Base URL for the NCCS efile S3 bucket. Passed
#'   directly to [retrieve_efile_data()].
#' @param bmf_url Full URL for the Unified BMF CSV. Passed directly to
#'   [retrieve_efile_data()].
#' @param timeout HTTP timeout in seconds for each file download. Default
#'   `300`.
#' @param retry_max Integer. Maximum download attempts per efile table.
#'   Default `3`. Passed directly to [retrieve_efile_data()]. A failed
#'   table is retried with a 1–5 second random back-off between attempts;
#'   if it still fails the table is skipped and a warning is issued, but
#'   the year's other tables are unaffected.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `data.frame` in long format with one row per
#'   organization-year. The `TAX_YEAR` column identifies the filing year.
#'   Columns that exist in some years but not others are filled with `NA`.
#'
#' @details
#' ## Year loop and column alignment
#'
#' Each year is downloaded independently and stacked with
#' [dplyr::bind_rows()], which handles mismatched columns across years by
#' filling missing columns with `NA`. This is the expected behaviour when
#' the IRS changes form schemas between years.
#'
#' ## BMF join
#'
#' When `include_bmf = TRUE`, the BMF is downloaded exactly **once** after
#' all years have been retrieved and stacked. Before joining, the BMF is
#' filtered to `bmf$EIN2 %in% unique(panel$EIN2)` so that only the
#' organizations actually present in the panel are kept in memory.
#' This avoids both repeated large downloads across years and holding the
#' full ~200 MB BMF in memory alongside the panel.
#'
#' @seealso [retrieve_efile_data()], [panel_smooth()],
#'   [sanitize_financials()], [compute_all()]
#'
#' @examples
#' \dontrun{
#' # Retrieve a three-year panel
#' panel <- get_panel( years = 2019:2021 )
#' dim( panel )
#' table( panel$TAX_YEAR )
#'
#' # Retrieve without BMF (faster; attach manually later)
#' panel <- get_panel( years = 2019:2021, include_bmf = FALSE )
#' }
#'
#' @export
get_panel <- function(
    years,
    tables      = c( "P00", "P01", "P08", "P09", "P10" ),
    include_bmf = TRUE,
    efile_root  = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/",
    bmf_url     = "https://nccsdata.s3.us-east-1.amazonaws.com/bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv",
    timeout     = 300,
    retry_max   = 3L,
    verbose     = TRUE
) {

  # ---- validate years ----
  if ( !is.numeric(years) || length(years) == 0 ) {
    stop( "`years` must be a non-empty numeric vector, e.g. 2019:2023." )
  }
  years <- as.integer( sort( unique( years ) ) )

  year_list <- vector( "list", length(years) )

  for ( i in seq_along(years) ) {

    yr <- years[i]
    if (verbose) message( "\n===  Year ", yr, " (", i, " of ", length(years), ")  ===" )

    df_yr <- tryCatch(
      retrieve_efile_data(
        year        = yr,
        tables      = tables,
        include_bmf = FALSE,      # BMF downloaded once after stacking, not per year
        efile_root  = efile_root,
        timeout     = timeout,
        retry_max   = retry_max,
        verbose     = verbose
      ),
      error = function(e) {
        warning( "Year ", yr, " failed: ", conditionMessage(e), call. = FALSE )
        NULL
      }
    )

    if ( !is.null(df_yr) ) {
      # Ensure TAX_YEAR is present (retrieve_efile_data includes it via P00,
      # but add/overwrite to guarantee the correct value regardless of tables)
      df_yr[["TAX_YEAR"]] <- yr
      year_list[[i]] <- df_yr
    }
  }

  # Drop failed years
  year_list <- Filter( Negate(is.null), year_list )

  if ( length(year_list) == 0 ) {
    stop( "All years failed to download. Check your internet connection and the `efile_root` URL." )
  }

  if (verbose) message( "\nStacking ", length(year_list), " year(s) ..." )

  out <- dplyr::bind_rows( year_list )

  if (verbose) message(
    "Stacked: ", nrow(out), " rows x ", ncol(out), " columns ",
    "(", length(years), " year(s) requested, ",
    length(year_list), " retrieved)."
  )

  # ---- attach BMF once, after stacking, filtered to panel EINs ----
  if ( include_bmf ) {

    if (verbose) message( "\nDownloading BMF (once for all years) ..." )

    # Raise timeout; .attach_bmf restores it internally via retrieve_efile_data,
    # but we handle it here since we are calling .attach_bmf directly.
    old_timeout <- getOption( "timeout" )
    on.exit( options( timeout = old_timeout ), add = TRUE )
    options( timeout = timeout )

    bmf_raw <- tryCatch(
      data.table::fread( bmf_url, showProgress = FALSE ),
      error = function(e) {
        warning( "BMF download failed: ", conditionMessage(e),
                 " — returning panel without BMF.", call. = FALSE )
        NULL
      }
    )

    if ( !is.null(bmf_raw) ) {

      # Filter BMF to only EINs present in the panel before any processing
      panel_eins <- unique( out[["EIN2"]] )
      panel_eins <- panel_eins[ !is.na(panel_eins) ]

      if (verbose) message(
        "  Filtering BMF to ", format( length(panel_eins), big.mark = "," ),
        " unique EIN(s) in panel ..."
      )

      data.table::setDT( bmf_raw )
      bmf_raw <- bmf_raw[ EIN2 %in% panel_eins ]

      if (verbose) message(
        "  Matched ", format( nrow(bmf_raw), big.mark = "," ), " BMF record(s)."
      )

      # Reuse .attach_bmf's deduplication and NTEE enrichment logic by
      # reconstructing a minimal data.table and calling the internal helper.
      # Since we already filtered, pass the pre-filtered bmf_raw directly
      # through the same processing steps .attach_bmf would apply.
      data.table::setDT( out )
      out <- .attach_bmf_filtered( out, bmf_raw = bmf_raw, verbose = verbose )

    }
  }

  if (verbose) message(
    "\nDone. Panel: ", nrow(out), " rows x ", ncol(out), " columns."
  )

  as.data.frame( out )
}


# ---- internal helper: attach pre-filtered BMF to panel ----
# Mirrors .attach_bmf() in retrieve-efile-data.R but accepts an already-
# filtered bmf data.table rather than downloading from a URL.

.attach_bmf_filtered <- function( df, bmf_raw, verbose ) {

  bmf <- data.table::copy( bmf_raw )
  bmf <- unique( bmf )   # drop exact duplicate rows before deduplication by EIN

  if (verbose) message( "  Deduplicating BMF by EIN ..." )

  # Keep one row per EIN — most recent ruling date
  data.table::setorder( bmf, EIN, -ORG_RULING_DATE )
  bmf <- bmf[ !duplicated(EIN) ]

  # Normalize NTEE codes
  ntee <- bmf$NTEE_NCCS
  ntee[ ntee == "" | is.na(ntee) ] <- bmf$NTEE_IRS[ ntee == "" | is.na(ntee) ]

  bmf[ , NTEE_NCCS     := get_clean_ntee(ntee) ]
  bmf[ , NTEEV2        := get_nteev2(ntee)     ]
  bmf[ , NTMAJ12       := get_industry(ntee)   ]
  bmf[ , NTEE_ORG_TYPE := get_org_type(ntee)   ]
  bmf[ , ORG_RULING_YEAR := as.integer( substr(ORG_RULING_DATE, 1, 4) ) ]

  bmf_keep <- intersect(
    c( "EIN2",
       "NTEE_NCCS", "NTEEV2", "NTMAJ12", "NTEE_ORG_TYPE",
       "CENSUS_CBSA_FIPS", "CENSUS_CBSA_NAME",
       "CENSUS_BLOCK_FIPS", "CENSUS_URBAN_AREA",
       "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME",
       "BMF_SUBSECTION_CODE", "BMF_FOUNDATION_CODE",
       "ORG_RULING_YEAR",
       "F990_TOTAL_REVENUE_RECENT", "F990_TOTAL_INCOME_RECENT",
       "F990_TOTAL_ASSETS_RECENT",  "F990_TOTAL_EXPENSES_RECENT" ),
    colnames(bmf)
  )
  bmf <- bmf[ , ..bmf_keep ]

  data.table::setkey( df,  EIN2 )
  data.table::setkey( bmf, EIN2 )

  if (verbose) message( "  Left-joining BMF onto panel by EIN2 ..." )
  df <- merge( df, bmf, by = "EIN2", all.x = TRUE )

  df
}


# ---- panel_smooth() --------------------------------------------------

#' @title
#' Smooth financial variables across time within a panel dataset
#'
#' @description
#' Applies a centered rolling window average within each organization's
#' time series to reduce year-to-year noise in financial variables. The
#' data frame is returned with the same dimensions and column names; only
#' the selected financial columns are replaced with their smoothed values.
#'
#' **Window behavior at panel edges:** When an observation is within
#' `floor(window/2)` periods of the start or end of an organization's
#' series, the window shifts to stay within the available data rather than
#' shrinking. This means every smoothed value is based on exactly
#' `min(window, n)` observations, where `n` is the panel length for that
#' organization.
#'
#' **Missing values:** `NA` observations are dropped from the window
#' before computing the weighted average. Their weight is reallocated
#' proportionally to the remaining non-missing observations so the weights
#' always sum to 1. If all observations in a window are `NA`, the smoothed
#' value is `NA`.
#'
#' **`NaN` values** (produced by [get_panel()] ratio functions for
#' structurally undefined observations, e.g. division by zero) are treated
#' as `NA` during smoothing and remain `NaN` in the output for those
#' positions.
#'
#' @param df A `data.frame` containing a panel of financial data with one
#'   row per organization-year.
#' @param vars Which financial variables to smooth. One of:
#'   \describe{
#'     \item{`"PZ"`}{Smooth all PZ-scope fields returned by
#'       [get_pz_fields()] that are present in `df`.}
#'     \item{`"PC"`}{Smooth all PC-scope fields returned by
#'       [get_pc_fields()] that are present in `df`.}
#'     \item{`"ALL"`}{Smooth all PC and PZ fields present in `df`.}
#'     \item{character vector}{A custom list of column names to smooth.
#'       All names must be present in `df`.}
#'   }
#' @param window Odd integer >= 1. Width of the rolling window in time
#'   periods. Default `3` (current year plus one year on each side).
#'   `window = 1` returns the data unchanged.
#' @param time Name of the column identifying the time period. Default
#'   `"TAX_YEAR"`.
#' @param id Name of the column identifying the organization. Default
#'   `"EIN2"`.
#' @param weights Weighting scheme for the rolling window. One of:
#'   \describe{
#'     \item{`"equal"`}{Uniform weights — each observation in the window
#'       contributes equally. Equivalent to a simple moving average.}
#'     \item{`"half"`}{The center observation receives weight 1/2; the
#'       remaining 1/2 is split equally among all other observations in
#'       the window. Emphasizes the focal year while incorporating
#'       neighbouring context.}
#'     \item{`"decay"`}{Half-life exponential decay from the center:
#'       distance 0 → weight 1, distance 1 → 1/2, distance 2 → 1/4,
#'       etc. Strongly emphasizes the focal year.}
#'   }
#' @param verbose Logical. If `TRUE` (default), prints a one-line
#'   progress message at the start and completion of each year's
#'   smoothing pass, including the number of organizations and columns
#'   being processed. Useful for monitoring progress on large panels.
#'   Set to `FALSE` to suppress all messages.
#'
#' @return A `data.frame` with the same dimensions and column names as
#'   `df`. The columns identified by `vars` contain smoothed values;
#'   all other columns (identifiers, non-numeric, excluded fields) are
#'   unchanged.
#'
#' @details
#' ## Choosing a weighting scheme
#'
#' `"equal"` is the simplest and most transparent option — it is the
#' standard moving average and makes no assumptions about the relative
#' importance of focal vs. neighbouring years.
#'
#' `"half"` is a reasonable default for nonprofit financial ratios where
#' the reported year is the most informative but a single year's value
#' is known to be noisy (e.g., due to one-time gifts or accrual timing).
#'
#' `"decay"` is appropriate when you believe recent-year data is
#' progressively more informative than distant years — for instance when
#' smoothing raw financial levels that have secular trends.
#'
#' ## Panel length and the window
#'
#' Organizations with fewer years than `window` are smoothed over their
#' full available history rather than being dropped or truncated. This
#' means short panels are smoothed more aggressively than long ones when
#' the same `window` is requested. Use `window = 1` to apply no smoothing
#' to any organization.
#'
#' ## Row order
#'
#' The output rows are returned in the same order as the input `df`.
#'
#' @seealso [get_panel()], [get_pz_fields()], [get_pc_fields()]
#'
#' @examples
#' \dontrun{
#' panel <- get_panel( years = 2019:2022 )
#'
#' # Smooth all PZ-scope fields (available for 990 + 990EZ filers)
#' panel_s <- panel_smooth( panel, vars = "PZ" )
#'
#' # Smooth a custom set of columns with decay weighting
#' panel_s <- panel_smooth(
#'   panel,
#'   vars    = c( "F9_01_REV_TOT_CY", "F9_01_EXP_TOT_CY" ),
#'   window  = 5,
#'   weights = "decay"
#' )
#' }
#'
#' @export
panel_smooth <- function(
    df,
    vars    = "PZ",
    window  = 3,
    time    = "TAX_YEAR",
    id      = "EIN2",
    weights = "equal",
    verbose = TRUE
) {

  # ---- input checks -------------------------------------------------------

  if ( !is.data.frame(df) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( length(window) != 1 || !is.numeric(window) || is.na(window) ) {
    stop( "`window` must be a single odd integer >= 1." )
  }
  window <- as.integer(window)
  if ( window < 1 || window %% 2 == 0 ) {
    stop( "`window` must be an odd integer >= 1 (e.g. 1, 3, 5, ...)." )
  }

  if ( !time %in% names(df) ) {
    stop( paste0( "`time` column not found in df: \"", time, "\"" ) )
  }
  if ( !id %in% names(df) ) {
    stop( paste0( "`id` column not found in df: \"", id, "\"" ) )
  }

  allowed_weights <- c( "equal", "half", "decay" )
  if ( length(weights) != 1 || !weights %in% allowed_weights ) {
    stop( paste0( '`weights` must be one of: "',
                  paste(allowed_weights, collapse = '", "'), '".' ) )
  }

  # ---- resolve vars argument to a concrete column vector ------------------

  if ( identical(vars, "PZ") ) {
    candidate <- get_pz_fields()
  } else if ( identical(vars, "PC") ) {
    candidate <- get_pc_fields()
  } else if ( identical(vars, "ALL") ) {
    candidate <- union( get_pc_fields(), get_pz_fields() )
  } else {
    # custom character vector
    if ( !is.character(vars) || length(vars) == 0 ) {
      stop( '`vars` must be "PZ", "PC", "ALL", or a non-empty character vector of column names.' )
    }
    missing_cols <- vars[ !vars %in% names(df) ]
    if ( length(missing_cols) > 0 ) {
      stop( paste0( "Column(s) in `vars` not found in df: ",
                    paste(missing_cols, collapse = ", ") ) )
    }
    candidate <- vars
  }

  # Keep only columns that (a) exist in df and (b) are numeric
  smooth_cols <- intersect( candidate, names(df) )
  smooth_cols <- smooth_cols[ vapply(
    smooth_cols,
    function(v) is.numeric(df[[v]]),
    logical(1)
  )]

  if ( length(smooth_cols) == 0 ) {
    warning( "No numeric columns matched `vars` — returning df unchanged." )
    return(df)
  }

  # ---- helpers ------------------------------------------------------------

  # Return window indices centered on i, clamped to [1, n], shifting at edges
  .get_window_idx <- function( i, n, w ) {
    w    <- min(w, n)
    half <- floor(w / 2)
    s    <- i - half
    e    <- i + half

    if ( s < 1 ) {
      shift <- 1L - s
      s     <- 1L
      e     <- min(n, e + shift)
    }
    if ( e > n ) {
      shift <- e - n
      e     <- n
      s     <- max(1L, s - shift)
    }
    seq.int(s, e)
  }

  # Raw (pre-NA-drop) weights for a window index vector and focal position
  .get_raw_weights <- function( idx, center_pos, scheme ) {
    k <- length(idx)
    if ( scheme == "equal" ) return( rep(1, k) )

    d <- abs(idx - center_pos)

    if ( scheme == "half" ) {
      # center gets weight 1, each of the (k-1) others gets weight 1/(k-1),
      # so after normalisation center = 1/2 and others share the remaining 1/2.
      # Robust to k == 1: only focal observation, weight = 1.
      w <- ifelse( d == 0, 1, if ( k > 1 ) 1 / ( k - 1 ) else 0 )
      return(w)
    }

    if ( scheme == "decay" ) {
      # half-life decay: distance 0 -> 1, distance 1 -> 1/2, distance 2 -> 1/4, ...
      return( 0.5 ^ d )
    }
  }

  # Smooth one numeric vector for one organization's sorted time series
  .smooth_one <- function( x, scheme, w ) {
    n   <- length(x)
    out <- rep(NA_real_, n)

    # Track which positions were originally NaN (not NA)
    is_nan <- is.nan(x)
    # Treat NaN as NA for smoothing purposes
    x[ is_nan ] <- NA_real_

    for ( i in seq_len(n) ) {
      idx   <- .get_window_idx(i, n, w)
      vals  <- x[idx]
      rw    <- .get_raw_weights(idx, center_pos = i, scheme = scheme)
      keep  <- !is.na(vals)

      if ( !any(keep) ) {
        out[i] <- NA_real_
      } else {
        v      <- vals[keep]
        rw     <- rw[keep]
        rw     <- rw / sum(rw)
        out[i] <- sum(v * rw)
      }
    }

    # Restore NaN for originally-NaN positions
    out[ is_nan ] <- NaN
    out
  }

  # ---- split by id, sort by time, smooth, reassemble ----------------------

  # Tag each row with its original position so we can restore input order
  df[["..row.."]] <- seq_len( nrow(df) )

  years_present <- sort( unique( df[[time]] ) )
  n_years       <- length( years_present )
  n_cols        <- length( smooth_cols )

  if (verbose) message(
    "Smoothing ", format( n_cols, big.mark = "," ),
    " column(s) across ", n_years, " year(s) ..."
  )

  # Split once by id.  Each org's full time series is smoothed in one pass
  # so the window can look across all available years for that org.
  split_df <- split( df, df[[id]], drop = TRUE )

  # Map each org to the first year it appears in.  We use this to batch
  # orgs by year for progress reporting — the smoothing itself is unchanged.
  org_first_year <- vapply(
    split_df,
    function(d) min( d[[time]], na.rm = TRUE ),
    numeric(1)
  )

  smoothed_list <- vector( "list", length(split_df) )
  names(smoothed_list) <- names(split_df)

  for ( yi in seq_along(years_present) ) {

    yr         <- years_present[[yi]]
    org_ids    <- names(split_df)[ org_first_year == yr ]
    n_orgs_yr  <- length(org_ids)

    # Count all rows that fall in this year (across all orgs)
    n_rows_yr  <- sum( df[[time]] == yr, na.rm = TRUE )

    if (verbose) message(
      "  Year ", yr,
      " (", yi, "/", n_years, "):  ",
      format( n_rows_yr,  big.mark = "," ), " row(s), ",
      format( n_orgs_yr,  big.mark = "," ), " org(s) to smooth ...",
      appendLF = FALSE
    )

    for ( org in org_ids ) {
      d   <- split_df[[org]]
      ord <- order( d[[time]] )
      d   <- d[ord, , drop = FALSE]
      for ( v in smooth_cols ) {
        d[[v]] <- .smooth_one( x = d[[v]], scheme = weights, w = window )
      }
      smoothed_list[[org]] <- d
    }

    if (verbose) message( " done." )
  }

  out <- do.call( rbind, smoothed_list )

  # Restore original row order
  out <- out[ order(out[["..row.."]]), , drop = FALSE ]
  out[["..row.."]] <- NULL
  rownames(out)    <- NULL

  if (verbose) message(
    "Done. ", format( nrow(out), big.mark = "," ),
    " row(s) smoothed across ", n_years, " year(s)."
  )

  out
}


# ---- deduplicate() ---------------------------------------------------

#' @title
#' Deduplicate a panel to one filing per organization per year
#'
#' @description
#' IRS 990 efile data can contain multiple records for the same organization
#' in a single tax year due to amended returns, partial-year filings, and
#' group returns. `deduplicate()` reduces the data to at most one record
#' per `by_id` x `by_year` combination using a three-step heuristic, then
#' prints a summary report of how many organizations and records were
#' affected.
#'
#' **Deduplication heuristics (applied in order):**
#'
#' 1. **Drop group returns** — filings where `RETURN_GROUP_X == "X"` are
#'    removed first. Group returns aggregate activity across multiple
#'    subordinate organizations and are not suitable as individual-organization
#'    observations in a panel.
#' 2. **Drop partial-year returns** — filings where `RETURN_PARTIAL_X == "X"`
#'    are removed next. Partial-year returns cover less than a full fiscal
#'    year and are not comparable to full-year filings for the same period.
#' 3. **Keep most recent filing** — when multiple records remain for the
#'    same organization-year, the filing with the latest `RETURN_TIME_STAMP`
#'    is retained. This is typically the most recently filed amended return,
#'    which supersedes earlier submissions for the same period.
#'
#' **Safety guarantee:** at least one record per organization-year is always
#' retained. If applying step 1 or 2 would eliminate *all* records for a
#' given organization-year (e.g., an organization that only filed a group
#' return), those records are kept and a warning is issued.
#'
#' **Missing flag columns:** if `RETURN_GROUP_X`, `RETURN_PARTIAL_X`, or
#' `RETURN_TIME_STAMP` are absent from `df`, the corresponding step is
#' skipped with a message.
#'
#' @param df A `data.frame` containing IRS 990 efile data with one or more
#'   rows per organization-year. Typically the output of [get_panel()].
#' @param by_id Name of the column identifying the organization. Default
#'   `"EIN2"`.
#' @param by_year Name of the column identifying the tax year. Default
#'   `"TAX_YEAR"`.
#' @param verbose Logical. If `TRUE` (default), prints a deduplication
#'   report showing records dropped per step and the distribution of
#'   duplicates per organization per year.
#'
#' @return A `data.frame` with the same columns as `df` and at most one
#'   row per `by_id` x `by_year` combination. Row names are reset.
#'
#' @details
#' ## Deduplication report
#'
#' When `verbose = TRUE`, the function prints:
#'
#' - Starting dimensions (rows and unique org-years).
#' - Records dropped at each step (group returns, partial-year returns,
#'   timestamp tie-breaking), with counts per year.
#' - A frequency table of how many *extra* records existed per
#'   organization-year before deduplication (e.g., "42 org-years had
#'   2 total filings, 7 had 3 total filings").
#' - Final dimensions after deduplication.
#'
#' ## When to call this function
#'
#' Call `deduplicate()` on the raw output of [get_panel()] before passing
#' the panel to [panel_smooth()] or [compute_all()], since both assume
#' one observation per organization-year.
#'
#' @seealso [get_panel()], [panel_smooth()]
#'
#' @examples
#' \dontrun{
#' panel <- get_panel( years = 2019:2022 )
#' panel_clean <- deduplicate( panel )
#' dim( panel_clean )
#'
#' # Suppress the report
#' panel_clean <- deduplicate( panel, verbose = FALSE )
#' }
#'
#' @export
deduplicate <- function(
    df,
    by_id   = "EIN2",
    by_year = "TAX_YEAR",
    verbose = TRUE
) {

  # ---- input checks -------------------------------------------------------
  if ( !is.data.frame(df) ) stop( "`df` must be a data.frame." )
  if ( !by_id   %in% names(df) ) stop( paste0( "`by_id` column not found: \"",   by_id,   "\"" ) )
  if ( !by_year %in% names(df) ) stop( paste0( "`by_year` column not found: \"", by_year, "\"" ) )

  n_start     <- nrow(df)
  pairs_start <- nrow( unique( df[ c(by_id, by_year) ] ) )

  # convenience: make a group key for fast subsetting
  df[["..key.."]] <- paste( df[[by_id]], df[[by_year]], sep = "___" )

  # compute per-key filing counts BEFORE any deduplication (used in report)
  key_counts_orig <- table( df[["..key.."]] )

  # keep a running tally for the report
  report <- list()

  # ---- helper: flag is_set for _X indicator columns ----------------------
  # NCCS convention: "X" = flagged, "" / NA = not flagged
  .is_flagged <- function( x ) {
    !is.na(x) & trimws(x) == "X"
  }

  # ---- helper: safe drop with "keep at least one" guarantee --------------
  # Drops rows where `flag` is TRUE, but never drops the last record
  # for any key group. Returns list(df, dropped_keys, rescued_keys).
  .safe_drop <- function( d, flag, step_name ) {

    keys_to_drop <- unique( d[["..key.."]][flag] )

    if ( length(keys_to_drop) == 0 ) {
      return( list( df = d, n_dropped = 0L, n_rescued = 0L,
                    dropped_by_year = integer(0) ) )
    }

    # For each key_to_drop, check whether ALL records for that key are flagged.
    # If so, we must rescue the whole key (keep all records for it).
    rescued_keys <- character(0)
    actually_drop <- flag

    for ( k in keys_to_drop ) {
      in_key    <- d[["..key.."]] == k
      all_flagged <- all( flag[in_key] )
      if ( all_flagged ) {
        actually_drop[ in_key ] <- FALSE
        rescued_keys <- c( rescued_keys, k )
      }
    }

    dropped_rows <- d[ actually_drop, , drop = FALSE ]
    n_dropped    <- nrow( dropped_rows )

    # tabulate by year for per-year report
    if ( n_dropped > 0 ) {
      dropped_by_year <- table( dropped_rows[[ by_year ]] )
    } else {
      dropped_by_year <- integer(0)
    }

    d_out <- d[ !actually_drop, , drop = FALSE ]

    list(
      df             = d_out,
      n_dropped      = n_dropped,
      n_rescued      = length( rescued_keys ),
      rescued_keys   = rescued_keys,
      dropped_by_year = dropped_by_year
    )
  }

  # ---- step 1: drop group returns ----------------------------------------
  col_group <- "RETURN_GROUP_X"

  if ( !col_group %in% names(df) ) {
    if (verbose) message( "  Step 1 (group returns): column '", col_group,
                          "' not found — step skipped." )
    report$group <- list( n_dropped = 0L, n_rescued = 0L, dropped_by_year = integer(0) )
  } else {
    flag_group <- .is_flagged( df[[col_group]] )
    res1       <- .safe_drop( df, flag_group, "group returns" )
    df         <- res1$df
    report$group <- res1
    if ( verbose && res1$n_rescued > 0 ) {
      warning( "  Step 1: ", res1$n_rescued,
               " org-year(s) had ONLY group returns — kept to preserve coverage.",
               call. = FALSE )
    }
  }

  # ---- step 2: drop partial-year returns ---------------------------------
  col_partial <- "RETURN_PARTIAL_X"

  if ( !col_partial %in% names(df) ) {
    if (verbose) message( "  Step 2 (partial returns): column '", col_partial,
                          "' not found — step skipped." )
    report$partial <- list( n_dropped = 0L, n_rescued = 0L, dropped_by_year = integer(0) )
  } else {
    flag_partial <- .is_flagged( df[[col_partial]] )
    res2         <- .safe_drop( df, flag_partial, "partial returns" )
    df           <- res2$df
    report$partial <- res2
    if ( verbose && res2$n_rescued > 0 ) {
      warning( "  Step 2: ", res2$n_rescued,
               " org-year(s) had ONLY partial returns — kept to preserve coverage.",
               call. = FALSE )
    }
  }

  # ---- step 3: keep most recent filing per org-year ----------------------
  col_stamp <- "RETURN_TIME_STAMP"

  # Find duplicated keys remaining after steps 1-2
  dup_keys <- df[["..key.."]][ duplicated( df[["..key.."]]) ]
  dup_keys <- unique( dup_keys )

  n_dropped_ts <- 0L
  dropped_ts_by_year <- integer(0)

  if ( length(dup_keys) > 0 ) {

    if ( !col_stamp %in% names(df) ) {
      if (verbose) message( "  Step 3 (timestamp tie-break): column '", col_stamp,
                            "' not found — keeping first record per duplicate group." )
      # Fallback: keep first record per key
      keep_idx <- !duplicated( df[["..key.."]] )
    } else {
      # For each duplicate key, keep the row with the maximum timestamp.
      # Ties in timestamp are broken by row order (first encountered kept).
      keep_idx <- rep( TRUE, nrow(df) )

      for ( k in dup_keys ) {
        in_key <- which( df[["..key.."]] == k )
        stamps <- df[[col_stamp]][ in_key ]

        # Treat NA timestamp as the earliest possible (empty string sorts first)
        stamps_clean <- ifelse( is.na(stamps), "", as.character(stamps) )
        best <- in_key[ which.max( stamps_clean ) ]
        drop <- setdiff( in_key, best )
        keep_idx[ drop ] <- FALSE
        n_dropped_ts <- n_dropped_ts + length(drop)
      }

      # build per-year summary for dropped timestamp rows
      if ( n_dropped_ts > 0 ) {
        dropped_ts_rows    <- df[ !keep_idx, , drop = FALSE ]
        dropped_ts_by_year <- table( dropped_ts_rows[[ by_year ]] )
      }
    }

    df <- df[ keep_idx, , drop = FALSE ]
  }

  report$timestamp <- list(
    n_dropped      = n_dropped_ts,
    dropped_by_year = dropped_ts_by_year
  )

  # ---- clean up key column -----------------------------------------------
  df[["..key.."]] <- NULL
  rownames(df)    <- NULL

  n_end      <- nrow(df)
  pairs_end  <- nrow( unique( df[ c(by_id, by_year) ] ) )
  n_total_dropped <- n_start - n_end

  # ---- report ------------------------------------------------------------
  if (verbose) {

    cat( "\n=== deduplicate() report ===\n\n" )
    cat( sprintf( "  Starting:  %s rows | %s unique org-years\n",
                  format( n_start,  big.mark = "," ),
                  format( pairs_start, big.mark = "," ) ) )
    cat( "\n" )

    # Step 1 summary
    g <- report$group
    cat( sprintf( "  Step 1 — Drop group returns:       %s record(s) removed",
                  format( g$n_dropped, big.mark = "," ) ) )
    if ( g$n_rescued > 0 )
      cat( sprintf( "  (%d org-year(s) rescued)", g$n_rescued ) )
    cat( "\n" )
    if ( length(g$dropped_by_year) > 0 ) {
      cat( "           by year:\n" )
      for ( yr in names(g$dropped_by_year) )
        cat( sprintf( "             %s:  %s\n", yr,
                      format( g$dropped_by_year[[yr]], big.mark = "," ) ) )
    }

    # Step 2 summary
    p <- report$partial
    cat( sprintf( "  Step 2 — Drop partial returns:     %s record(s) removed",
                  format( p$n_dropped, big.mark = "," ) ) )
    if ( p$n_rescued > 0 )
      cat( sprintf( "  (%d org-year(s) rescued)", p$n_rescued ) )
    cat( "\n" )
    if ( length(p$dropped_by_year) > 0 ) {
      cat( "           by year:\n" )
      for ( yr in names(p$dropped_by_year) )
        cat( sprintf( "             %s:  %s\n", yr,
                      format( p$dropped_by_year[[yr]], big.mark = "," ) ) )
    }

    # Step 3 summary
    ts <- report$timestamp
    cat( sprintf( "  Step 3 — Timestamp tie-break:      %s record(s) removed\n",
                  format( ts$n_dropped, big.mark = "," ) ) )
    if ( length(ts$dropped_by_year) > 0 ) {
      cat( "           by year:\n" )
      for ( yr in names(ts$dropped_by_year) )
        cat( sprintf( "             %s:  %s\n", yr,
                      format( ts$dropped_by_year[[yr]], big.mark = "," ) ) )
    }

    cat( "\n" )
    cat( sprintf( "  Total removed: %s record(s) across %s org-year(s)\n",
                  format( n_total_dropped, big.mark = "," ),
                  format( pairs_start - pairs_end, big.mark = "," ) ) )

    if ( n_total_dropped > 0 ) {
      # Exact distribution: how many org-years had k total filings (before dedup)?
      filing_dist <- table( as.integer( key_counts_orig ) )
      dup_dist    <- filing_dist[ as.integer( names(filing_dist) ) > 1 ]

      if ( length(dup_dist) > 0 ) {
        cat( "\n  Distribution of total filings per org-year (before deduplication):\n" )
        cat( "  (only org-years with > 1 filing shown)\n\n" )
        cat( sprintf( "  %-16s  %s\n", "Total filings", "Org-years affected" ) )
        cat( "  ", strrep( "-", 36 ), "\n", sep = "" )
        for ( k in sort( as.integer( names(dup_dist) ) ) ) {
          cat( sprintf( "  %-16s  %s\n",
                        k,
                        format( dup_dist[ as.character(k) ], big.mark = "," ) ) )
        }
      }
    }

    cat( "\n" )
    cat( sprintf( "  Final:     %s rows | %s unique org-years\n",
                  format( n_end,   big.mark = "," ),
                  format( pairs_end, big.mark = "," ) ) )
    cat( "============================\n\n" )
  }

  df
}
