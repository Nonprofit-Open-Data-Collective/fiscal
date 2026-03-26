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
#'   EINs present in the assembled panel before joining - keeping memory use
#'   proportional to the panel rather than to the full BMF.
#' @param efile_root Base URL for the NCCS efile S3 bucket. Passed
#'   directly to [retrieve_efile_data()].
#' @param bmf_url Full URL for the Unified BMF CSV. Passed directly to
#'   [retrieve_efile_data()].
#' @param timeout HTTP timeout in seconds for each file download. Default
#'   `300`.
#' @param retry_max Integer. Maximum download attempts per efile table.
#'   Default `3`. Passed directly to [retrieve_efile_data()]. A failed
#'   table is retried with a 1-5 second random back-off between attempts;
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

    if ( verbose ) {
      message( "\n=== Year ", yr, " (", i, " of ", length(years), ") ===" )
    }

    df_yr <- tryCatch(
      retrieve_efile_data(
        year        = yr,
        tables      = tables,
        include_bmf = FALSE,
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
      df_yr[["TAX_YEAR"]] <- yr
      year_list[[i]] <- df_yr
    }
  }

  # Drop failed years
  year_list <- Filter( Negate(is.null), year_list )

  if ( length(year_list) == 0 ) {
    stop( "All years failed to download. Check your internet connection and the `efile_root` URL." )
  }

  if ( verbose ) {
    message( "\nStacking ", length(year_list), " year(s) ..." )
  }

  out <- dplyr::bind_rows( year_list )

  if ( verbose ) {
    message(
      "Stacked: ", nrow(out), " rows x ", ncol(out), " columns (",
      length(years), " year(s) requested, ",
      length(year_list), " retrieved)."
    )
  }

  # ---- attach BMF once ----
  if ( include_bmf ) {

    if ( verbose ) message( "\nDownloading BMF (once for all years) ..." )

    old_timeout <- getOption( "timeout" )
    on.exit( options( timeout = old_timeout ), add = TRUE )
    options( timeout = timeout )

    bmf_raw <- tryCatch(
      data.table::fread( bmf_url, showProgress = FALSE ),
      error = function(e) {
        warning(
          "BMF download failed: ", conditionMessage(e),
          " - returning panel without BMF.",
          call. = FALSE
        )
        NULL
      }
    )

    if ( !is.null(bmf_raw) ) {

      panel_eins <- unique( out[["EIN2"]] )
      panel_eins <- panel_eins[ !is.na(panel_eins) ]

      if ( verbose ) {
        message(
          "  Filtering BMF to ",
          format( length(panel_eins), big.mark = "," ),
          " unique EIN(s) in panel ..."
        )
      }

      data.table::setDT( bmf_raw )
      bmf_raw <- bmf_raw[ EIN2 %in% panel_eins ]

      if ( verbose ) {
        message(
          "  Matched ",
          format( nrow(bmf_raw), big.mark = "," ),
          " BMF record(s)."
        )
      }

      data.table::setDT( out )
      out <- .attach_bmf_filtered(
        out,
        bmf_raw = bmf_raw,
        verbose = verbose
      )
    }
  }

  if ( verbose ) {
    message(
      "\nDone. Panel: ",
      nrow(out), " rows x ", ncol(out), " columns."
    )
  }

  as.data.frame( out )
}


# ---- internal helper: attach pre-filtered BMF to panel ----
# Mirrors .attach_bmf() in retrieve-efile-data.R but accepts an already-
# filtered bmf data.table rather than downloading from a URL.

.attach_bmf_filtered <- function( df, bmf_raw, verbose ) {

  bmf <- data.table::copy( bmf_raw )
  bmf <- unique( bmf )   # drop exact duplicate rows before deduplication by EIN

  if (verbose) message( "  Deduplicating BMF by EIN ..." )

  # Keep one row per EIN - most recent ruling date
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
