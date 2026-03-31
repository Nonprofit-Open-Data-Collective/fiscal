#' Inspect duplicate filings before deduplication
#'
#' @description
#' Diagnoses duplicated organization-year filings using the same ranking logic
#' as `deduplicate()`. For each duplicated `by_id` x `by_year` group, the
#' function identifies the row that would be retained and classifies each
#' candidate filing by filing type.
#'
#' Filing types are:
#'
#' - `"normal"`: not amended, not group, not partial
#' - `"partial"`: partial only
#' - `"amended"`: amended only
#' - `"group"`: group only
#' - `"amended_partial"`: amended + partial
#' - `"amended_group"`: amended + group
#' - `"group_partial"`: group + partial
#' - `"amended_group_partial"`: amended + group + partial
#'
#' The retained filing is indicated separately by the logical column
#' `retained`.
#'
#' @param df A `data.frame` containing IRS 990 e-file data.
#' @param by_id Name of the organization identifier column. Default `"EIN2"`.
#' @param by_year Name of the tax year column. Default `"TAX_YEAR"`.
#' @param col_group Name of the group return flag column. Default `"RETURN_GROUP_X"`.
#' @param col_partial Name of the partial-year return flag column.
#'   Default `"RETURN_PARTIAL_X"`.
#' @param col_amended Name of the amended return flag column.
#'   Default `"RETURN_AMENDED_X"`.
#' @param col_stamp Name of the filing timestamp column.
#'   Default `"RETURN_TIME_STAMP"`.
#' @param return_candidates Logical. If `TRUE`, return the full duplicate
#'   candidate table with ranking columns. Default is `FALSE`.
#'
#' @return A list with:
#' \describe{
#'   \item{summary_types}{Count of duplicated candidate rows by filing type, with total, kept, and dropped counts.}
#'   \item{summary_sizes}{Count of duplicated org-years by number of filings.}
#'   \item{summary_size_by_type}{Count of duplicated candidate rows by number of filings and filing type, with total, kept, and dropped counts.}
#'   \item{examples}{One representative duplicated org-year for each number of filings, including all candidate rows in that example group.}
#'   \item{candidates}{Optional full duplicate candidate table, returned only if `return_candidates = TRUE`.}
#' }
#'
#' @export
inspect_duplicates <- function(
    df,
    by_id             = "EIN2",
    by_year           = "TAX_YEAR",
    col_group         = "RETURN_GROUP_X",
    col_partial       = "RETURN_PARTIAL_X",
    col_amended       = "RETURN_AMENDED_X",
    col_stamp         = "RETURN_TIME_STAMP",
    return_candidates = FALSE
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( !by_id %in% names( df ) ) {
    stop( paste0( "`by_id` column not found: \"", by_id, "\"" ) )
  }

  if ( !by_year %in% names( df ) ) {
    stop( paste0( "`by_year` column not found: \"", by_year, "\"" ) )
  }

  .is_flagged <- function( x ) {

    if ( is.logical( x ) ) {
      return( !is.na( x ) & x )
    }

    if ( is.numeric( x ) ) {
      return( !is.na( x ) & x == 1 )
    }

    x_chr <- trimws( toupper( as.character( x ) ) )

    !is.na( x_chr ) & x_chr %in% c( "X", "TRUE", "T", "1", "Y", "YES" )
  }

  .parse_stamp <- function( x ) {
    x_chr <- as.character( x )
    x_chr[ is.na( x_chr ) | trimws( x_chr ) == "" ] <- NA_character_
    x_chr <- sub( " UTC$", "", x_chr )

    suppressWarnings(
      as.POSIXct(
        x_chr,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  }

  .classify_filing_type <- function( is_group, is_partial, is_amended ) {

    out <- rep( "normal", length( is_group ) )

    out[ is_amended & is_group & is_partial ] <- "amended_group_partial"
    out[ is_amended & is_group & !is_partial ] <- "amended_group"
    out[ is_amended & !is_group & is_partial ] <- "amended_partial"
    out[ !is_amended & is_group & is_partial ] <- "group_partial"
    out[ is_amended & !is_group & !is_partial ] <- "amended"
    out[ !is_amended & is_group & !is_partial ] <- "group"
    out[ !is_amended & !is_group & is_partial ] <- "partial"

    out
  }

  dt <- data.table::as.data.table( df )
  dt[ , .row_id__ := seq_len( .N ) ]
  dt[ , .id__ := get( by_id ) ]
  dt[ , .year__ := get( by_year ) ]

  dt[ , filings_per_org_year := .N, by = .( .id__, .year__ ) ]

  dt_dup <- dt[ filings_per_org_year > 1L ]

  if ( nrow( dt_dup ) == 0L ) {

    if ( isTRUE( getOption( "inspect_duplicates.verbose", TRUE ) ) ) {
      message(
        "\nNo duplicate filings detected.\n",
        "Each ", by_id, " x ", by_year, " combination appears exactly once.\n"
      )
    }

    out <- list(
      summary_types = data.frame(
        filing_type  = character( 0 ),
        rows_total   = integer( 0 ),
        rows_kept    = integer( 0 ),
        rows_dropped = integer( 0 )
      ),
      summary_sizes = data.frame(
        filings_per_org_year = integer( 0 ),
        n_org_years = integer( 0 )
      ),
      summary_size_by_type = data.frame(
        filings_per_org_year = integer( 0 ),
        filing_type  = character( 0 ),
        rows_total   = integer( 0 ),
        rows_kept    = integer( 0 ),
        rows_dropped = integer( 0 )
      ),
      examples = data.frame()
    )

    attr( out, "has_duplicates" ) <- FALSE

    if ( isTRUE( return_candidates ) ) {
      out$candidates <- data.frame()
    }

    return( out )
  }

  dt_dup[ , score_group := 1L ]
  dt_dup[ , score_partial := 1L ]
  dt_dup[ , score_amended := 0L ]
  dt_dup[ , stamp_num := as.numeric( NA ) ]

  if ( col_group %in% names( dt_dup ) ) {
    dt_dup[ ,
      score_group := data.table::fifelse(
        !.is_flagged( get( col_group ) ),
        1L,
        0L
      )
    ]
  }

  if ( col_partial %in% names( dt_dup ) ) {
    dt_dup[ ,
      score_partial := data.table::fifelse(
        !.is_flagged( get( col_partial ) ),
        1L,
        0L
      )
    ]
  }

  if ( !is.null( col_amended ) && col_amended %in% names( dt_dup ) ) {
    dt_dup[ ,
      score_amended := data.table::fifelse(
        .is_flagged( get( col_amended ) ),
        1L,
        0L
      )
    ]
  }

  if ( col_stamp %in% names( dt_dup ) ) {
    dt_dup[ , stamp_num := as.numeric( .parse_stamp( get( col_stamp ) ) ) ]
  }

  data.table::setorderv(
    dt_dup,
    cols = c(
      ".id__",
      ".year__",
      "score_group",
      "score_partial",
      "score_amended",
      "stamp_num",
      ".row_id__"
    ),
    order = c(
      1L,
      1L,
      -1L,
      -1L,
      -1L,
      -1L,
      1L
    ),
    na.last = TRUE
  )

  dt_dup[ , rank_within_org_year := seq_len( .N ), by = .( .id__, .year__ ) ]
  dt_dup[ , retained := rank_within_org_year == 1L ]

  is_group <- if ( col_group %in% names( dt_dup ) ) {
    .is_flagged( dt_dup[[ col_group ]] )
  } else {
    rep( FALSE, nrow( dt_dup ) )
  }

  is_partial <- if ( col_partial %in% names( dt_dup ) ) {
    .is_flagged( dt_dup[[ col_partial ]] )
  } else {
    rep( FALSE, nrow( dt_dup ) )
  }

  is_amended <- if ( !is.null( col_amended ) && col_amended %in% names( dt_dup ) ) {
    .is_flagged( dt_dup[[ col_amended ]] )
  } else {
    rep( FALSE, nrow( dt_dup ) )
  }

  dt_dup[ ,
    filing_type := .classify_filing_type(
      is_group   = is_group,
      is_partial = is_partial,
      is_amended = is_amended
    )
  ]

  summary_types <- dt_dup[ ,
    .(
      rows_total   = .N,
      rows_kept    = sum( retained ),
      rows_dropped = sum( !retained )
    ),
    by = filing_type
  ][
    order( -rows_total, filing_type )
  ]

  summary_sizes <- unique(
    dt_dup[ , .( .id__, .year__, filings_per_org_year ) ]
  )[ ,
    .( n_org_years = .N ),
    by = filings_per_org_year
  ][
    order( filings_per_org_year )
  ]

  summary_size_by_type <- dt_dup[ ,
    .(
      rows_total   = .N,
      rows_kept    = sum( retained ),
      rows_dropped = sum( !retained )
    ),
    by = .( filings_per_org_year, filing_type )
  ][
    order( filings_per_org_year, filing_type )
  ]

  example_keys <- unique(
    dt_dup[ , .( .id__, .year__, filings_per_org_year ) ]
  )[ ,
    .SD[ 1L ],
    by = filings_per_org_year
  ][
    order( filings_per_org_year )
  ]

  examples <- dt_dup[
    example_keys,
    on = c( ".id__", ".year__", "filings_per_org_year" ),
    nomatch = 0L
  ][
    order( filings_per_org_year, .id__, .year__, rank_within_org_year )
  ]

  examples[ , example_id := paste0( .id__, " / ", .year__ ) ]

  keep_front <- c(
    ".id__", ".year__", "example_id", "filings_per_org_year",
    "rank_within_org_year", "retained", "filing_type",
    "score_group", "score_partial", "score_amended", "stamp_num"
  )

  keep_front <- c(
    keep_front,
    setdiff( names( examples ), keep_front )
  )

  data.table::setcolorder( examples, keep_front )

  names( examples )[ names( examples ) == ".id__" ] <- by_id
  names( examples )[ names( examples ) == ".year__" ] <- by_year

  out <- list(
    summary_types = as.data.frame( summary_types ),
    summary_sizes = as.data.frame( summary_sizes ),
    summary_size_by_type = as.data.frame( summary_size_by_type ),
    examples = as.data.frame( examples )
  )

  if ( isTRUE( return_candidates ) ) {
    candidates <- data.table::copy( dt_dup )
    names( candidates )[ names( candidates ) == ".id__" ] <- by_id
    names( candidates )[ names( candidates ) == ".year__" ] <- by_year
    out$candidates <- as.data.frame( candidates )
  }

  out
}

#' Deduplicate a panel to one filing per organization per year
#'
#' @description
#' IRS 990 e-file panels can contain multiple filings for the same
#' organization-year due to group returns, partial-year filings, amended
#' returns, and repeated submissions. `deduplicate()` reduces the data to
#' exactly one record per `by_id` x `by_year` combination by assigning each
#' candidate filing a priority rank and keeping the best-ranked row within
#' each organization-year.
#'
#' **Priority rules (applied jointly):**
#'
#' 1. Prefer non-group returns over group returns.
#' 2. Prefer non-partial returns over partial-year returns.
#' 3. Prefer amended returns over non-amended returns when an amended flag
#'    column is available.
#' 4. Prefer the most recent filing timestamp when a timestamp column is
#'    available.
#' 5. Break any remaining ties by original row order.
#'
#' This approach guarantees that at least one record is kept for every
#' observed organization-year, even if all available filings are group
#' returns, partial returns, or have missing timestamps.
#'
#' @param df A `data.frame` containing IRS 990 e-file data.
#' @param by_id Name of the organization identifier column. Default `"EIN2"`.
#' @param by_year Name of the tax year column. Default `"TAX_YEAR"`.
#' @param col_group Name of the group return flag column. Default `"RETURN_GROUP_X"`.
#' @param col_partial Name of the partial-year return flag column.
#'   Default `"RETURN_PARTIAL_X"`.
#' @param col_amended Name of the amended return flag column. Default `NULL`.
#'   If `NULL` or not found in `df`, amended-return priority is skipped.
#' @param col_stamp Name of the filing timestamp column.
#'   Default `"RETURN_TIME_STAMP"`.
#' @param verbose Logical. If `TRUE` (default), print a deduplication report.
#'
#' @return A `data.frame` with the same columns as `df` and at most one row
#'   per `by_id` x `by_year` combination.
#'
#' @details
#' The function uses `data.table` internally for speed on large panels.
#'
#' The timestamp column is parsed with `as.POSIXct()`. If parsing fails for
#' some rows, those rows are treated as having missing timestamps and lose the
#' timestamp tie-break unless all competitors are also missing timestamps.
#'
#' If no amended-return column is supplied, the function still prefers the
#' newest timestamp after accounting for group and partial return status.
#'
#' @export
deduplicate <- function(
    df,
    by_id       = "EIN2",
    by_year     = "TAX_YEAR",
    col_group   = "RETURN_GROUP_X",
    col_partial = "RETURN_PARTIAL_X",
    col_amended = "RETURN_AMENDED_X",
    col_stamp   = "RETURN_TIME_STAMP",
    verbose     = TRUE
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( !by_id %in% names( df ) ) {
    stop( paste0( "`by_id` column not found: \"", by_id, "\"" ) )
  }

  if ( !by_year %in% names( df ) ) {
    stop( paste0( "`by_year` column not found: \"", by_year, "\"" ) )
  }
  
  .is_flagged <- function( x ) {
  
    if ( is.logical( x ) ) {
      return( !is.na( x ) & x )
    }
  
    if ( is.numeric( x ) ) {
      return( !is.na( x ) & x == 1 )
    }
  
    x_chr <- trimws( toupper( as.character( x ) ) )
  
    !is.na( x_chr ) & x_chr %in% c( "X", "TRUE", "T", "1", "Y", "YES" )
  }

  .parse_stamp <- function( x ) {
    x_chr <- as.character( x )
    x_chr[ is.na( x_chr ) | trimws( x_chr ) == "" ] <- NA_character_
    x_chr <- sub( " UTC$", "", x_chr )

    suppressWarnings(
      as.POSIXct(
        x_chr,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    )
  }

  dt <- data.table::as.data.table( df )
  dt[ , .row_id__ := seq_len( .N ) ]

  n_start <- nrow( dt )
  pairs_start <- nrow( unique( dt[ , c( by_id, by_year ), with = FALSE ] ) )

  dt[ , score_group   := 1L ]
  dt[ , score_partial := 1L ]
  dt[ , score_amended := 0L ]
  dt[ , stamp_num     := as.numeric( NA ) ]

  if ( col_group %in% names( dt ) ) {
    dt[ ,
      score_group := data.table::fifelse(
        !.is_flagged( get( col_group ) ),
        1L,
        0L
      )
    ]
  } else if ( verbose ) {
    message( "Column '", col_group, "' not found - group-return preference skipped." )
  }

  if ( col_partial %in% names( dt ) ) {
    dt[ ,
      score_partial := data.table::fifelse(
        !.is_flagged( get( col_partial ) ),
        1L,
        0L
      )
    ]
  } else if ( verbose ) {
    message( "Column '", col_partial, "' not found - partial-return preference skipped." )
  }

  if ( !is.null( col_amended ) ) {
    if ( col_amended %in% names( dt ) ) {
      dt[ ,
        score_amended := data.table::fifelse(
          .is_flagged( get( col_amended ) ),
          1L,
          0L
        )
      ]
    } else if ( verbose ) {
      message( "Column '", col_amended, "' not found - amended-return preference skipped." )
    }
  }

  if ( col_stamp %in% names( dt ) ) {
    dt[ , stamp_num := as.numeric( .parse_stamp( get( col_stamp ) ) ) ]
  } else if ( verbose ) {
    message( "Column '", col_stamp, "' not found - timestamp tie-break skipped." )
  }

  dup_dist <- dt[ ,
    .N,
    by = c( by_id, by_year )
  ][
    ,
    .N,
    by = .( filings_per_org_year = N )
  ][
    order( filings_per_org_year )
  ]

  data.table::setorderv(
    dt,
    cols = c(
      by_id,
      by_year,
      "score_group",
      "score_partial",
      "score_amended",
      "stamp_num",
      ".row_id__"
    ),
    order = c(
      1L,
      1L,
      -1L,
      -1L,
      -1L,
      -1L,
      1L
    ),
    na.last = TRUE
  )

  dt_keep <- dt[ , .SD[ 1L ], by = c( by_id, by_year ) ]

  n_end <- nrow( dt_keep )
  pairs_end <- nrow( unique( dt_keep[ , c( by_id, by_year ), with = FALSE ] ) )
  n_dropped <- n_start - n_end

  if ( verbose ) {

    message( "\nDeduplication report" )
    message( "--------------------" )
    message( "Starting rows:           ", format( n_start, big.mark = "," ) )
    message( "Starting org-years:      ", format( pairs_start, big.mark = "," ) )
    message( "Final rows:              ", format( n_end, big.mark = "," ) )
    message( "Final org-years:         ", format( pairs_end, big.mark = "," ) )
    message( "Rows dropped:            ", format( n_dropped, big.mark = "," ) )

    if ( nrow( dup_dist ) > 0L ) {
      message( "\nDuplicate distribution before dedupe:" )
      for ( i in seq_len( nrow( dup_dist ) ) ) {
        message(
          "  ",
          dup_dist$filings_per_org_year[[ i ]], " filing(s): ",
          format( dup_dist$N[[ i ]], big.mark = "," ), " org-year(s)"
        )
      }
    }

    winners_group <- if ( col_group %in% names( dt_keep ) ) {
      sum( .is_flagged( dt_keep[[ col_group ]] ), na.rm = TRUE )
    } else {
      0L
    }

    winners_partial <- if ( col_partial %in% names( dt_keep ) ) {
      sum( .is_flagged( dt_keep[[ col_partial ]] ), na.rm = TRUE )
    } else {
      0L
    }

    winners_amended <- if ( !is.null( col_amended ) && col_amended %in% names( dt_keep ) ) {
      sum( .is_flagged( dt_keep[[ col_amended ]] ), na.rm = TRUE )
    } else {
      NA_integer_
    }

    message( "\nChosen filings among retained rows:" )
    message( "  Group returns kept:    ", format( winners_group, big.mark = "," ) )
    message( "  Partial returns kept:  ", format( winners_partial, big.mark = "," ) )

    if ( !is.na( winners_amended ) ) {
      message( "  Amended returns kept:  ", format( winners_amended, big.mark = "," ) )
    }
  }

  drop_cols <- c(
    ".row_id__",
    "score_group",
    "score_partial",
    "score_amended",
    "stamp_num"
  )

  drop_cols <- intersect( drop_cols, names( dt_keep ) )
  dt_keep[ , ( drop_cols ) := NULL ]

  rownames( dt_keep ) <- NULL
  as.data.frame( dt_keep )
}