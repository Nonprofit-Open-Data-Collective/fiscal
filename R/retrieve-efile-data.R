###---------------------------------------------------
###   NTEE CODE UTILITIES
###---------------------------------------------------
#  (sourced from the NCCS Nonprofit Open Data Collective)

#' Normalize legacy NTEE codes
#'
#' Converts raw NTEE codes into a normalized 3-character form used by all
#' downstream classification functions. Handles padding, blank codes, specialty
#' nonprofits (digits 2-3 of 01-19), and 5-character legacy codes.
#'
#' @param x Character vector of raw NTEE codes.
#' @return A character vector of normalized 3-character NTEE codes.
#' @examples
#' get_clean_ntee( c("B29", "B0129", "B8443", "E0521", "", "A115") )
#' @export
get_clean_ntee <- function( x ) {
  x <- toupper( trimws(x) )
  x[ is.na(x) | x == "" ] <- "Z99"
  x[ nchar(x) == 4 ] <- stringr::str_pad( x[ nchar(x) == 4 ], width = 5, side = "right", pad = "0" )
  letter    <- substr( x, 1, 1 )
  first_two <- substr( x, 2, 3 )
  last_two  <- substr( x, 4, 5 )
  first_two_num <- suppressWarnings( as.numeric(first_two) )
  first_two_num[ is.na(first_two_num) ] <- 99
  ntee   <- substr( x, 1, 3 )
  ntee45 <- paste0( letter, last_two )
  ntee00 <- paste0( letter, "00" )
  specialty3 <- first_two_num <= 19 & nchar(x) == 3
  specialty5 <- first_two_num <= 19 & nchar(x) == 5
  ntee[ specialty3 ] <- ntee00[ specialty3 ]
  ntee[ specialty5 ] <- ntee45[ specialty5 ]
  ntee
}

#' Derive top-level NTEE industry classification
#'
#' Maps raw NTEE codes to their 3-letter high-level industry group.
#' Special overrides: B40-B43/B50 → UNI (Universities), E20-E24 → HOS (Hospitals).
#'
#' @param x Character vector of NTEE codes (raw or cleaned).
#' @return Character vector of 3-letter industry codes.
#' @examples
#' get_industry( c("B29", "B8443", "E0521", "Q12", "Z99") )
#' @export
get_industry <- function( x ) {
  x <- toupper( trimws(x) )
  x[ is.na(x) | x == "" ] <- "Z99"
  x[ nchar(x) == 4 ] <- stringr::str_pad( x[ nchar(x) == 4 ], width = 5, side = "right", pad = "0" )
  letter   <- substr( x, 1, 1 )
  last_two <- substr( x, 4, 5 )
  ntee_ind <- substr( x, 1, 3 )
  ntee_ind[ nchar(x) == 5 ] <- paste0( letter[ nchar(x) == 5 ], last_two[ nchar(x) == 5 ] )
  dplyr::case_when(
    ntee_ind %in% c("B40","B41","B42","B43","B50")  ~ "UNI",
    ntee_ind %in% c("E20","E21","E22","E24")         ~ "HOS",
    substr(ntee_ind,1,1) == "A"                      ~ "ART",
    substr(ntee_ind,1,1) == "B"                      ~ "EDU",
    substr(ntee_ind,1,1) %in% c("C","D")             ~ "ENV",
    substr(ntee_ind,1,1) %in% c("E","F","G","H")     ~ "HEL",
    substr(ntee_ind,1,1) %in% LETTERS[9:16]          ~ "HMS",
    substr(ntee_ind,1,1) == "Q"                      ~ "IFA",
    substr(ntee_ind,1,1) %in% c("R","S","T","U","V","W") ~ "PSB",
    substr(ntee_ind,1,1) == "X"                      ~ "REL",
    substr(ntee_ind,1,1) == "Y"                      ~ "MMB",
    TRUE                                              ~ "UNU"
  )
}

#' Classify nonprofit organization type from NTEE code
#'
#' Determines the functional type of the organization (regular, advocacy,
#' research, funding, etc.) based on digits 2-3 of the NTEE code.
#'
#' @param x Character vector of NTEE codes.
#' @return Character vector of organization type codes:
#'   AA (alliance), MT (management/technical), PA (planning/advocacy),
#'   RP (research), MS (single-org support), MM (multi-org support),
#'   NS (general nonprofit support), RG (regular program organization).
#' @examples
#' get_org_type( c("B01", "B02", "B05", "B11", "B29") )
#' @export
get_org_type <- function( x ) {
  x <- toupper( trimws(x) )
  x[ x == "" | is.na(x) ] <- "Z99"
  two <- suppressWarnings( as.numeric( substr(x, 2, 3) ) )
  dplyr::case_when(
    two == 1  ~ "AA",
    two == 2  ~ "MT",
    two == 3  ~ "PA",
    two == 5  ~ "RP",
    two == 11 ~ "MS",
    two == 12 ~ "MM",
    two == 19 ~ "NS",
    TRUE      ~ "RG"
  )
}

#' Generate full NTEE v2 code
#'
#' Combines the normalized NTEE code, industry group, and organization type
#' into the three-part structure: `[INDUSTRY]-[NTEE]-[ORGTYPE]`.
#'
#' @param x Character vector of raw NTEE codes.
#' @return Character vector of NTEE v2 codes (e.g., `"EDU-B29-RG"`).
#' @examples
#' get_nteev2( c("B29", "B8443", "E0521", "B01", "B0129") )
#' @export
get_nteev2 <- function( x ) {
  paste0( get_industry(x), "-", get_clean_ntee(x), "-", get_org_type(x) )
}

#' Convert NTEE V2 codes to NTMAJ12 major group labels
#'
#' @param nteev2 Character vector of NTEE V2 codes.
#' @return Factor vector with 12 levels (ARTS, EDUCATION, HIGHER ED, etc.).
#' @examples
#' get_ntmaj12( c("HOS-E05-RG", "EDU-B29-RG") )
#' @export
get_ntmaj12 <- function( nteev2 ) {
  ind <- c(
    "ART" = "ARTS {ART}",          "EDU" = "EDUCATION {EDU}",
    "UNI" = "HIGHER ED {UNI}",     "ENV" = "ENVIRONMENT {ENV}",
    "HEL" = "HEALTH {HEL}",        "HOS" = "HOSPITALS {HOS}",
    "IFA" = "INTERNATIONAL {IFA}", "PSB" = "PUBLIC BENEFIT  {PSB}",
    "MMB" = "MEMBERSHIP {MMB}",    "UNU" = "UNCLASSIFIED {UNU}",
    "REL" = "RELIGION {REL}",      "HMS" = "HUMAN SERVICES {HMS}"
  )
  factor( substr(nteev2, 1, 3), levels = names(ind), labels = ind )
}


###---------------------------------------------------
###   RETRIEVE EFILE DATA
###---------------------------------------------------

#' Retrieve and merge IRS 990 efile tables from the NCCS data server
#'
#' @description
#' Downloads one or more IRS 990 efile tables for a given tax year directly
#' from the NCCS public S3 bucket using `data.table::fread()`, merges
#' them with keyed `data.table` joins, optionally attaches BMF
#' organizational metadata, and returns a single `data.frame`.
#'
#' @param year Integer. The tax year to retrieve (e.g., `2021`).
#' @param tables Character vector of table codes to retrieve. Defaults to
#'   `c("P00","P01","P08","P09","P10")`, which correspond to:
#'
#'     - `P00` - Return header (filing metadata)
#'     - `P01` - Part I summary (revenue, expenses, net assets)
#'     - `P08` - Part VIII revenue detail (990 filers only)
#'     - `P09` - Part IX expense detail (990 filers only)
#'     - `P10` - Part X balance sheet (990 filers only)
#'
#' @param include_bmf Logical. If `TRUE` (default), downloads the NCCS
#'   Unified BMF and left-joins organizational metadata (NTEE codes, census
#'   fields, recent financial summaries) onto the efile data by `EIN2`.
#' @param bmf_vars Character vector of BMF variables to retain when
#'   `include_bmf = TRUE`. Defaults to the internal package constant returned by
#'   `get_bmf_vars()`. Use `NULL` to retain all available BMF fields.
#' @param efile_root Character. Base URL for the NCCS efile S3 bucket. Defaults
#'   to `"https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"`.
#'   Update this when NCCS releases a new version of the dataset.
#' @param bmf_url Character. Full URL for the Unified BMF CSV. Defaults to
#'   `"https://nccsdata.s3.us-east-1.amazonaws.com/bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv"`.
#' @param timeout Integer. HTTP timeout in seconds for each file download.
#'   Defaults to `300` (5 minutes). R's built-in default of 60 seconds
#'   is too short for the BMF (~200 MB) on slow connections. The original
#'   timeout value is restored after the function exits.
#' @param retry_max Integer. Maximum number of attempts for each individual
#'   table download. Default `3`. On each failed attempt a short random
#'   back-off (`1`–`5` seconds) is observed before retrying, which is usually
#'   sufficient to recover from transient SSL or connection-reset errors. If
#'   all `retry_max` attempts fail the table is skipped and a warning is
#'   issued; the year's data is still returned from whichever tables
#'   succeeded.
#' @param verbose Logical. If `TRUE` (default), prints progress messages
#'   including row/column counts after each download.
#'
#' @return A `data.frame` containing the merged efile tables, with BMF
#'   fields appended as additional columns if `include_bmf = TRUE`.
#'
#' @details
#' ## Why `data.table` rather than base R or an external database:
#' `data.table::fread()` streams CSV files directly from S3 URLs into
#' memory-efficient column-oriented storage without requiring a local copy.
#' Before each merge, `data.table::setkey()` builds an index on the join
#' column in-place (no copy), so the subsequent `merge.data.table()` call
#' uses a binary-search join rather than a hash-sort copy. Each right-hand
#' table is removed from the environment immediately after joining to free RAM.
#' Together these three choices - streaming reads, keyed joins, and eager
#' cleanup - give substantially lower peak memory usage than `base::merge()`
#' while adding no external dependencies beyond `data.table`.
#'
#' ## Join strategy:
#' Efile tables share two key columns: `OBJECTID` (a unique filing
#' identifier present in every table) and `EIN2` (present in most).
#' Tables are merged sequentially as full outer joins on all shared columns,
#' so organizations that filed only one of the component forms (e.g., 990EZ
#' filers who have P00 and P01 data but no P08/P09/P10) are retained with
#' `NA` in the columns from tables they did not file.
#'
#' ## Table-level retry logic:
#' Each table download is attempted up to `retry_max` times. Failures are
#' caught individually so a single flaky table (e.g. a transient SSL error
#' on P09) does not abort the other four. Failed tables are retried with a
#' random 1–5 second back-off between attempts. If a table still fails after
#' all attempts it is skipped entirely: the returned data frame will be
#' missing its columns but will contain full data from all successful tables.
#'
#' ## BMF processing:
#' The BMF is deduplicated to one row per EIN (most recent `ORG_RULING_DATE`
#' retained). NTEE codes are normalized via the NCCS NTEE v2 system
#' ([get_clean_ntee()], [get_nteev2()]), and a curated
#' subset of fields is retained before the left-join onto the efile data.
#'
#' @examples
#' \dontrun{
#' # Default: 2021 efile (P00 + P01 + P08 + P09 + P10) with BMF
#' df <- retrieve_efile_data( year = 2021 )
#'
#' # Header and Part I only, no BMF
#' df <- retrieve_efile_data( year = 2021,
#'                             tables      = c("P00", "P01"),
#'                             include_bmf = FALSE )
#'
#' # Retrieve a different year
#' df <- retrieve_efile_data( year = 2019 )
#'
#' # Point to a newer efile version when released
#' df <- retrieve_efile_data( year = 2022,
#'   efile_root = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v3_0/" )
#' }
#'
#' @importFrom data.table fread setDT setkey
#' @export
retrieve_efile_data <- function(
  year,
  tables = c("P00", "P01", "P08", "P09", "P10"),
  include_bmf = TRUE,
  bmf_vars = .BMF_VARS,
  efile_root = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/",
  bmf_url = "https://nccsdata.s3.us-east-1.amazonaws.com/bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv",
  timeout = 300,
  retry_max = 3L,
  path = "efdata",
  keep_files = TRUE,
  verbose = TRUE
) {
  if (!is.numeric(year) || length(year) != 1) {
    stop("`year` must be a single integer, e.g. 2021.")
  }
  year <- as.integer(year)

  if (!is.numeric(retry_max) || length(retry_max) != 1 || retry_max < 1) {
    stop("`retry_max` must be a positive integer.")
  }
  retry_max <- as.integer(retry_max)

  if (!is.character(path) || length(path) != 1 || is.na(path) || nchar(path) == 0) {
    stop("`path` must be a single non-empty character string.")
  }

  if (!is.logical(keep_files) || length(keep_files) != 1 || is.na(keep_files)) {
    stop("`keep_files` must be TRUE or FALSE.")
  }

  if (!is.null(bmf_vars) && !is.character(bmf_vars)) {
    stop("`bmf_vars` must be NULL or a character vector.")
  }

  table_map <- c(
    P00 = "F9-P00-T00-HEADER",
    P01 = "F9-P01-T00-SUMMARY",
    P08 = "F9-P08-T00-REVENUE",
    P09 = "F9-P09-T00-EXPENSES",
    P10 = "F9-P10-T00-BALANCE-SHEET"
  )

  unknown <- setdiff(toupper(tables), names(table_map))
  if (length(unknown) > 0) {
    stop(
      "Unknown table code(s): ", paste(unknown, collapse = ", "),
      ". Valid codes are: ", paste(names(table_map), collapse = ", ")
    )
  }
  tables <- toupper(tables)

  base_dir <- path
  year_dir <- file.path(base_dir, as.character(year))
  bmf_dir  <- file.path(base_dir, "BMF")

  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(bmf_dir,  recursive = TRUE, showWarnings = FALSE)

  start_time <- Sys.time()
  timestamp  <- format(start_time, "%Y%m%d-%H%M%S")
  log_file   <- file.path(
    year_dir,
    paste0("retrieve_efile_data_", year, "_", timestamp, ".txt")
  )

  call_text <- paste(deparse(match.call()), collapse = "\n")
  log_lines <- character()

  add_log <- function(...) {
    msg <- paste0(...)
    log_lines <<- c(log_lines, msg)
    if (isTRUE(verbose)) message(msg)
    invisible(msg)
  }

  write_log <- function(final = FALSE) {
    lines <- c(
      "============================================================",
      "retrieve_efile_data() receipt",
      "============================================================",
      paste0("Run started: ", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
      if (final) paste0("Run ended:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")) else NULL,
      paste0("Working directory: ", getwd()),
      paste0("Base path: ", normalizePath(base_dir, winslash = "/", mustWork = FALSE)),
      paste0("Year path: ", normalizePath(year_dir, winslash = "/", mustWork = FALSE)),
      paste0("BMF path: ", normalizePath(bmf_dir, winslash = "/", mustWork = FALSE)),
      paste0("keep_files: ", keep_files),
      paste0("include_bmf: ", include_bmf),
      paste0("timeout: ", timeout),
      paste0("retry_max: ", retry_max),
      paste0("efile_root: ", efile_root),
      paste0("bmf_url: ", bmf_url),
      paste0("tables requested: ", paste(tables, collapse = ", ")),
      paste0("bmf_vars: ",
             if (is.null(bmf_vars)) "NULL (keep all available BMF fields)"
             else paste(bmf_vars, collapse = ", ")),
      "",
      "Function call:",
      call_text,
      "",
      "Event log:",
      log_lines,
      ""
    )

    writeLines(lines, con = log_file, useBytes = TRUE)
    invisible(log_file)
  }

  write_log(final = FALSE)

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  download_status <- data.frame(
    code = character(),
    file_name = character(),
    file_path = character(),
    url = character(),
    status = character(),
    attempts = integer(),
    rows = integer(),
    cols = integer(),
    stringsAsFactors = FALSE
  )

  bmf_status <- list(
    used = FALSE,
    source = NA_character_,
    source_type = NA_character_,
    resolved_path = NA_character_,
    status = "not_requested"
  )

  is_url <- function(x) {
    is.character(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      grepl("^https?://", x, ignore.case = TRUE)
  }

  add_download_status <- function(code, file_name, file_path, url, status, attempts, rows, cols) {
    download_status <<- rbind(
      download_status,
      data.frame(
        code = code,
        file_name = file_name,
        file_path = file_path,
        url = url,
        status = status,
        attempts = as.integer(attempts),
        rows = if (is.na(rows)) NA_integer_ else as.integer(rows),
        cols = if (is.na(cols)) NA_integer_ else as.integer(cols),
        stringsAsFactors = FALSE
      )
    )
  }

  .read_efile_table <- function(code) {
    fname <- paste0(table_map[[code]], "-", year, ".CSV")
    url   <- paste0(efile_root, fname)
    dest  <- file.path(year_dir, fname)

    if (file.exists(dest)) {
      add_log("Using existing local file for ", code, ": ", dest)

      result <- tryCatch({
        dt <- data.table::fread(dest, showProgress = FALSE)
        dt <- unique(dt)

        add_log("  Read existing file successfully: ",
                nrow(dt), " rows, ", ncol(dt), " columns.")

        add_download_status(
          code = code,
          file_name = fname,
          file_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
          url = url,
          status = "reused_local",
          attempts = 0L,
          rows = nrow(dt),
          cols = ncol(dt)
        )

        dt
      }, error = function(e) e)

      if (!inherits(result, "error")) {
        return(result)
      }

      add_log("  Existing local file could not be read; will re-download. Error: ",
              conditionMessage(result))
    }

    last_err <- NULL

    for (attempt in seq_len(retry_max)) {
      add_log(
        if (attempt == 1L) {
          paste0("Downloading ", fname, " from ", url)
        } else {
          paste0("Retrying ", fname, " (attempt ", attempt, " of ", retry_max, ") from ", url)
        }
      )

      result <- tryCatch({
        dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

        utils::download.file(
          url = url,
          destfile = dest,
          mode = "wb",
          quiet = TRUE
        )

        dt <- data.table::fread(dest, showProgress = FALSE)
        dt <- unique(dt)

        add_log("  Download successful: ", dest)
        add_log("  Table dimensions: ", nrow(dt), " rows, ", ncol(dt), " columns.")

        add_download_status(
          code = code,
          file_name = fname,
          file_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
          url = url,
          status = "downloaded",
          attempts = attempt,
          rows = nrow(dt),
          cols = ncol(dt)
        )

        dt
      }, error = function(e) e)

      if (!inherits(result, "error")) {
        return(result)
      }

      last_err <- result

      if (attempt < retry_max) {
        wait <- sample(1:5, 1)
        add_log("  Download failed: ", conditionMessage(last_err))
        add_log("  Waiting ", wait, " second(s) before retry.")
        Sys.sleep(wait)
      }
    }

    add_log("FAILED: Table ", code, " (", fname, ") failed after ",
            retry_max, " attempt(s).")
    add_log("  Final error: ", conditionMessage(last_err))

    add_download_status(
      code = code,
      file_name = fname,
      file_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
      url = url,
      status = "failed",
      attempts = retry_max,
      rows = NA_integer_,
      cols = NA_integer_
    )

    warning(
      "Table ", code, " (", fname, ") failed after ", retry_max,
      " attempt(s): ", conditionMessage(last_err),
      " — skipping table.",
      call. = FALSE
    )

    NULL
  }

  .resolve_bmf_file <- function(bmf_source) {
    bmf_status$used   <<- TRUE
    bmf_status$source <<- bmf_source

    if (!is_url(bmf_source)) {
      bmf_status$source_type <<- "local_file"

      if (!file.exists(bmf_source)) {
        stop("`bmf_url` does not appear to be a URL, and local file does not exist: ", bmf_source)
      }

      resolved <- normalizePath(bmf_source, winslash = "/", mustWork = TRUE)
      bmf_status$resolved_path <<- resolved
      bmf_status$status <<- "local_file_supplied"

      add_log("Using local BMF file: ", resolved)
      return(resolved)
    }

    bmf_status$source_type <<- "url"

    bmf_fname <- basename(bmf_source)
    if (!nzchar(bmf_fname)) {
      bmf_fname <- "BMF.csv"
    }

    bmf_dest <- file.path(bmf_dir, bmf_fname)

    if (file.exists(bmf_dest)) {
      resolved <- normalizePath(bmf_dest, winslash = "/", mustWork = TRUE)
      bmf_status$resolved_path <<- resolved
      bmf_status$status <<- "reused_cached_download"

      add_log("Using cached BMF file: ", resolved)
      add_log("  Source URL: ", bmf_source)
      return(resolved)
    }

    last_err <- NULL

    for (attempt in seq_len(retry_max)) {
      add_log(
        if (attempt == 1L) {
          paste0("Downloading BMF from ", bmf_source)
        } else {
          paste0("Retrying BMF download (attempt ", attempt, " of ", retry_max, ") from ", bmf_source)
        }
      )

      result <- tryCatch({
        dir.create(bmf_dir, recursive = TRUE, showWarnings = FALSE)

        utils::download.file(
          url = bmf_source,
          destfile = bmf_dest,
          mode = "wb",
          quiet = TRUE
        )

        normalizePath(bmf_dest, winslash = "/", mustWork = TRUE)
      }, error = function(e) e)

      if (!inherits(result, "error")) {
        bmf_status$resolved_path <<- result
        bmf_status$status <<- "downloaded"
        add_log("  BMF download successful: ", result)
        return(result)
      }

      last_err <- result

      if (attempt < retry_max) {
        wait <- sample(1:5, 1)
        add_log("  BMF download failed: ", conditionMessage(last_err))
        add_log("  Waiting ", wait, " second(s) before retry.")
        Sys.sleep(wait)
      }
    }

    bmf_status$status <<- "failed"
    add_log("FAILED: BMF download failed after ", retry_max, " attempt(s).")
    add_log("  Final error: ", conditionMessage(last_err))

    warning("BMF download failed: ", conditionMessage(last_err), call. = FALSE)
    NULL
  }

  .attach_bmf_with_logging <- function(df, bmf_source, bmf_vars) {
    bmf_file <- .resolve_bmf_file(bmf_source)

    if (is.null(bmf_file)) {
      add_log("BMF was requested but could not be resolved. Returning df unchanged.")
      return(df)
    }

    result <- tryCatch({
      .attach_bmf(
        df = df,
        bmf_url = bmf_file,
        verbose = verbose,
        bmf_vars = bmf_vars
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      bmf_status$status <<- paste0(bmf_status$status, "_attach_failed")
      add_log("BMF attach failed: ", conditionMessage(result))
      warning("BMF attach failed: ", conditionMessage(result), call. = FALSE)
      return(df)
    }

    add_log("BMF successfully attached using: ", bmf_file)
    result
  }

  add_log("Retrieving efile tables for year ", year, " ...")

  table_data <- lapply(tables, .read_efile_table)
  names(table_data) <- tables

  succeeded <- Filter(Negate(is.null), table_data)
  failed_codes <- names(table_data)[vapply(table_data, is.null, logical(1))]

  if (length(succeeded) == 0) {
    add_log("All requested tables failed.")
    write_log(final = TRUE)
    stop("All tables failed to download for year ", year, ".")
  }

  df <- succeeded[[1]]
  data.table::setDT(df)

  if (length(succeeded) > 1) {
    for (right in succeeded[-1]) {
      data.table::setDT(right)
      shared <- intersect(colnames(df), colnames(right))

      if (length(shared) == 0) {
        stop("No shared columns found for merge. Cannot merge downloaded tables.")
      }

      data.table::setkeyv(df, shared)
      data.table::setkeyv(right, shared)

      add_log("Merging on: ", paste(shared, collapse = ", "))
      df <- merge(df, right, all = TRUE)
      rm(right)
    }
  }

  if (length(failed_codes) > 0) {
    add_log("Tables permanently skipped: ", paste(failed_codes, collapse = ", "))
    warning(
      "Year ", year, ": ", length(failed_codes),
      " table(s) permanently skipped: ",
      paste(failed_codes, collapse = ", "),
      ". Their columns will be absent from the result.",
      call. = FALSE
    )
  }

  if (isTRUE(include_bmf)) {
    df <- .attach_bmf_with_logging(df, bmf_source = bmf_url, bmf_vars = bmf_vars)
  } else {
    add_log("BMF attachment not requested.")
  }

  if (!isTRUE(keep_files)) {
    add_log("keep_files = FALSE, removing year-specific downloaded table files.")

    removable <- unique(download_status$file_path[download_status$status %in% c("downloaded", "reused_local")])
    removable <- removable[!is.na(removable)]

    for (fp in removable) {
      if (file.exists(fp)) {
        ok <- tryCatch(file.remove(fp), warning = function(w) FALSE, error = function(e) FALSE)
        add_log("  ", if (isTRUE(ok)) "Deleted: " else "Could not delete: ", fp)
      }
    }

    add_log("BMF files are stored in shared cache folder and are not deleted automatically.")
  } else {
    add_log("keep_files = TRUE, downloaded files retained.")
  }

  add_log("")
  add_log("Download summary:")
  if (nrow(download_status) > 0) {
    for (i in seq_len(nrow(download_status))) {
      add_log(
        "  [", download_status$code[i], "] ",
        download_status$status[i],
        " | file=", download_status$file_name[i],
        " | attempts=", download_status$attempts[i],
        " | rows=", ifelse(is.na(download_status$rows[i]), "NA", download_status$rows[i]),
        " | cols=", ifelse(is.na(download_status$cols[i]), "NA", download_status$cols[i]),
        " | url=", download_status$url[i]
      )
    }
  }

  add_log("")
  add_log("BMF summary:")
  add_log("  source=", ifelse(is.na(bmf_status$source), "NA", bmf_status$source))
  add_log("  source_type=", ifelse(is.na(bmf_status$source_type), "NA", bmf_status$source_type))
  add_log("  status=", bmf_status$status)
  add_log("  resolved_path=", ifelse(is.na(bmf_status$resolved_path), "NA", bmf_status$resolved_path))

  add_log("")
  add_log("Final dataset: ", nrow(df), " rows, ", ncol(df), " columns.")

  write_log(final = TRUE)

  attr(df, "download_log") <- log_file
  attr(df, "download_dir") <- normalizePath(year_dir, winslash = "/", mustWork = FALSE)
  attr(df, "bmf_dir") <- normalizePath(bmf_dir, winslash = "/", mustWork = FALSE)
  attr(df, "download_status") <- download_status
  attr(df, "bmf_status") <- bmf_status

  as.data.frame(df)
}


#==================================================
# load and standardize BMF 
#==================================================
.attach_bmf <- function(
  df,
  bmf_url,
  verbose = TRUE,
  by_df = "EIN2",
  by_bmf = "EIN2",
  deduplicate_bmf = TRUE,
  normalize_ntee = TRUE,
  bmf_vars = .BMF_VARS
) {
  is_url <- function(x) {
    is.character(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      grepl("^https?://", x, ignore.case = TRUE)
  }

  if (!inherits(df, c("data.frame", "data.table"))) {
    stop("`df` must be a data.frame or data.table.")
  }

  if (!is.character(bmf_url) || length(bmf_url) != 1 || is.na(bmf_url) || nchar(bmf_url) == 0) {
    stop("`bmf_url` must be a single non-empty character string giving either a URL or local file path.")
  }

  if (!is.character(by_df) || length(by_df) != 1 || is.na(by_df)) {
    stop("`by_df` must be a single column name in `df`.")
  }

  if (!is.character(by_bmf) || length(by_bmf) != 1 || is.na(by_bmf)) {
    stop("`by_bmf` must be a single column name in the BMF file.")
  }

  if (!is.null(bmf_vars) && !is.character(bmf_vars)) {
    stop("`bmf_vars` must be NULL or a character vector of BMF columns to retain.")
  }

  df <- data.table::as.data.table(data.table::copy(df))

  if (!by_df %in% names(df)) {
    stop("Join column `", by_df, "` not found in `df`.")
  }

  if (isTRUE(verbose)) {
    if (is_url(bmf_url)) {
      message("Reading BMF from URL: ", bmf_url)
    } else {
      message("Reading BMF from local file: ",
              normalizePath(bmf_url, winslash = "/", mustWork = FALSE))
    }
  }

  bmf <- tryCatch({
    if (is_url(bmf_url)) {
      data.table::fread(bmf_url, showProgress = FALSE)
    } else {
      if (!file.exists(bmf_url)) {
        stop("BMF file does not exist: ", bmf_url)
      }
      data.table::fread(bmf_url, showProgress = FALSE)
    }
  }, error = function(e) {
    stop("Failed to read BMF source: ", conditionMessage(e))
  })

  bmf <- data.table::as.data.table(bmf)

  if (!"EIN2" %in% names(df) && "EIN" %in% names(df)) {
    df[, EIN2 := gsub("[^0-9]", "", as.character(EIN))]
  }

  if (!"EIN2" %in% names(bmf) && "EIN" %in% names(bmf)) {
    bmf[, EIN2 := gsub("[^0-9]", "", as.character(EIN))]
  }

  if (!by_df %in% names(df)) {
    stop("Join column `", by_df, "` not found in `df` after standardization.")
  }

  if (!by_bmf %in% names(bmf)) {
    stop("Join column `", by_bmf, "` not found in BMF after standardization.")
  }

  df[,  (by_df)  := trimws(as.character(get(by_df)))]
  bmf[, (by_bmf) := trimws(as.character(get(by_bmf)))]

  if (by_df %in% c("EIN", "EIN2")) {
    df[, (by_df) := gsub("[^0-9]", "", get(by_df))]
  }

  if (by_bmf %in% c("EIN", "EIN2")) {
    bmf[, (by_bmf) := gsub("[^0-9]", "", get(by_bmf))]
  }

  if (isTRUE(normalize_ntee)) {
    if ("NTEE_NCCS" %in% names(bmf) || "NTEE_IRS" %in% names(bmf)) {
      ntee_nccs <- if ("NTEE_NCCS" %in% names(bmf)) as.character(bmf$NTEE_NCCS) else rep(NA_character_, nrow(bmf))
      ntee_irs  <- if ("NTEE_IRS"  %in% names(bmf)) as.character(bmf$NTEE_IRS)  else rep(NA_character_, nrow(bmf))

      ntee <- ntee_nccs
      use_fallback <- is.na(ntee) | trimws(ntee) == ""
      ntee[use_fallback] <- ntee_irs[use_fallback]

      bmf[, NTEE_NCCS := get_clean_ntee(ntee)]
      bmf[, NTEEV2 := get_nteev2(ntee)]
      bmf[, NTMAJ12 := get_industry(ntee)]
      bmf[, NTEE_ORG_TYPE := get_org_type(ntee)]
    }

    if ("ORG_RULING_DATE" %in% names(bmf)) {
      bmf[, ORG_RULING_YEAR := suppressWarnings(
        as.integer(substr(as.character(ORG_RULING_DATE), 1, 4))
      )]
    }
  }

  n_bmf_before <- nrow(bmf)
  bmf <- bmf[!is.na(get(by_bmf)) & nzchar(get(by_bmf))]

  if (isTRUE(verbose) && nrow(bmf) < n_bmf_before) {
    message("Dropped ", n_bmf_before - nrow(bmf),
            " BMF row(s) with missing or blank join keys.")
  }

  if (!is.null(bmf_vars)) {
    bmf_keep <- intersect(unique(c(by_bmf, bmf_vars)), names(bmf))
    bmf <- bmf[, ..bmf_keep]

    if (isTRUE(verbose)) {
      message("Keeping ", length(bmf_keep), " BMF column(s) after filtering.")
    }
  }

  if (isTRUE(deduplicate_bmf)) {
    dup_n <- sum(duplicated(bmf[[by_bmf]]))

    if (dup_n > 0 && isTRUE(verbose)) {
      message("Deduplicating BMF on `", by_bmf, "`: removing ", dup_n, " duplicate row(s).")
    }

    if (dup_n > 0) {
      data.table::setorderv(bmf, by_bmf)
      bmf <- bmf[!duplicated(get(by_bmf))]
    }
  }

  bmf[, .bmf_match_flag__ := TRUE]

  n_df_before <- nrow(df)
  ncol_before <- ncol(df)
  n_bmf_used  <- nrow(bmf)

  result <- merge(
    x = df,
    y = bmf,
    by.x = by_df,
    by.y = by_bmf,
    all.x = TRUE,
    sort = FALSE
  )

  n_matched <- sum(!is.na(result$.bmf_match_flag__))
  result[, .bmf_match_flag__ := NULL]

  if (isTRUE(verbose)) {
    message("BMF merge complete.")
    message("  Input rows:      ", format(n_df_before, big.mark = ","))
    message("  BMF rows used:   ", format(n_bmf_used, big.mark = ","))
    message("  Output rows:     ", format(nrow(result), big.mark = ","))
    message("  Rows matched:    ", format(n_matched, big.mark = ","))
    message("  Match rate:      ", round(100 * n_matched / max(1, n_df_before), 2), "%")
    message("  Columns added:   ", ncol(result) - ncol_before)
  }

  as.data.frame(result)
}