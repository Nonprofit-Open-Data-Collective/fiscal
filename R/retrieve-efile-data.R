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
#' @param efile_root Character. Base URL for the NCCS efile S3 bucket. Defaults
#'   to `"https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"`.
#'   Update this when NCCS releases a new version of the dataset.
#' @param bmf_url Character. Full URL for the Unified BMF CSV. Defaults to
#'   `"https://nccsdata.s3.us-east-1.amazonaws.com/bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv"`.
#' @param timeout Integer. HTTP timeout in seconds for each file download.
#'   Defaults to `300` (5 minutes). R's built-in default of 60 seconds
#'   is too short for the BMF (~200 MB) on slow connections. The original
#'   timeout value is restored after the function exits.
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
    tables      = c("P00","P01","P08","P09","P10"),
    include_bmf = TRUE,
    efile_root  = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/",
    bmf_url     = "https://nccsdata.s3.us-east-1.amazonaws.com/bmf/unified/v1.2/UNIFIED_BMF_V1.2.csv",
    timeout     = 300,
    verbose     = TRUE
) {

  # ---- Input validation ----
  if ( !is.numeric(year) || length(year) != 1 )
    stop( "`year` must be a single integer, e.g. 2021." )
  year <- as.integer(year)

  table_map <- c(
    "P00" = "F9-P00-T00-HEADER",
    "P01" = "F9-P01-T00-SUMMARY",
    "P08" = "F9-P08-T00-REVENUE",
    "P09" = "F9-P09-T00-EXPENSES",
    "P10" = "F9-P10-T00-BALANCE-SHEET"
  )

  unknown <- setdiff( toupper(tables), names(table_map) )
  if ( length(unknown) > 0 )
    stop( "Unknown table code(s): ", paste(unknown, collapse = ", "),
          ". Valid codes are: ", paste(names(table_map), collapse = ", ") )

  tables <- toupper(tables)

  # ---- Raise timeout; restore on exit ----
  old_timeout <- getOption("timeout")
  on.exit( options(timeout = old_timeout), add = TRUE )
  options(timeout = timeout)

  # ---- Helper: stream one efile table from URL ----
  .read_efile_table <- function( code ) {
    fname <- paste0( table_map[[code]], "-", year, ".CSV" )
    url   <- paste0( efile_root, fname )
    if (verbose) message( "  Downloading ", fname, " ..." )
    dt <- data.table::fread( url, showProgress = FALSE )
    dt <- unique(dt)
    if (verbose) message( "    ", nrow(dt), " rows, ", ncol(dt), " columns." )
    dt
  }

  # ---- Download all requested tables ----
  if (verbose) message( "Retrieving efile tables for year ", year, " ..." )
  df <- .read_efile_table( tables[1] )
  data.table::setDT(df)

  # Merge remaining tables sequentially, keying on shared columns each time,
  # then remove the right-hand table immediately to free RAM.
  for ( code in tables[-1] ) {
    right <- .read_efile_table(code)
    data.table::setDT(right)

    # Key both tables on their shared columns before merging
    shared <- intersect( colnames(df), colnames(right) )
    data.table::setkeyv( df,    shared )
    data.table::setkeyv( right, shared )

    if (verbose) message( "  Merging ", code, " on: ", paste(shared, collapse = ", ") )
    df <- merge( df, right, all = TRUE )
    rm(right)
  }

  # ---- Optionally attach BMF ----
  if ( include_bmf ) {
    df <- .attach_bmf( df, bmf_url = bmf_url, verbose = verbose )
  }

  if (verbose) message( "\nDone. Final dataset: ",
                        nrow(df), " rows, ", ncol(df), " columns." )

  return( as.data.frame(df) )
}


# ---- Internal helper: download, clean, and left-join BMF ----

.attach_bmf <- function( df, bmf_url, verbose ) {

  if (verbose) message( "\nDownloading BMF ..." )

  bmf <- data.table::fread( bmf_url, showProgress = FALSE )
  bmf <- unique(bmf)

  if (verbose) message( "  ", nrow(bmf), " BMF records loaded. Deduplicating by EIN ..." )

  # Keep one row per EIN - most recent ruling date
  data.table::setDT(bmf)
  data.table::setorder( bmf, EIN, -ORG_RULING_DATE )
  bmf <- bmf[ !duplicated(EIN) ]

  # Normalize NTEE codes: prefer NTEE_NCCS, fall back to NTEE_IRS
  ntee <- bmf$NTEE_NCCS
  ntee[ ntee == "" | is.na(ntee) ] <- bmf$NTEE_IRS[ ntee == "" | is.na(ntee) ]

  bmf[ , NTEE_NCCS     := get_clean_ntee(ntee) ]
  bmf[ , NTEEV2        := get_nteev2(ntee)     ]
  bmf[ , NTMAJ12       := get_industry(ntee)   ]
  bmf[ , NTEE_ORG_TYPE := get_org_type(ntee)   ]
  bmf[ , ORG_RULING_YEAR := as.integer( substr(ORG_RULING_DATE, 1, 4) ) ]

  bmf_keep <- intersect(
    c("EIN2",
      "NTEE_NCCS", "NTEEV2", "NTMAJ12", "NTEE_ORG_TYPE",
      "CENSUS_CBSA_FIPS", "CENSUS_CBSA_NAME",
      "CENSUS_BLOCK_FIPS", "CENSUS_URBAN_AREA",
      "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME",
      "BMF_SUBSECTION_CODE", "BMF_FOUNDATION_CODE",
      "ORG_RULING_YEAR",
      "F990_TOTAL_REVENUE_RECENT", "F990_TOTAL_INCOME_RECENT",
      "F990_TOTAL_ASSETS_RECENT",  "F990_TOTAL_EXPENSES_RECENT"),
    colnames(bmf)
  )
  bmf <- bmf[ , ..bmf_keep ]

  # Left-join: keep all efile rows, attach BMF where EIN2 matches
  data.table::setDT(df)
  data.table::setkey( df,  EIN2 )
  data.table::setkey( bmf, EIN2 )

  if (verbose) message( "  Left-joining BMF onto efile data by EIN2 ..." )
  df <- merge( df, bmf, by = "EIN2", all.x = TRUE )

  df
}
