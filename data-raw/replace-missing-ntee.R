###
###  REPLACE dat10k ROWS THAT LACK A USABLE NTEE CODE (2026-09)
###
###  create-dat10k.R filtered on NTEE_NCCS after get_clean_ntee() had already
###  turned blank codes into "Z99", so organizations without an NTEE code got
###  into the sample. This script swaps those rows (plus a few malformed codes
###  such as "000", "B6", "HZ0") for new random draws from the same 2021 efile
###  population, keeping every other row and the row order unchanged.
###
###  The Unified BMF v1.2 CSV used by create-dat10k.R is no longer available,
###  so BMF fields for the new rows come from the current geocoded Unified BMF
###  (panel990::bmf_url()). Fields with an equivalent there are filled; the rest
###  are NA for replacement rows:
###    filled: NTEE_NCCS, NTEEV2, NTMAJ12, NTEE_ORG_TYPE (from the IRS NTEE code),
###            BMF_SUBSECTION_CODE, BMF_FOUNDATION_CODE, ORG_RULING_YEAR,
###            CENSUS_STATE_ABBR, CENSUS_COUNTY_NAME
###    NA:     CENSUS_CBSA_FIPS, CENSUS_CBSA_NAME, CENSUS_BLOCK_FIPS,
###            CENSUS_URBAN_AREA, F990_TOTAL_*_RECENT (current BMF amounts are
###            from a 2026 snapshot and would not be comparable)
###

library( data.table )
library( fiscal )

YEAR  <- 2021
cache <- file.path( tools::R_user_dir( "fiscal", "cache" ), "efile", "efdata", YEAR )
root  <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"

tables <- paste0( c( "F9-P00-T00-HEADER-", "F9-P01-T00-SUMMARY-", "F9-P08-T00-REVENUE-",
                     "F9-P09-T00-EXPENSES-", "F9-P10-T00-BALANCE-SHEET-" ), YEAR, ".CSV" )
dir.create( cache, showWarnings = FALSE, recursive = TRUE )
for ( f in tables ) {
  if ( !file.exists( file.path( cache, f ) ) )
    download.file( paste0( root, f ), destfile = file.path( cache, f ), mode = "wb" )
}


# A usable NTEE code: letter, digit, then digit or letter, and not "Z99"
# (the placeholder get_clean_ntee() assigns to blank codes).
is_usable_ntee <- function( x ) {
  !is.na( x ) & grepl( "^[A-Y][0-9][0-9A-Z]$", x )
}


###
###  CURRENT SAMPLE
###

load( "data/dat10k.rda" )
dat10k <- copy( dat10k )   # restore data.table internals after load
bad <- which( !is_usable_ntee( dat10k$NTEE_NCCS ) )
cat( length( bad ), "rows lack a usable NTEE code\n" )
print( table( dat10k$NTEE_NCCS[ bad ] ) )


###
###  CANDIDATE POOL: 2021 efile filings, same columns and filters as dat10k
###

keep_cols <- names( dat10k )

read_table <- function( f ) {
  hdr  <- names( fread( file.path( cache, f ), nrows = 0 ) )
  cols <- intersect( hdr, keep_cols )
  unique( fread( file.path( cache, f ), select = cols, integer64 = "double" ) )
}

d <- lapply( tables, read_table )
df <- Reduce( function( x, y ) merge( x, y, all = TRUE ), d )

bmf_path <- file.path( tools::R_user_dir( "fiscal", "cache" ), "PANEL990", "bmf", "bmf_unified_geocoded.parquet" )
if ( !file.exists( bmf_path ) ) {
  dir.create( dirname( bmf_path ), showWarnings = FALSE, recursive = TRUE )
  download.file( panel990::bmf_url(), destfile = bmf_path, mode = "wb" )
}
bmf <- as.data.table( dplyr::collect( dplyr::select(
  arrow::open_dataset( bmf_path ),
  EIN2, ntee_code_raw, subsection_code, foundation_code, ruling_date,
  geo_state_abbr, geo_county ) ) )
bmf <- bmf[ !duplicated( EIN2 ) ]
bmf[ , NTEE_NCCS := get_clean_ntee( ntee_code_raw ) ]
bmf <- bmf[ !is.na( ntee_code_raw ) & is_usable_ntee( NTEE_NCCS ) ]

bmf[ , `:=`(
  NTEEV2              = get_nteev2( ntee_code_raw ),
  NTMAJ12             = get_industry( ntee_code_raw ),
  NTEE_ORG_TYPE       = get_org_type( ntee_code_raw ),
  BMF_SUBSECTION_CODE = as.integer( subsection_code ),
  BMF_FOUNDATION_CODE = as.integer( foundation_code ),
  ORG_RULING_YEAR     = as.integer( substr( ruling_date, 1, 4 ) ),
  CENSUS_STATE_ABBR   = geo_state_abbr,
  CENSUS_COUNTY_NAME  = geo_county
) ]
bmf_cols <- c( "EIN2", "NTEE_NCCS", "NTEEV2", "NTMAJ12", "NTEE_ORG_TYPE",
               "BMF_SUBSECTION_CODE", "BMF_FOUNDATION_CODE", "ORG_RULING_YEAR",
               "CENSUS_STATE_ABBR", "CENSUS_COUNTY_NAME" )

pool <- merge( df[ , setdiff( names( df ), bmf_cols[ -1 ] ), with = FALSE ],
               bmf[ , ..bmf_cols ], by = "EIN2" )
pool <- pool[ !is.na( F9_01_REV_TOT_CY ) & F9_01_REV_TOT_CY >= 0 ]
pool <- pool[ !( EIN2 %in% dat10k$EIN2 ) & !( OBJECTID %in% dat10k$OBJECTID ) ]
cat( "candidate pool:", nrow( pool ), "filings\n" )


###
###  DRAW REPLACEMENTS AND SWAP THEM IN
###

set.seed( 20260916 )
new <- pool[ sample( .N, length( bad ) ) ]

# Columns with no equivalent in the current BMF
for ( v in setdiff( keep_cols, names( new ) ) ) set( new, j = v, value = NA )
new <- new[ , ..keep_cols ]

# Match dat10k's column classes; widen integer columns if a new value overflows
for ( v in keep_cols ) {
  target <- dat10k[[ v ]]
  x      <- new[[ v ]]
  if ( is.integer( target ) ) {
    if ( any( abs( as.numeric( x ) ) > .Machine$integer.max, na.rm = TRUE ) ) {
      set( dat10k, j = v, value = as.numeric( target ) )
      x <- as.numeric( x )
    } else {
      x <- as.integer( x )
    }
  } else if ( inherits( target, "IDate" ) ) {
    x <- as.IDate( x )
  } else if ( inherits( target, "POSIXct" ) ) {
    x <- as.POSIXct( x, tz = attr( target, "tzone" ) %||% "UTC" )
  } else if ( is.logical( target ) ) {
    x <- as.logical( x )
  } else if ( is.double( target ) ) {
    x <- as.numeric( x )
  } else if ( is.character( target ) ) {
    x <- as.character( x )
  }
  set( new, j = v, value = x )
}

for ( v in keep_cols ) set( dat10k, i = bad, j = v, value = new[[ v ]] )
# The saved file carried a data.table key on EIN2, but the rows were never in
# EIN2 order. Keep the row order and drop the stale key.
setkey( dat10k, NULL )

stopifnot(
  nrow( dat10k ) == 10000L,
  all( is_usable_ntee( dat10k$NTEE_NCCS ) ),
  !anyDuplicated( dat10k$OBJECTID ),
  !any( vapply( dat10k, inherits, logical(1), "integer64" ) ),
  median( dat10k$F9_10_ASSET_TOT_EOY, na.rm = TRUE ) > 1000
)

save( dat10k, file = "data/dat10k.rda", compress = "xz", version = 3 )
