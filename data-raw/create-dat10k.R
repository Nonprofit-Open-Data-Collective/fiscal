###
###  BUILD THE dat10k SAMPLE (TAX YEAR 2023)
###
###  A 10,000-row random sample of deduplicated 2023 Form 990 and 990-EZ
###  e-filings with Business Master File (BMF) classification fields. The
###  sample carries every efile field that the get_*() metric functions use
###  by default, so compute_all( dat10k ) runs the full battery offline.
###
###  Run from the package root:  source( "data-raw/create-dat10k.R" )
###  The download (about 1.4 GB) is cached under tools::R_user_dir().
###

pkgload::load_all( "." )

YEAR      <- 2023
N_SAMPLE  <- 10000
cache_dir <- file.path( tools::R_user_dir( "fiscal", "cache" ), "efile" )
dir.create( cache_dir, recursive = TRUE, showWarnings = FALSE )


###
###  RETRIEVE AND DEDUPLICATE
###

raw_file   <- file.path( cache_dir, paste0( "efile-", YEAR, "-bmf.rds" ) )
dedup_file <- file.path( cache_dir, paste0( "efile-", YEAR, "-bmf-dedup.rds" ) )

if ( file.exists( dedup_file ) ) {
  efile <- readRDS( dedup_file )
} else {
  if ( file.exists( raw_file ) ) {
    efile <- readRDS( raw_file )
  } else {
    efile <- retrieve_efile_data( year = YEAR, include_bmf = TRUE,
                                  path = file.path( cache_dir, "efdata" ),
                                  timeout = 3600 )
    saveRDS( efile, raw_file )
  }
  # one filing per organization-year: drops group and partial-year returns
  # where a complete return exists and keeps the most recent submission
  efile <- deduplicate( efile )
  saveRDS( efile, dedup_file )
}


###
###  FIELDS TO KEEP
###

# identifiers, filing metadata, and BMF classification / geography
v_meta <- c(
  "EIN2", "OBJECTID", "ORG_EIN", "ORG_NAME_L1", "ORG_NAME_L2",
  "RETURN_AMENDED_X", "RETURN_GROUP_X", "RETURN_PARTIAL_X", "RETURN_TAXPER_DAYS",
  "RETURN_TIME_STAMP", "RETURN_TYPE", "TAX_PERIOD_BEGIN_DATE",
  "TAX_PERIOD_END_DATE", "TAX_YEAR", "URL", "VERSION",
  "F9_00_EXEMPT_STAT_501C_X", "F9_00_EXEMPT_STAT_501C3_X",
  "F9_00_EXEMPT_STAT_4947A1_X", "F9_00_TYPE_ORG_CORP_X",
  "F9_00_TYPE_ORG_TRUST_X", "F9_00_TYPE_ORG_ASSOC_X", "F9_00_TYPE_ORG_OTH_X",
  "F9_00_YEAR_FORMATION",
  # BMF fields as returned by retrieve_efile_data( include_bmf = TRUE )
  "ntee_code_clean", "ntee_code_major_group", "nteev2", "nteev2_subsector",
  "nteev2_org_type", "subsection_code", "foundation_code", "ruling_year",
  "filing_requirement_code", "asset_amount", "income_amount", "revenue_amount",
  "geo_state_abbr", "geo_county", "geo_metro_area", "bmf_vintage_ym"
)

# every efile field referenced by a get_*() default argument
v_metrics <- sort( unique( unlist( lapply( fiscal_metrics()$function_name, function( fn ) {
  unlist( lapply( formals( get( fn ) ), function( a ) {
    v <- tryCatch( eval( a ), error = function( e ) NULL )
    if ( is.character( v ) ) v[ grepl( "^F9_", v ) ] else NULL
  } ) )
} ) ) ) )

# balance-sheet detail for filers that do not follow SFAS 117 (Part X
# lines 30-32) plus the SFAS 117 check boxes and BOY net assets
v_extra <- c(
  "F9_10_NAFB_FOLLOW_SFAS117_X", "F9_10_NAFB_NO_FOLLOW_SFAS117_X",
  "F9_10_NAFB_CAP_STCK_EOY", "F9_10_NAFB_CAP_SURPLUS_EOY",
  "F9_10_NAFB_EARNING_RETAINED_EOY", "F9_10_NAFB_UNRESTRICT_BOY",
  "F9_10_NAFB_TOT_BOY"
)

keep    <- unique( c( v_meta, v_metrics, v_extra ) )
missing <- setdiff( keep, names( efile ) )
if ( length( missing ) > 0 )
  message( "Not present in the ", YEAR, " efile tables (skipped): ",
           paste( missing, collapse = ", " ) )
keep <- intersect( keep, names( efile ) )


###
###  SAMPLE
###

set.seed( 2023 )

# A usable NTEE code: letter A-Y, digit, then digit or letter. The BMF marks
# organizations without one as "UNDEFINED", "INVALID", or the "Z99"
# placeholder, and all of those pass a simple non-blank test on nteev2.
is_usable_ntee <- function( x ) !is.na( x ) & grepl( "^[A-Y][0-9][0-9A-Z]$", x )

pool <- efile[ efile$RETURN_TYPE %in% c( "990", "990EZ" ) &
                 is_usable_ntee( efile$ntee_code_clean ) &
                 suppressWarnings( as.numeric( efile$F9_01_REV_TOT_CY ) ) >= 0 &
                 !is.na( efile$F9_01_REV_TOT_CY ), ]

dat10k <- pool[ sample( nrow( pool ), N_SAMPLE ), keep ]
num_vars <- c( v_metrics, v_extra )
num_vars <- intersect( keep, num_vars[ !grepl( "_X$", num_vars ) ] )
dat10k   <- coerce_numeric( dat10k, vars = num_vars )
dat10k <- as.data.frame( dat10k )
# data.table IDate columns -> base Date so the data need no extra packages
is_idate <- vapply( dat10k, inherits, logical( 1 ), what = "IDate" )
dat10k[ is_idate ] <- lapply( dat10k[ is_idate ], function( x ) as.Date( unclass( x ), origin = "1970-01-01" ) )
rownames( dat10k ) <- NULL

# Guard: no integer64 columns, classed or class-stripped, may reach the package
# (they read as tiny doubles, and negatives as NaN, wherever the class is lost),
# and every row must carry a usable NTEE code.
dat10k <- repair_integer64_columns( dat10k )
stopifnot(
  !any( vapply( dat10k, inherits, logical( 1 ), "integer64" ) ),
  !any( vapply( dat10k, is_unclassed_integer64, logical( 1 ) ) ),
  median( dat10k$F9_10_ASSET_TOT_EOY, na.rm = TRUE ) > 1000,
  all( is_usable_ntee( dat10k$ntee_code_clean ) )
)

usethis::use_data( dat10k, overwrite = TRUE, compress = "xz" )
