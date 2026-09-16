# test-integer64-integrity.R
# Guards against bit64 integer64 storage leaking into package data or being
# misread as doubles. A class-stripped integer64 column reads as subnormal
# doubles (|x| < 1e-300), negatives as NaN, and NA_integer64_ as -0.

library( fiscal )


# ---- bundled dat10k ------------------------------------------------------

test_that( "dat10k has no integer64 columns, classed or class-stripped", {
  expect_false( any( vapply( dat10k, inherits, logical(1), "integer64" ) ) )

  stripped <- names( dat10k )[ vapply( dat10k, fiscal:::is_unclassed_integer64, logical(1) ) ]
  expect_equal( stripped, character(0) )
})

test_that( "no numeric dat10k column has only tiny nonzero values", {
  num <- names( dat10k )[ vapply( dat10k, is.numeric, logical(1) ) ]
  all_tiny <- vapply( num, function( v ) {
    x  <- as.vector( dat10k[[ v ]] )
    nz <- x[ !is.na( x ) & x != 0 ]
    length( nz ) > 0L && all( abs( nz ) < 1e-300 )
  }, logical(1) )
  expect_equal( num[ all_tiny ], character(0) )
})

test_that( "dat10k financial columns have plausible dollar magnitudes", {
  med <- function( v ) stats::median( dat10k[[ v ]], na.rm = TRUE )

  expect_gt( med( "F9_10_ASSET_TOT_EOY" ), 1000 )
  expect_gt( med( "F9_10_NAFB_TOT_EOY" ),  1000 )
  expect_gt( med( "F9_09_EXP_TOT_TOT" ),   1000 )
  expect_gt( med( "F9_08_REV_TOT_TOT" ),   1000 )
  expect_gt( med( "F9_01_REV_TOT_CY" ),    1000 )

  # Negative net assets exist (they read as NaN when integer64 is misread)
  na_eoy <- dat10k$F9_10_NAFB_TOT_EOY
  expect_gt( mean( na_eoy < 0, na.rm = TRUE ), 0.01 )
  expect_lt( mean( na_eoy < 0, na.rm = TRUE ), 0.20 )

  # Balance-sheet identity holds for most full 990 filers
  is990 <- dat10k$RETURN_TYPE == "990"
  gap   <- with( dat10k[ is990, ],
                 F9_10_ASSET_TOT_EOY - F9_10_LIAB_TOT_EOY - F9_10_NAFB_TOT_EOY )
  expect_gt( mean( abs( gap ) <= 1, na.rm = TRUE ), 0.9 )
})

test_that( "every dat10k row has a usable NTEE code", {
  # "Z99" is what get_clean_ntee() assigns to a blank code
  ntee <- dat10k$NTEE_NCCS
  expect_false( anyNA( ntee ) )
  expect_equal( ntee[ !grepl( "^[A-Y][0-9][0-9A-Z]$", ntee ) ], character(0) )
  expect_false( any( dat10k$NTMAJ12 == "UNU" ) )
  expect_identical( substr( dat10k$NTEEV2, 5, 7 ), ntee )
})


# ---- detection and repair helpers ----------------------------------------

make_stripped <- function( dollars ) {
  x <- bit64::as.integer64( dollars )
  unclass( x )
}

test_that( "is_unclassed_integer64 flags stripped integer64 and nothing else", {
  expect_true(  fiscal:::is_unclassed_integer64( make_stripped( c( 0, 150, 2e6, NA ) ) ) )
  expect_false( fiscal:::is_unclassed_integer64( c( 0, 150, 2e6, NA ) ) )
  expect_false( fiscal:::is_unclassed_integer64( c( 0, 0, NA ) ) )
  expect_false( fiscal:::is_unclassed_integer64( 1:3 ) )
  expect_false( fiscal:::is_unclassed_integer64( bit64::as.integer64( 1:3 ) ) )
  expect_false( fiscal:::is_unclassed_integer64( Sys.time() ) )
})

test_that( "repair_integer64 recovers values, negatives, and NA", {
  dollars <- c( 0, 150, -2500, 2e6, NA )
  expected <- dollars

  expect_identical( fiscal:::repair_integer64( make_stripped( dollars ) ), expected )
  expect_identical( fiscal:::repair_integer64( bit64::as.integer64( dollars ) ), expected )
  expect_identical( fiscal:::repair_integer64( dollars ), dollars )
})

test_that( "coerce_numeric repairs class-stripped integer64 with a warning", {
  d <- data.frame( a = make_stripped( c( 10, -20, NA ) ), b = c( 1, 2, 3 ) )
  expect_warning( out <- coerce_numeric( d, vars = c( "a", "b" ) ), "class attribute dropped" )
  expect_identical( out$a, c( 10, -20, NA ) )
  expect_identical( out$b, c( 1, 2, 3 ) )
})

test_that( "coerce_numeric converts classed integer64 silently", {
  d <- data.frame( a = bit64::as.integer64( c( 10, -20, NA ) ) )
  expect_silent( out <- coerce_numeric( d, vars = "a" ) )
  expect_identical( out$a, c( 10, -20, NA ) )
})

test_that( "sanitize_financials repairs stripped integer64 before imputing", {
  d <- data.frame(
    RETURN_TYPE         = c( "990", "990", "990" ),
    F9_10_NAFB_TOT_EOY  = make_stripped( c( 5000, -1200, NA ) ),
    F9_10_ASSET_TOT_EOY = c( 9000, 800, 100 )
  )
  expect_warning( out <- sanitize_financials( d ), "class attribute dropped" )

  # The negative survives (a stripped column reads it as NaN, which imputation
  # would otherwise zero out).
  expect_equal( out$F9_10_NAFB_TOT_EOY[ 1:2 ], c( 5000, -1200 ) )
  expect_type( out$F9_10_NAFB_TOT_EOY, "double" )
  expect_false( inherits( out$F9_10_NAFB_TOT_EOY, "integer64" ) )
})

test_that( "sanitize_financials keeps integer64 negatives when bit64 is not yet loaded", {
  skip_on_cran()
  skip_if_not_installed( "callr" )

  # integer64 S3 methods are registered only once the bit64 namespace loads.
  # Before that, is.na() reads negatives as NaN and zero-imputation destroys
  # them. This bit compute_all(dat10k) in a fresh session, so test in one.
  d <- data.frame(
    RETURN_TYPE         = c( "990", "990", "990" ),
    F9_10_NAFB_TOT_EOY  = bit64::as.integer64( c( 5000, -1200, NA ) ),
    F9_10_ASSET_TOT_EOY = c( 9000, 800, 100 )
  )
  pkg_root    <- normalizePath( test_path( "..", ".." ), mustWork = FALSE )
  from_source <- file.exists( file.path( pkg_root, "DESCRIPTION" ) )

  res <- callr::r( function( d, pkg_root, from_source ) {
    if ( from_source ) pkgload::load_all( pkg_root, quiet = TRUE ) else library( fiscal )
    bit64_loaded <- "bit64" %in% loadedNamespaces()
    out <- sanitize_financials( d )
    list( bit64_loaded = bit64_loaded, net_assets = out$F9_10_NAFB_TOT_EOY )
  }, args = list( d, pkg_root, from_source ) )

  skip_if( res$bit64_loaded, "bit64 was already loaded in the child session" )
  expect_identical( res$net_assets, c( 5000, -1200, 0 ) )
})
