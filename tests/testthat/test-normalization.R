# test-normalization.R
# Tests for the normalization pipeline:
#   winsorize_x(), find_best_normalization(), apply_normalization(), normalize_x()

library( fiscal )

set.seed(123)


# ---- winsorize_x() -------------------------------------------------------

test_that( "winsorize_x: basic structure", {
  x <- c( NA, rnorm(100), 200, -200 )
  w <- winsorize_x( x, range = "np", winsorize = 0.98 )

  expect_equal( length(w$x_w), length(x) )
  expect_true(  is.numeric(w$x_w) )
  expect_true(  !is.na(w$top) )
  expect_true(  !is.na(w$bottom) )
  expect_true(  w$top > w$bottom )
  expect_equal( length(w$is_sentinel), length(x) )
  expect_true(  is.logical(w$is_sentinel) )
})

test_that( "winsorize_x: extreme values capped at bounds", {
  x <- c( rnorm(100), 1e9, -1e9 )
  w <- winsorize_x( x, range = "np", winsorize = 0.98 )

  expect_true( max( w$x_w, na.rm = TRUE ) <= w$top + .Machine$double.eps )
  expect_true( min( w$x_w, na.rm = TRUE ) >= w$bottom - .Machine$double.eps )
})

test_that( "winsorize_x: zo range uses fixed bounds", {
  x <- runif(50)
  w <- winsorize_x( x, range = "zo", offset = 0.001 )

  expect_equal( w$top,    1.001, tolerance = 1e-10 )
  expect_equal( w$bottom, -0.001, tolerance = 1e-10 )
  # All values should already be within [0,1] so no sentinels
  expect_false( any(w$is_sentinel) )
})

test_that( "winsorize_x: zp range uses one-sided winsorization", {
  x <- c( 0, abs(rnorm(100)) )
  w <- winsorize_x( x, range = "zp", winsorize = 0.95 )

  expect_equal( w$bottom, -0.001, tolerance = 1e-10 )
  expect_true( is.na(w$top) || w$top > 0 )
})

test_that( "winsorize_x: NAs preserved in output", {
  x <- c( 1, NA, 3, NA, 5 )
  w <- winsorize_x( x, range = "np" )

  expect_true(  is.na( w$x_w[2] ) )
  expect_true(  is.na( w$x_w[4] ) )
  expect_false( is.na( w$x_w[1] ) )
})


# ---- find_best_normalization() + apply_normalization() -------------------

test_that( "find_best_normalization: returns normalize_x_fit object", {
  x   <- rnorm(200)
  fit <- find_best_normalization( x, range = "np", verbose = FALSE )

  expect_s3_class( fit, "normalize_x_fit" )
  expect_true( !is.null(fit$transform_type) )
  expect_true( !is.null(fit$center) )
  expect_true( !is.null(fit$scale) )
  expect_true( fit$n_clean > 0 )
})

test_that( "find_best_normalization: np range selects rank_normal", {
  # Mixed-support data → should select rank_normal
  x <- c( rnorm(100), -rnorm(100) )
  fit <- find_best_normalization( x, range = "np", verbose = FALSE )

  expect_equal( fit$transform_type, "rank_normal" )
})

test_that( "find_best_normalization: zo range with zero mass selects hurdle", {
  x <- c( rep(0, 40), runif(60) )
  fit <- find_best_normalization( x, range = "zo", verbose = FALSE )

  expect_equal( fit$transform_type, "hurdle" )
})

test_that( "find_best_normalization: zo range without zero mass selects logit", {
  x <- runif( 100, 0.01, 0.99 )
  fit <- find_best_normalization( x, range = "zo", verbose = FALSE )

  expect_equal( fit$transform_type, "logit" )
})

test_that( "find_best_normalization: zp range selects asinh", {
  x <- abs( rnorm(100) ) + 0.1
  fit <- find_best_normalization( x, range = "zp", verbose = FALSE )

  expect_equal( fit$transform_type, "asinh" )
})

test_that( "apply_normalization: same length as input", {
  x_train <- rnorm(200)
  x_new   <- rnorm(50)
  fit     <- find_best_normalization( x_train, range = "np", verbose = FALSE )
  z       <- apply_normalization( x_new, fit = fit )

  expect_equal( length(z), length(x_new) )
  expect_true(  is.numeric(z) )
})

test_that( "apply_normalization: fit/apply split gives same result as normalize_x", {
  x   <- rnorm(100)
  fit <- find_best_normalization( x, range = "np", verbose = FALSE )
  z1  <- apply_normalization( x, fit = fit )
  z2  <- normalize_x( x, range = "np", verbose = FALSE )

  expect_equal( z1, z2, tolerance = 1e-12 )
})

test_that( "apply_normalization: rejects non-fit object", {
  expect_error(
    apply_normalization( rnorm(10), fit = list(a = 1) ),
    regexp = "normalize_x_fit"
  )
})


# ---- normalize_x() -------------------------------------------------------

test_that( "normalize_x: output length equals input length", {
  x <- c( NA, rnorm(50) )
  z <- normalize_x( x, range = "np", verbose = FALSE )

  expect_equal( length(z), length(x) )
})

test_that( "normalize_x: NAs in input remain NA in output", {
  x <- c( 1, 2, NA, 4, 5 )
  z <- normalize_x( x, range = "np", verbose = FALSE )

  expect_true( is.na(z[3]) )
  expect_false( any( is.na(z[-3]) ) )
})

test_that( "normalize_x: vtype override is respected", {
  x <- abs( rnorm(100) ) + 0.1
  z <- normalize_x( x, range = "zp", vtype = "rank_normal", verbose = FALSE )

  expect_equal( attr(z, "transform_type"), "rank_normal" )
})

test_that( "normalize_x: invalid range rejected", {
  expect_error(
    normalize_x( rnorm(10), range = "bad_range" ),
    regexp = "range"
  )
})

test_that( "normalize_x: custom lo;hi range accepted", {
  x <- runif(50, 0, 10)
  z <- normalize_x( x, range = "0;10", verbose = FALSE )

  expect_equal( length(z), 50 )
  expect_true( is.numeric(z) )
})


# ---- apply_transformations() ---------------------------------------------

test_that( "apply_transformations: returns named list with 4 elements", {
  x   <- rnorm(50)
  out <- apply_transformations( x, range = "np" )

  expect_true( is.list(out) )
  expect_named( out, c("raw", "winsorized", "z", "pctile") )
})

test_that( "apply_transformations: all elements have correct length", {
  x   <- c( NA, rnorm(30) )
  out <- apply_transformations( x, range = "np" )

  expect_equal( length(out$raw),       length(x) )
  expect_equal( length(out$winsorized),length(x) )
  expect_equal( length(out$z),         length(x) )
  expect_equal( length(out$pctile),    length(x) )
})

test_that( "apply_transformations: raw equals input", {
  x   <- rnorm(20)
  out <- apply_transformations( x, range = "np" )

  expect_equal( out$raw, x )
})

test_that( "apply_transformations: winsorized never exceeds raw range by more than offset", {
  x   <- c( rnorm(100), 1000, -1000 )
  out <- apply_transformations( x, range = "np", winsorize = 0.98 )

  # Extreme values should be capped
  expect_true( max( out$winsorized, na.rm = TRUE ) < 1000 )
  expect_true( min( out$winsorized, na.rm = TRUE ) > -1000 )
})

test_that( "apply_transformations: pctile values in 1-100", {
  x   <- rnorm(50)
  out <- apply_transformations( x, range = "np" )
  p   <- out$pctile[ !is.na(out$pctile) ]

  expect_true( all( p >= 1L & p <= 100L ) )
})
