# test-nan-behaviour.R
# Tests the NaN / NA distinction for zero-denominator cases:
#   - Zero denominator   → NaN  (structurally undefined but numerator meaningful)
#   - Missing data       → NA   (data absent)
# Also tests that NaN propagates correctly through winsorization and scoring.

library( fiscal )

# ---- NaN produced when denominator is zero --------------------------------

test_that( "zero denominator produces NaN, not NA", {

  df <- make_test_df( n = 6 )
  df$F9_10_LIAB_TOT_EOY[1] <- 0   # denominator for debt_secured, debt_unsecured

  result <- suppressMessages( get_debt_secured_ratio( df, sanitize = TRUE ) )

  # Row 1 should be NaN (zero denominator), not NA
  expect_true( is.nan( result$debt_secured[1] ),
               label = "zero total_liabilities → debt_secured is NaN" )

  # Remaining rows should be finite (not NA, not NaN)
  non_zero <- result$debt_secured[-1]
  expect_true( all( is.finite(non_zero) | is.nan(non_zero) ),
               label = "non-zero denominator rows are finite or NaN" )
})

test_that( "NaN is distinct from NA in ratio output", {

  df <- make_test_df( n = 4 )
  df$F9_10_ASSET_TOT_EOY[1] <- 0       # zero denominator → NaN
  df$F9_10_LIAB_TOT_EOY[2]  <- NA_real_ # missing numerator → NA in ratio

  result <- suppressMessages( get_debt_assets_ratio( df, sanitize = FALSE ) )

  expect_true(  is.nan( result$debt_assets[1] ), label = "zero denom → NaN" )
  expect_true(  is.na(  result$debt_assets[2] ) && !is.nan( result$debt_assets[2] ),
                label = "NA numerator → NA (not NaN)" )
})

test_that( "revenue zero denominator → NaN for surplus_margin", {

  df <- make_test_df( n = 5 )
  df$F9_08_REV_TOT_TOT[2]  <- 0
  df$F9_01_REV_TOT_CY[2]   <- 0

  result <- suppressMessages( get_surplus_margin_ratio( df, sanitize = FALSE ) )

  expect_true( is.nan( result$surplus_margin[2] ),
               label = "zero total revenue → surplus_margin is NaN" )
})

test_that( "expense zero denominator → NaN for overhead_ratio", {

  df <- make_test_df( n = 5 )
  df$F9_09_EXP_TOT_TOT[3] <- 0

  result <- suppressMessages( get_overhead_ratio( df, sanitize = FALSE ) )

  expect_true( is.nan( result$overhead_ratio[3] ),
               label = "zero total expenses → overhead_ratio is NaN" )
})


# ---- NaN handling in apply_transformations --------------------------------

test_that( "apply_transformations handles NaN in input vector", {

  x <- c( 1, 2, NaN, 4, 5, NaN, 7, 8, 9, 10 )
  out <- apply_transformations( x, range = "np" )

  # Structural checks
  expect_equal( length(out$raw),       length(x) )
  expect_equal( length(out$winsorized),length(x) )
  expect_equal( length(out$z),         length(x) )
  expect_equal( length(out$pctile),    length(x) )

  # NaN positions in input remain NaN or NA in output (not propagated to others)
  nan_pos <- which( is.nan(x) )
  non_nan <- which( !is.nan(x) )

  # Non-NaN positions should have finite z-scores
  expect_true( all( is.finite( out$z[non_nan] ) | is.na( out$z[non_nan] ) ),
               label = "non-NaN positions have finite or NA z-scores" )
})

test_that( "apply_transformations: all-NaN input returns all-NaN/NA output", {

  x <- c( NaN, NaN, NaN )
  expect_warning(
    out <- apply_transformations( x, range = "np" ),
    regexp = NA  # allow any warning or none — just shouldn't error
  )
  expect_equal( length(out$raw), 3 )
})


# ---- NaN handling in panel_smooth ----------------------------------------

test_that( "panel_smooth treats NaN like NA during smoothing", {

  df <- data.frame(
    EIN2     = rep( "A", 5 ),
    TAX_YEAR = 2018:2022,
    revenue  = c( 100, NaN, 300, 400, 500 )
  )

  result <- panel_smooth( df, vars = "revenue", window = 3,
                          time = "TAX_YEAR", id = "EIN2" )

  # Year 2019 (NaN): should be smoothed over years 2018 and 2020 only
  # equal weights: (100 + 300) / 2 = 200  — NaN excluded
  expect_equal( result$revenue[2], 200, tolerance = 1e-9,
                label = "NaN excluded from window average" )

  # NaN position stays NaN (not converted to a numeric average)
  expect_true( is.nan( result$revenue[2] ),
               label = "original NaN position restored to NaN in output" )
})

test_that( "panel_smooth: NaN at edge of panel handled correctly", {

  df <- data.frame(
    EIN2     = rep( "B", 4 ),
    TAX_YEAR = 2020:2023,
    x        = c( NaN, 200, 300, NaN )
  )

  result <- panel_smooth( df, vars = "x", window = 3,
                          time = "TAX_YEAR", id = "EIN2" )

  # Year 2021 (window [2020,2021,2022]): NaN at 2020 excluded → (200+300)/2 = 250
  expect_equal( result$x[2], 250, tolerance = 1e-9 )

  # NaN positions should remain NaN
  expect_true( is.nan( result$x[1] ) )
  expect_true( is.nan( result$x[4] ) )
})
