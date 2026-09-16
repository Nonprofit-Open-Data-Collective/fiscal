# test-normalization-bounds.R
# The `_z` columns feed correlation-based modeling, so boundary pile-ups and
# winsorized tails must not be scored as extreme outliers.

rank_normal_ref <- function( x ) {
  r <- rank( x, na.last = "keep", ties.method = "average" )
  qnorm( ( r - 0.5 ) / sum( !is.na( x ) ) )
}

test_that( "midrank_ecdf() matches average ranks for in-sample values", {
  x <- c( 0, 0, 0, 0.2, 0.5, 0.5, 1 )
  p <- fiscal:::midrank_ecdf( x, sort( x ) )
  expect_equal( p, ( rank( x, ties.method = "average" ) - 0.5 ) / length( x ) )
})

test_that( "rank_normal scores a tied boundary block at its mid-rank, not the top", {
  set.seed( 11 )
  # 30% of values are exactly 1 (e.g. program-expense ratio of 100%)
  x   <- c( runif( 700, 0.3, 0.99 ), rep( 1, 300 ) )
  fit <- find_best_normalization( x, range = "zo", verbose = FALSE )
  expect_equal( fit$transform_type, "rank_normal" )

  z  <- apply_normalization( x, fit )
  # all ones share a single score near qnorm(1 - 0.15) ~ 1.04 before scaling;
  # the old ECDF placed them at qnorm(1 - 0.5/n) ~ 3.1
  expect_length( unique( z[ x == 1 ] ), 1 )
  expect_lt( max( z ), 2 )
})

test_that( "logit scores for exact zeros stay bounded as n grows", {
  set.seed( 12 )
  score_zero <- function( n ) {
    x <- rbeta( n, 2, 10 )
    x[ seq_len( round( 0.05 * n ) ) ] <- 0          # 5% zeros: below hurdle cutoff
    fit <- find_best_normalization( x, range = "zo", verbose = FALSE )
    expect_equal( fit$transform_type, "logit" )
    z <- apply_normalization( x, fit )
    c( zero = z[1], mean = mean( z ) )
  }
  small <- score_zero( 2000 )
  large <- score_zero( 200000 )
  # With eps = 0.5/n the zero score drifted from about -6 to about -10.
  expect_gt( large[["zero"]], -4 )
  expect_lt( abs( large[["zero"]] - small[["zero"]] ), 0.5 )
  expect_lt( abs( large[["mean"]] ), 0.5 )
})

test_that( "zero-inflated proportions no longer produce an all-NA _z column", {
  set.seed( 13 )
  x <- c( rep( 0, 700 ), runif( 300 ) )             # 70% zeros -> MAD of scores is 0
  out <- apply_transformations( x, range = "zo" )
  expect_false( any( is.na( out$z ) ) )
  expect_length( unique( out$z[ x == 0 ] ), 1 )
  expect_true( all( out$z[ x > 0 ] > out$z[ x == 0 ][1] ) )
})

test_that( "winsorized np tails are scored at their quantile, not at qnorm(0.5/n)", {
  set.seed( 14 )
  x   <- rt( 50000, df = 1.5 )
  out <- apply_transformations( x, range = "np", winsorize = 0.98 )
  # 1% tails tie at the bound: mid-rank 0.5% -> about +/-2.58
  expect_lt( max( abs( out$z ), na.rm = TRUE ), 3 )
  expect_equal( mean( out$z ), 0, tolerance = 0.05 )
})

test_that( "Pearson correlations of _z columns track raw Spearman correlations", {
  set.seed( 15 )
  n    <- 20000
  adm  <- rbeta( n, 2, 12 ); adm[ sample( n, 0.07 * n ) ] <- 0
  fund <- rbeta( n, 1, 30 ); fund[ sample( n, 0.5 * n ) ] <- 0
  prog <- pmin( 1, pmax( 0, 1 - adm - fund + rnorm( n, 0, 0.01 ) ) )
  prog[ sample( n, 0.12 * n ) ] <- 1

  pairs <- list( c( "adm", "prog" ), c( "fund", "prog" ), c( "adm", "fund" ) )
  vals  <- list( adm = adm, prog = prog, fund = fund )
  for ( p in pairs ) {
    a <- vals[[ p[1] ]]; b <- vals[[ p[2] ]]
    za <- apply_transformations( a, range = "zo" )$z
    zb <- apply_transformations( b, range = "zo" )$z
    rho <- cor( a, b, method = "spearman" )
    expect_lt( abs( cor( za, zb ) - rho ), 0.06, label = paste( p, collapse = "~" ) )
  }
})

test_that( "fits saved before interior-quantile clamping still apply", {
  x   <- runif( 200, 0.05, 0.95 )
  fit <- find_best_normalization( x, range = "zo", verbose = FALSE )
  fit$params$p_lo <- NULL
  fit$params$p_hi <- NULL
  z <- apply_normalization( c( 0, x ), fit )
  expect_true( is.finite( z[1] ) )
})
