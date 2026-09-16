# test-api-fixes.R
# compute_all_panel() row handling, earned income vs. donations consistency,
# debt to net assets definition, and numerator/denominator overrides.

quiet <- function( expr ) suppressWarnings( suppressMessages( expr ) )


# ---- compute_all_panel() ------------------------------------------------------

make_panel <- function() {
  a <- make_test_df( n = 12 ); a$TAX_YEAR <- 2021L
  b <- make_test_df( n = 12 ); b$TAX_YEAR <- 2022L
  b$EIN2 <- paste0( "EIN", 13:24 )
  p <- rbind( a, b )
  p <- p[ c( 13, 1, 14, 2, 15:24, 3:12 ), ]      # interleave years
  rownames( p ) <- NULL
  p
}

test_that( "compute_all_panel() works with append_to_df = FALSE and keeps row order", {
  p   <- make_panel()
  out <- quiet( compute_all_panel( p, append_to_df = FALSE, verbose = FALSE ) )

  expect_equal( nrow( out ), nrow( p ) )
  expect_equal( out$EIN2, p$EIN2 )
  expect_false( ".row_order." %in% names( out ) )
  expect_true( all( c( "debt_assets", "debt_assets_z" ) %in% names( out ) ) )
  expect_false( "F9_10_LIAB_TOT_EOY" %in% names( out ) )
})

test_that( "compute_all_panel() matches compute_all() within each year", {
  p   <- make_panel()
  out <- quiet( compute_all_panel( p, verbose = FALSE ) )
  expect_equal( out$EIN2, p$EIN2 )

  y21 <- quiet( compute_all( p[ p$TAX_YEAR == 2021L, ], verbose = FALSE ) )
  expect_equal( out$debt_assets_p[ p$TAX_YEAR == 2021L ], y21$debt_assets_p )
})

test_that( "compute_all_panel() keeps rows with a missing year, with NA metrics", {
  p <- make_panel()
  p$TAX_YEAR[ c( 2, 5 ) ] <- NA

  expect_warning( out <- suppressMessages( compute_all_panel( p, verbose = FALSE ) ),
                  "missing `TAX_YEAR`" )
  expect_equal( nrow( out ), nrow( p ) )
  expect_equal( out$EIN2, p$EIN2 )
  expect_true( all( is.na( out$debt_assets[ c( 2, 5 ) ] ) ) )
  expect_false( anyNA( out$debt_assets[ -c( 2, 5 ) ] ) )

  out2 <- quiet( compute_all_panel( p, append_to_df = FALSE, verbose = FALSE ) )
  expect_equal( out2$EIN2, p$EIN2 )
})


# ---- earned income vs. donations ---------------------------------------------

test_that( "earned income excludes membership dues already counted in contributions", {
  df <- make_test_df( n = 10 )
  # contributions (1h) include dues (1b); keep every revenue line non-negative
  df$F9_08_REV_CONTR_TOT <- df$F9_08_REV_CONTR_TOT + df$F9_08_REV_CONTR_MEMBSHIP_DUE
  df$F9_08_REV_TOT_TOT   <- df$F9_08_REV_CONTR_TOT + df$F9_08_REV_OTH_FUNDR_NET_TOT +
                            df$F9_08_REV_PROG_TOT_TOT + df$F9_08_REV_OTH_ROY_TOT +
                            df$F9_08_REV_MISC_OTH_TOT

  ei <- quiet( get_earned_income_ratio( df, sanitize = FALSE ) )$earned_income
  dr <- quiet( get_donations_revenue_ratio( df, sanitize = FALSE ) )$donations_rev

  expect_equal( ei, ( df$F9_08_REV_PROG_TOT_TOT + df$F9_08_REV_OTH_ROY_TOT +
                      df$F9_08_REV_MISC_OTH_TOT ) / df$F9_08_REV_TOT_TOT )
  expect_equal( ei + dr, rep( 1, nrow( df ) ) )
})

test_that( "get_earned_income_ratio() warns that membership_dues is ignored", {
  df <- make_test_df( n = 10 )
  base <- quiet( get_earned_income_ratio( df, sanitize = FALSE ) )$earned_income
  expect_warning(
    alt <- suppressMessages( get_earned_income_ratio(
      df, membership_dues = "F9_08_REV_CONTR_MEMBSHIP_DUE", sanitize = FALSE ) ),
    "deprecated" )
  expect_equal( alt$earned_income, base )
})


# ---- debt to net assets --------------------------------------------------------

test_that( "debt_netassets divides by total net assets; debt_equity by unrestricted", {
  df <- make_test_df( n = 10 )
  dn <- quiet( get_debt_netassets_ratio( df, sanitize = FALSE ) )$debt_netassets
  de <- quiet( get_debt_equity_ratio( df, sanitize = FALSE ) )$debt_equity

  expect_equal( dn, df$F9_10_LIAB_TOT_EOY / df$F9_10_NAFB_TOT_EOY )
  expect_equal( de, df$F9_10_LIAB_TOT_EOY / df$F9_10_NAFB_UNRESTRICT_EOY )
  expect_false( isTRUE( all.equal( dn, de ) ) )
})

test_that( "debt_netassets is available for 990-EZ filers", {
  df <- make_test_df( n = 10 )
  df$F9_10_NAFB_UNRESTRICT_EOY[ df$RETURN_TYPE == "990EZ" ] <- NA
  dn <- quiet( get_debt_netassets_ratio( df, sanitize = FALSE ) )$debt_netassets
  expect_false( anyNA( dn ) )
})


# ---- numerator / denominator overrides ----------------------------------------

override_cases <- list(
  list( fn = "get_current_ratio",           metric = "current" ),
  list( fn = "get_quick_ratio",             metric = "quick" ),
  list( fn = "get_days_cash_operations",    metric = "days_cash_ops" ),
  list( fn = "get_months_cash_operations",  metric = "months_cash_ops" ),
  list( fn = "get_days_cash_investments",   metric = "days_cash_inv" ),
  list( fn = "get_debt_shortterm_ratio",    metric = "debt_shortterm" ),
  list( fn = "get_donations_revenue_ratio", metric = "donations_rev" ),
  list( fn = "get_earned_income_ratio",     metric = "earned_income" ),
  list( fn = "get_investment_income_ratio", metric = "invest_income" ),
  list( fn = "get_profit_margin_predepr",   metric = "profit_predepr" )
)

test_that( "numerator alone overrides the default component columns", {
  df <- make_test_df( n = 10 )
  df$NUM <- seq_len( nrow( df ) ) * 1000
  for ( case in override_cases ) {
    fn  <- get( case$fn )
    out <- quiet( fn( df, numerator = "NUM", sanitize = FALSE ) )
    val <- out[[ case$metric ]]
    expect_true( all( is.finite( val ) ), info = case$fn )
    # the metric is proportional to NUM when only the numerator changes
    out2 <- quiet( fn( transform( df, NUM = NUM * 2 ), numerator = "NUM", sanitize = FALSE ) )
    expect_equal( out2[[ case$metric ]], 2 * val, info = case$fn )
  }
})

test_that( "denominator alone overrides the default component columns", {
  df <- make_test_df( n = 10 )
  df$DEN <- seq_len( nrow( df ) ) * 1e5
  for ( case in override_cases ) {
    fn  <- get( case$fn )
    out  <- quiet( fn( df, denominator = "DEN", sanitize = FALSE ) )
    out2 <- quiet( fn( transform( df, DEN = DEN * 2 ), denominator = "DEN", sanitize = FALSE ) )
    expect_equal( out2[[ case$metric ]], out[[ case$metric ]] / 2, info = case$fn )
  }
})

test_that( "numerator and denominator together need no component columns", {
  df <- data.frame( EIN2 = paste0( "E", 1:5 ), RETURN_TYPE = "990",
                    NUM = c( 10, 20, 30, 40, 50 ), DEN = c( 100, 100, 50, 80, 25 ) )
  for ( case in override_cases ) {
    out <- quiet( get( case$fn )( df, numerator = "NUM", denominator = "DEN", sanitize = FALSE ) )
    scale <- switch( case$metric, days_cash_ops = 365, days_cash_inv = 365,
                     months_cash_ops = 12, 1 )
    expect_equal( out[[ case$metric ]], df$NUM / df$DEN * scale, info = case$fn )
  }
})

test_that( "an override plus an explicitly set component is an error", {
  df <- make_test_df( n = 10 )
  df$NUM <- 1
  expect_error( get_current_ratio( df, numerator = "NUM", cash = "F9_10_ASSET_CASH_EOY" ),
                "`cash`" )
  expect_error( get_donations_revenue_ratio( df, denominator = "NUM",
                                             total_revenue = "F9_08_REV_TOT_TOT" ),
                "`total_revenue`" )
  expect_error( get_profit_margin_predepr( df, numerator = "NUM", expenses = "F9_09_EXP_TOT_TOT" ),
                "`expenses`" )
  expect_error( get_profit_margin_predepr( df, numerator = "NUM", denominator = "NUM",
                                           revenue = "F9_08_REV_TOT_TOT" ),
                "`revenue`" )
  # an explicit NULL component is fine
  expect_silent( quiet( get_current_ratio( df, numerator = "NUM", cash = NULL, sanitize = FALSE ) ) )
})

test_that( "leaving both the override and every component NULL is an error", {
  df <- make_test_df( n = 10 )
  expect_error( get_debt_shortterm_ratio( df, accounts_payable = NULL, grants_payable = NULL ),
                "No numerator specified" )
})
