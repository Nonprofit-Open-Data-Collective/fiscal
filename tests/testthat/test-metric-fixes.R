# test-metric-fixes.R
# Regression tests for metric definitions corrected after building the 2023
# fiscal health index (dev/health-index/fiscal-health-index-2023.Rmd).

quiet_metric <- function( expr ) suppressMessages( expr )

# A small full-990 frame with hand-checkable values.
make_990_df <- function() {
  data.frame(
    EIN2        = paste0( "EIN", 1:6 ),
    RETURN_TYPE = "990",
    F9_09_EXP_TOT_TOT   = c( 1000, 1000, 2000, 500, 800, 1200 ),
    F9_09_EXP_TOT_MGMT  = c(  100,  150,  300,  50,   0,  100 ),
    F9_09_EXP_TOT_FUNDR = c(   50,    0,  100,  25,  40,   20 ),
    F9_10_ASSET_CASH_EOY   = c( 100, 200,   0, 50, 10,  30 ),
    F9_10_ASSET_SAVING_EOY = c(  50,   0, 300, 25, 40, 100 ),
    F9_10_ASSET_TOT_EOY    = c( 1000, 400, 600, 150, 100, 520 ),
    stringsAsFactors = FALSE
  )
}


# ---- 1. overhead = ( mgmt + fundraising ) / total ---------------------------

test_that( "get_overhead_ratio() includes fundraising expenses", {
  df  <- make_990_df()
  out <- quiet_metric( get_overhead_ratio( df, sanitize = FALSE ) )

  expected <- ( df$F9_09_EXP_TOT_MGMT + df$F9_09_EXP_TOT_FUNDR ) / df$F9_09_EXP_TOT_TOT
  expect_equal( out$overhead, expected )

  admin <- quiet_metric( get_expenses_admin_ratio( df, sanitize = FALSE ) )
  expect_false( isTRUE( all.equal( out$overhead, admin$expenses_admin ) ) )
})

test_that( "get_overhead_ratio() is the complement of the program-expense ratio", {
  df <- make_test_df( n = 20 )
  df$F9_09_EXP_TOT_TOT <- df$F9_09_EXP_TOT_PROG + df$F9_09_EXP_TOT_MGMT + df$F9_09_EXP_TOT_FUNDR
  oh <- quiet_metric( get_overhead_ratio( df, sanitize = FALSE ) )$overhead
  pe <- quiet_metric( get_program_expenses_ratio( df, sanitize = FALSE ) )$prog_exp
  expect_equal( oh + pe, rep( 1, nrow( df ) ) )
})


# ---- 2. cash_assets = ( cash + savings ) / total_assets ---------------------

test_that( "get_cash_assets_ratio() includes savings", {
  df  <- make_990_df()
  out <- quiet_metric( get_cash_assets_ratio( df, sanitize = FALSE ) )

  expected <- ( df$F9_10_ASSET_CASH_EOY + df$F9_10_ASSET_SAVING_EOY ) / df$F9_10_ASSET_TOT_EOY
  expect_equal( out$cash_assets, expected )
  # row 3 holds only savings: previously scored 0
  expect_equal( out$cash_assets[3], 0.5 )
})

test_that( "get_investments_assets_ratio() includes other securities", {
  df  <- make_test_df( n = 10 )
  out <- quiet_metric( get_investments_assets_ratio( df, sanitize = FALSE ) )
  expected <- ( df$F9_10_ASSET_INVEST_SEC_EOY + df$F9_10_ASSET_INVEST_SEC_OTH_EOY ) /
    df$F9_10_ASSET_TOT_EOY
  expect_equal( out$investments_assets, expected )
})

test_that( "every column argument of a get_*() metric enters its calculation", {
  # Guards against the class of bug in issues 1-2: an argument that is
  # declared and documented but never used. Changing any one input column
  # must change the raw ratio.
  df   <- make_test_df( n = 12 )
  reg  <- fiscal_metrics()
  skip <- c( "RETURN_TYPE" )
  for ( fn_name in reg$function_name ) {
    fn   <- get( fn_name )
    base <- quiet_metric( fn( df, sanitize = FALSE ) )
    metric <- reg$metric[ reg$function_name == fn_name ]
    for ( arg in names( formals( fn ) ) ) {
      cols <- tryCatch( eval( formals( fn )[[ arg ]] ), error = function( e ) NULL )
      if ( !is.character( cols ) ) next
      col <- intersect( cols, colnames( df ) )[1]
      if ( is.na( col ) || col %in% skip ) next
      # net-asset fallback columns only matter when lines 27-28 are zero
      if ( arg %in% c( "restricted_net_assets" ) ) next
      if ( arg == "total_net_assets" && fn_name != "get_netassets_composition_ratio" ) next
      d2 <- df
      d2[[ col ]] <- d2[[ col ]] * 1.37 + 1000
      alt <- suppressWarnings( quiet_metric( fn( d2, sanitize = FALSE ) ) )
      expect_false( isTRUE( all.equal( base[[ metric ]], alt[[ metric ]] ) ),
                    info = paste0( fn_name, "(", arg, " = ", col, ")" ) )
    }
  }
})


# ---- 3. registry directions -------------------------------------------------

test_that( "fiscal_metrics() directions match the metric definitions", {
  reg <- fiscal_metrics()
  dir <- setNames( reg$direction, reg$metric )

  # fundraising expense per dollar raised: lower is better
  expect_equal( unname( dir[ "fundr_eff" ] ), "lower" )
  # cash_eoy / cash_boy: accumulating cash is better
  expect_equal( unname( dir[ "cash_burn" ] ), "higher" )
  # liability composition shares have no benchmark direction
  expect_equal( unname( dir[ c( "debt_secured", "debt_unsecured" ) ] ), c( "context", "context" ) )

  lower <- c( "debt_assets", "debt_equity", "debt_netassets", "debt_shortterm",
              "overhead", "expenses_admin", "fundr_eff" )
  expect_true( all( dir[ lower ] == "lower" ) )
  higher <- c( "current", "quick", "cash_liq", "days_cash_ops", "days_cash_inv",
               "months_cash_ops", "liquid_assets_months", "op_reserve", "equity",
               "surplus_margin", "return_assets", "prog_exp", "netassets_growth" )
  expect_true( all( dir[ higher ] == "higher" ) )
})

test_that( "fiscal_metrics() rejects a direction vector out of order", {
  body_txt <- paste( deparse( body( fiscal_metrics ) ), collapse = "\n" )
  expect_match( body_txt, "identical(names(direction), metric)", fixed = TRUE )
})


# ---- 4. non-SFAS 117 filers: fall back to total net assets ------------------

make_sfas_df <- function() {
  data.frame(
    EIN2        = paste0( "EIN", 1:4 ),
    RETURN_TYPE = "990",
    #                              SFAS  non-SFAS  all-restricted  EZ-like NA
    F9_10_NAFB_UNRESTRICT_EOY = c(  600,        0,              0,         NA ),
    F9_10_NAFB_RESTRICT_EOY   = c(  400,        0,            500,         NA ),
    F9_10_NAFB_TOT_EOY        = c( 1000,      900,            500,        300 ),
    F9_10_ASSET_LAND_BLDG_NET_EOY = c( 100, 100, 100, 100 ),
    F9_10_LIAB_MTG_NOTE_EOY       = c(  50,  50,  50,  50 ),
    F9_10_LIAB_TOT_EOY            = c( 300, 300, 300, 300 ),
    F9_01_NAFB_LIAB_TOT_EOY       = c( 300, 300, 300, 300 ),
    F9_10_ASSET_INVEST_SEC_EOY     = c( 10, 10, 10, 10 ),
    F9_10_ASSET_INVEST_SEC_OTH_EOY = c(  5,  5,  5,  5 ),
    F9_09_EXP_TOT_TOT    = c( 1200, 1200, 1200, 1200 ),
    F9_09_EXP_DEPREC_TOT = c(    0,    0,    0,    0 ),
    stringsAsFactors = FALSE
  )
}

test_that( "resolve_unrestricted_net_assets() substitutes total net assets only for non-SFAS 117 rows", {
  df <- make_sfas_df()
  expect_message( una <- resolve_unrestricted_net_assets( df ), "1 case" )
  expect_equal( una, c( 600, 900, 0, NA ) )

  # fallback can be disabled
  expect_equal( resolve_unrestricted_net_assets( df, total = NULL ), df$F9_10_NAFB_UNRESTRICT_EOY )

  # without a restricted column, a zero unrestricted value with nonzero total falls back
  una2 <- suppressMessages( resolve_unrestricted_net_assets( df, restricted = NULL ) )
  expect_equal( una2, c( 600, 900, 500, NA ) )
})

test_that( "reserve metrics use total net assets for non-SFAS 117 filers", {
  df <- make_sfas_df()
  exp_m <- 1200 / 12

  orr <- quiet_metric( get_operating_reserve_ratio( df, sanitize = FALSE ) )$op_reserve
  expect_equal( orr[1:3], c( 600 - 100, 900 - 100, 0 - 100 ) / 1200 )

  luna <- quiet_metric( get_liquid_assets_months( df, sanitize = FALSE ) )$liquid_assets_months
  expect_equal( luna[1:3], ( c( 600, 900, 0 ) - ( 100 - 50 ) ) / exp_m )

  der <- quiet_metric( get_debt_equity_ratio( df, sanitize = FALSE ) )$debt_equity
  expect_equal( der[1:2], c( 300 / 600, 300 / 900 ) )
  expect_true( is.nan( der[3] ) )   # genuinely zero unrestricted net assets

  nac <- quiet_metric( get_netassets_composition_ratio( df, sanitize = FALSE ) )$netassets_comp
  expect_equal( nac[1:3], c( 0.6, 1, 0 ) )

  doci <- quiet_metric( get_days_cash_investments( df, sanitize = FALSE ) )$days_cash_inv
  expect_equal( doci[1:3], ( c( 600, 900, 0 ) + 15 - ( 100 + 50 ) ) / ( 1200 / 365 ) )
})

test_that( "setting total_net_assets = NULL restores the unadjusted reserve metric", {
  df  <- make_sfas_df()
  orr <- quiet_metric( get_operating_reserve_ratio( df, total_net_assets = NULL,
                                                    sanitize = FALSE ) )$op_reserve
  expect_equal( orr[2], ( 0 - 100 ) / 1200 )
})


# ---- 5. F9_10_ASSET_INV_SALE_EOY is inventories (Part X line 8) -------------

test_that( "get_days_cash_investments() uses investment securities, not inventories", {
  df <- make_sfas_df()
  base <- quiet_metric( get_days_cash_investments( df, sanitize = FALSE ) )$days_cash_inv

  df$F9_10_ASSET_INV_SALE_EOY <- 1e6
  with_inventory <- quiet_metric( get_days_cash_investments( df, sanitize = FALSE ) )$days_cash_inv
  expect_equal( with_inventory, base )

  df$F9_10_ASSET_INVEST_SEC_OTH_EOY <- df$F9_10_ASSET_INVEST_SEC_OTH_EOY + 365
  more_inv <- quiet_metric( get_days_cash_investments( df, sanitize = FALSE ) )$days_cash_inv
  expect_equal( ( more_inv - base )[1:3], rep( 365 / ( 1200 / 365 ), 3 ) )
})

test_that( "get_days_cash_investments() errors clearly when investment columns are absent", {
  df <- make_sfas_df()
  df$F9_10_ASSET_INVEST_SEC_OTH_EOY <- NULL
  expect_error( quiet_metric( get_days_cash_investments( df, sanitize = FALSE ) ),
                "F9_10_ASSET_INVEST_SEC_OTH_EOY" )
})

test_that( "get_current_ratio() labels line 8 as inventories and keeps a deprecated alias", {
  expect_true( "inventories" %in% names( formals( get_current_ratio ) ) )
  expect_equal( formals( get_current_ratio )$inventories, "F9_10_ASSET_INV_SALE_EOY" )

  df   <- make_test_df( n = 10 )
  base <- quiet_metric( get_current_ratio( df, sanitize = FALSE ) )$current

  df$MY_INV <- df$F9_10_ASSET_INV_SALE_EOY
  expect_warning(
    alias <- quiet_metric( get_current_ratio( df, investment_sales = "MY_INV", sanitize = FALSE ) ),
    "deprecated"
  )
  expect_equal( alias$current, base )
})


# ---- 6. cash_burn = cash_eoy / cash_boy -------------------------------------

test_that( "get_cash_burn_ratio() returns cash_eoy / cash_boy", {
  df <- data.frame(
    EIN2 = paste0( "EIN", 1:4 ), RETURN_TYPE = "990",
    F9_10_ASSET_CASH_EOY = c( 50, 200, 100, 10 ),
    F9_10_ASSET_CASH_BOY = c( 100, 100, 0, 10 )
  )
  expect_message( out <- get_cash_burn_ratio( df, sanitize = FALSE ), "1 case" )
  expect_equal( out$cash_burn[c( 1, 2, 4 )], c( 0.5, 2, 1 ) )
  expect_true( is.nan( out$cash_burn[3] ) )
})

test_that( "get_cash_burn_ratio() warns that months_in_period is ignored", {
  df <- make_test_df( n = 10 )
  expect_warning(
    out <- quiet_metric( get_cash_burn_ratio( df, months_in_period = 6, sanitize = FALSE ) ),
    "deprecated"
  )
  expect_equal( out$cash_burn, df$F9_10_ASSET_CASH_EOY / df$F9_10_ASSET_CASH_BOY )
})


# ---- field defaults verified against the NCCS efile concordance --------------

test_that( "get_land_assets_gross_ratio() uses cost basis (line 10a), not accumulated depreciation", {
  expect_equal( formals( get_land_assets_gross_ratio )$land_buildings, "F9_10_ASSET_LAND_BLDG" )
  df  <- make_test_df( n = 10 )
  out <- quiet_metric( get_land_assets_gross_ratio( df, sanitize = FALSE ) )
  expect_equal( out$land_assets_gross, df$F9_10_ASSET_LAND_BLDG / df$F9_10_ASSET_TOT_EOY )
})

test_that( "get_investment_income_ratio() uses net rent (6d) and net gain on asset sales (7d)", {
  f <- formals( get_investment_income_ratio )
  expect_equal( f$rent_income, "F9_08_REV_OTH_RENT_NET_TOT" )
  expect_equal( f$asset_sale_income, "F9_08_REV_OTH_SALE_GAIN_NET_TOT" )

  df  <- make_test_df( n = 10 )
  out <- quiet_metric( get_investment_income_ratio( df, sanitize = FALSE ) )
  expected <- ( df$F9_08_REV_OTH_INVEST_INCOME_TOT + df$F9_08_REV_OTH_INVEST_BOND_TOT +
                df$F9_08_REV_OTH_RENT_NET_TOT + df$F9_08_REV_OTH_SALE_GAIN_NET_TOT ) /
    df$F9_08_REV_TOT_TOT
  expect_equal( out$invest_income, expected )
})

test_that( "get_debt_equity_ratio() no longer defaults to a nonexistent Part I field", {
  expect_equal( formals( get_debt_equity_ratio )$equity, "F9_10_NAFB_UNRESTRICT_EOY" )
})


# ---- bundled data ------------------------------------------------------------

test_that( "dat10k is a 2023 sample that supports every registered metric", {
  data( dat10k, package = "fiscal", envir = environment() )
  expect_equal( nrow( dat10k ), 10000L )
  expect_true( all( dat10k$TAX_YEAR == 2023 ) )
  expect_setequal( unique( dat10k$RETURN_TYPE ), c( "990", "990EZ" ) )

  fields <- unique( unlist( lapply( fiscal_metrics()$function_name, function( fn ) {
    unlist( lapply( formals( get( fn ) ), function( a ) {
      v <- tryCatch( eval( a ), error = function( e ) NULL )
      if ( is.character( v ) ) v[ grepl( "^F9_", v ) ] else NULL
    } ) )
  } ) ) )
  expect_equal( setdiff( fields, names( dat10k ) ), character( 0 ) )

  scored <- suppressWarnings( suppressMessages(
    compute_all( dat10k[ 1:500, ], metrics = "ratio", append_to_df = FALSE ) ) )
  expect_true( all( fiscal_metrics()$metric %in% names( scored ) ) )
})
