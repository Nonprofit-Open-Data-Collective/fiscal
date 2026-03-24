# test-ratio-structure.R
# Tests that every get_*() function satisfies structural invariants:
# - returns a data.frame
# - has the same number of rows as the input
# - appends exactly 4 output columns (raw, _w, _z, _p)
# - raw column is numeric
# - _w values are within the _z distribution support
# - _p values are integer 1-100

library( fiscal )

df <- make_test_df()

# ---- helper: run one function and check structure -------------------------

check_ratio_structure <- function( fn, df, col_prefix, ... ) {

  result <- suppressMessages( fn( df, ..., sanitize = TRUE ) )

  # Returns a data.frame
  expect_true( is.data.frame(result),
               label = paste0( col_prefix, ": result is data.frame" ) )

  # Same number of rows
  expect_equal( nrow(result), nrow(df),
                label = paste0( col_prefix, ": nrow preserved" ) )

  # Has at least as many columns as df + 4 new ones
  expect_gte( ncol(result), ncol(df) + 4,
              label = paste0( col_prefix, ": at least 4 new columns" ) )

  # Check the 4 output columns exist
  raw_col <- col_prefix
  w_col   <- paste0( col_prefix, "_w" )
  z_col   <- paste0( col_prefix, "_z" )
  p_col   <- paste0( col_prefix, "_p" )

  expect_true( raw_col %in% names(result),
               label = paste0( col_prefix, ": raw column exists" ) )
  expect_true( w_col   %in% names(result),
               label = paste0( col_prefix, ": _w column exists" ) )
  expect_true( z_col   %in% names(result),
               label = paste0( col_prefix, ": _z column exists" ) )
  expect_true( p_col   %in% names(result),
               label = paste0( col_prefix, ": _p column exists" ) )

  # Raw and winsorized are numeric
  expect_true( is.numeric( result[[raw_col]] ),
               label = paste0( col_prefix, ": raw is numeric" ) )
  expect_true( is.numeric( result[[w_col]] ),
               label = paste0( col_prefix, ": _w is numeric" ) )

  # Percentile is integer-valued 1-100 (non-NA values)
  p_vals <- result[[p_col]]
  p_valid <- p_vals[ !is.na(p_vals) ]
  if ( length(p_valid) > 0 ) {
    expect_true( all( p_valid >= 1L & p_valid <= 100L ),
                 label = paste0( col_prefix, ": _p in [1,100]" ) )
    expect_true( all( p_valid == as.integer(p_valid) ),
                 label = paste0( col_prefix, ": _p is integer-valued" ) )
  }

  invisible(result)
}


# ---- Liquidity ratios ----------------------------------------------------

test_that( "get_current_ratio() returns correct structure", {
  check_ratio_structure( get_current_ratio, df, "current_ratio" )
})

test_that( "get_quick_ratio() returns correct structure", {
  check_ratio_structure( get_quick_ratio, df, "quick_ratio" )
})

test_that( "get_cash_liquidity_ratio() returns correct structure", {
  check_ratio_structure( get_cash_liquidity_ratio, df, "cash_liquidity" )
})

test_that( "get_cash_assets_ratio() returns correct structure", {
  check_ratio_structure( get_cash_assets_ratio, df, "cash_assets" )
})

test_that( "get_cash_on_hand() returns correct structure", {
  check_ratio_structure( get_cash_on_hand, df, "cash_on_hand" )
})

test_that( "get_cash_burn_ratio() returns correct structure", {
  check_ratio_structure( get_cash_burn_ratio, df, "cash_burn" )
})

test_that( "get_days_cash_operations() returns correct structure", {
  check_ratio_structure( get_days_cash_operations, df, "days_cash_ops" )
})

test_that( "get_months_cash_operations() returns correct structure", {
  check_ratio_structure( get_months_cash_operations, df, "months_cash_ops" )
})

test_that( "get_liquid_assets_months() returns correct structure", {
  check_ratio_structure( get_liquid_assets_months, df, "liquid_assets_months" )
})


# ---- Leverage & solvency ratios ------------------------------------------

test_that( "get_debt_assets_ratio() returns correct structure", {
  check_ratio_structure( get_debt_assets_ratio, df, "debt_assets" )
})

test_that( "get_debt_equity_ratio() returns correct structure", {
  check_ratio_structure( get_debt_equity_ratio, df, "debt_equity" )
})

test_that( "get_debt_netassets_ratio() returns correct structure", {
  check_ratio_structure( get_debt_netassets_ratio, df, "debt_netassets" )
})

test_that( "get_debt_shortterm_ratio() returns correct structure", {
  check_ratio_structure( get_debt_shortterm_ratio, df, "debt_shortterm" )
})

test_that( "get_debt_secured_ratio() returns correct structure", {
  check_ratio_structure( get_debt_secured_ratio, df, "debt_secured" )
})

test_that( "get_debt_unsecured_ratio() returns correct structure", {
  check_ratio_structure( get_debt_unsecured_ratio, df, "debt_unsecured" )
})

test_that( "get_equity_ratio() returns correct structure", {
  check_ratio_structure( get_equity_ratio, df, "equity_ratio" )
})


# ---- Performance & profitability ratios ----------------------------------

test_that( "get_surplus_margin_ratio() returns correct structure", {
  check_ratio_structure( get_surplus_margin_ratio, df, "surplus_margin" )
})

test_that( "get_return_assets_ratio() returns correct structure", {
  check_ratio_structure( get_return_assets_ratio, df, "return_assets" )
})

test_that( "get_return_netassets_ratio() returns correct structure", {
  check_ratio_structure( get_return_netassets_ratio, df, "return_netassets" )
})

test_that( "get_profit_margin_postdepr() returns correct structure", {
  check_ratio_structure( get_profit_margin_postdepr, df, "profit_postdepr" )
})

test_that( "get_profit_margin_predepr() returns correct structure", {
  check_ratio_structure( get_profit_margin_predepr, df, "profit_predepr" )
})

test_that( "get_self_sufficiency_ratio() returns correct structure", {
  check_ratio_structure( get_self_sufficiency_ratio, df, "self_suff" )
})


# ---- Expense ratios ------------------------------------------------------

test_that( "get_program_expenses_ratio() returns correct structure", {
  check_ratio_structure( get_program_expenses_ratio, df, "program_exp_ratio" )
})

test_that( "get_overhead_ratio() returns correct structure", {
  check_ratio_structure( get_overhead_ratio, df, "overhead_ratio" )
})

test_that( "get_expenses_admin_ratio() returns correct structure", {
  check_ratio_structure( get_expenses_admin_ratio, df, "exp_admin_ratio" )
})

test_that( "get_fundraising_efficiency_ratio() returns correct structure", {
  check_ratio_structure( get_fundraising_efficiency_ratio, df, "fundraising_efficiency" )
})

test_that( "get_expenses_compensation_ratio() returns correct structure", {
  check_ratio_structure( get_expenses_compensation_ratio, df, "exp_compensation" )
})

test_that( "get_expenses_grants_ratio() returns correct structure", {
  check_ratio_structure( get_expenses_grants_ratio, df, "exp_grants" )
})


# ---- Revenue composition ratios ------------------------------------------

test_that( "get_grants_govt_ratio() returns correct structure", {
  check_ratio_structure( get_grants_govt_ratio, df, "grants_govt" )
})

test_that( "get_donations_revenue_ratio() returns correct structure", {
  check_ratio_structure( get_donations_revenue_ratio, df, "donations_revenue" )
})

test_that( "get_earned_income_ratio() returns correct structure", {
  check_ratio_structure( get_earned_income_ratio, df, "earned_income" )
})

test_that( "get_revenue_programs_ratio() returns correct structure", {
  check_ratio_structure( get_revenue_programs_ratio, df, "revenue_programs" )
})

test_that( "get_investment_income_ratio() returns correct structure", {
  check_ratio_structure( get_investment_income_ratio, df, "investment_income" )
})


# ---- Balance sheet ratios ------------------------------------------------

test_that( "get_netassets_composition_ratio() returns correct structure", {
  check_ratio_structure( get_netassets_composition_ratio, df, "netassets_composition" )
})

test_that( "get_netassets_growth_ratio() returns correct structure", {
  check_ratio_structure( get_netassets_growth_ratio, df, "netassets_growth" )
})

test_that( "get_operating_reserve_ratio() returns correct structure", {
  check_ratio_structure( get_operating_reserve_ratio, df, "operating_reserve" )
})

test_that( "get_investments_assets_ratio() returns correct structure", {
  check_ratio_structure( get_investments_assets_ratio, df, "investments_assets" )
})

test_that( "get_land_assets_net_ratio() returns correct structure", {
  check_ratio_structure( get_land_assets_net_ratio, df, "land_assets_net" )
})

test_that( "get_assets_revenue_ratio() returns correct structure", {
  check_ratio_structure( get_assets_revenue_ratio, df, "assets_revenue" )
})
