# helper-fiscal.R
# Shared test fixtures loaded automatically by testthat before each test file.

# Minimal synthetic data frame that covers the most commonly used fields
# across all ratio functions.  Uses round numbers so expected values are easy
# to reason about.

make_test_df <- function( n = 10 ) {
  set.seed( 42 )
  data.frame(
    EIN2              = paste0( "EIN", seq_len(n) ),
    RETURN_TYPE       = rep( c("990", "990EZ"), length.out = n ),
    TAX_YEAR          = rep( 2021L, n ),

    # Part I summary (PZ scope)
    F9_01_REV_TOT_CY          = runif( n, 1e5, 1e7 ),
    F9_01_EXP_TOT_CY          = runif( n, 1e5, 1e7 ),
    F9_01_EXP_REV_LESS_EXP_CY = runif( n, -1e5, 1e5 ),
    F9_01_NAFB_TOT_EOY        = runif( n, 1e4, 1e6 ),
    F9_01_NAFB_TOT_BOY        = runif( n, 1e4, 1e6 ),
    F9_01_NAFB_ASSET_TOT_EOY  = runif( n, 1e5, 5e6 ),
    F9_01_NAFB_LIAB_TOT_EOY   = runif( n, 1e4, 2e6 ),
    F9_01_REV_PROG_TOT_CY     = runif( n, 0,   8e6 ),

    # Part VIII revenue (PC scope)
    F9_08_REV_TOT_TOT         = runif( n, 1e5, 1e7 ),
    F9_08_REV_PROG_TOT_TOT    = runif( n, 0,   8e6 ),
    F9_08_REV_CONTR_TOT       = runif( n, 0,   5e6 ),
    F9_08_REV_CONTR_GOVT_GRANT= runif( n, 0,   3e6 ),
    F9_08_REV_CONTR_MEMBSHIP_DUE = runif( n, 0, 5e5 ),
    F9_08_REV_OTH_FUNDR_NET_TOT  = runif( n, 0, 2e5 ),
    F9_08_REV_OTH_INVEST_INCOME_TOT = runif( n, 0, 5e5 ),
    F9_08_REV_OTH_INVEST_BOND_TOT   = runif( n, 0, 1e5 ),
    F9_08_REV_OTH_RENT_GRO_PERS     = runif( n, 0, 1e5 ),
    F9_08_REV_OTH_SALE_ASSET_OTH    = runif( n, 0, 2e5 ),
    F9_08_REV_OTH_ROY_TOT           = runif( n, 0, 5e4 ),
    F9_08_REV_MISC_OTH_TOT          = runif( n, 0, 5e4 ),
    F9_08_REV_CONTR_FED_CAMP        = runif( n, 0, 1e5 ),
    F9_08_REV_CONTR_FUNDR_EVNT      = runif( n, 0, 1e5 ),
    F9_08_REV_CONTR_RLTD_ORG        = runif( n, 0, 1e5 ),

    # Part IX expenses (PC scope)
    F9_09_EXP_TOT_TOT         = runif( n, 1e5, 1e7 ),
    F9_09_EXP_TOT_PROG        = runif( n, 5e4, 8e6 ),
    F9_09_EXP_TOT_MGMT        = runif( n, 1e4, 1e6 ),
    F9_09_EXP_TOT_FUNDR       = runif( n, 0,   5e5 ),
    F9_09_EXP_DEPREC_TOT      = runif( n, 0,   5e5 ),
    F9_09_EXP_GRANT_US_ORG_TOT    = runif( n, 0, 2e6 ),
    F9_09_EXP_GRANT_US_INDIV_TOT  = runif( n, 0, 5e5 ),
    F9_09_EXP_GRANT_FRGN_TOT      = runif( n, 0, 1e5 ),
    F9_09_EXP_BEN_PAID_MEMB_TOT   = runif( n, 0, 1e5 ),
    F9_09_EXP_PAY_AFFIL_TOT       = runif( n, 0, 5e5 ),
    F9_09_EXP_COMP_DTK_TOT        = runif( n, 0, 5e5 ),
    F9_09_EXP_COMP_DSQ_PERS_TOT   = runif( n, 0, 1e5 ),
    F9_09_EXP_OTH_SAL_WAGE_TOT    = runif( n, 5e4, 5e6 ),
    F9_09_EXP_PENSION_CONTR_TOT   = runif( n, 0, 2e5 ),
    F9_09_EXP_OTH_EMPL_BEN_TOT    = runif( n, 0, 3e5 ),
    F9_09_EXP_PAYROLL_TAX_TOT     = runif( n, 0, 2e5 ),
    F9_09_EXP_FEE_SVC_MGMT_TOT    = runif( n, 0, 1e5 ),
    F9_09_EXP_FEE_SVC_LEGAL_TOT   = runif( n, 0, 5e4 ),
    F9_09_EXP_FEE_SVC_ACC_TOT     = runif( n, 0, 5e4 ),
    F9_09_EXP_FEE_SVC_LOB_TOT     = runif( n, 0, 2e4 ),
    F9_09_EXP_FEE_SVC_FUNDR_TOT   = runif( n, 0, 3e4 ),
    F9_09_EXP_FEE_SVC_INVEST_TOT  = runif( n, 0, 3e4 ),
    F9_09_EXP_FEE_SVC_OTH_TOT     = runif( n, 0, 5e4 ),

    # Part X balance sheet (PC scope, with PZ fallbacks)
    F9_10_ASSET_CASH_EOY          = runif( n, 1e4, 5e5 ),
    F9_10_ASSET_CASH_BOY          = runif( n, 1e4, 5e5 ),
    F9_10_ASSET_SAVING_EOY        = runif( n, 0,   3e5 ),
    F9_10_ASSET_PLEDGE_NET_EOY    = runif( n, 0,   2e5 ),
    F9_10_ASSET_ACC_NET_EOY       = runif( n, 0,   3e5 ),
    F9_10_ASSET_INV_SALE_EOY      = runif( n, 0,   5e5 ),
    F9_10_ASSET_EXP_PREPAID_EOY   = runif( n, 0,   5e4 ),
    F9_10_ASSET_INVEST_SEC_EOY    = runif( n, 0,   2e6 ),
    F9_10_ASSET_INVEST_SEC_OTH_EOY= runif( n, 0,   1e6 ),
    F9_10_ASSET_LAND_BLDG_NET_EOY = runif( n, 0,   3e6 ),
    F9_10_ASSET_LAND_BLDG_DEPREC  = runif( n, 0,   1e6 ),
    F9_10_ASSET_TOT_EOY           = runif( n, 2e5, 1e7 ),
    F9_10_LIAB_ACC_PAYABLE_EOY    = runif( n, 1e3, 2e5 ),
    F9_10_LIAB_GRANT_PAYABLE_EOY  = runif( n, 0,   1e5 ),
    F9_10_LIAB_MTG_NOTE_EOY       = runif( n, 0,   2e6 ),
    F9_10_LIAB_NOTE_UNSEC_EOY     = runif( n, 0,   5e5 ),
    F9_10_LIAB_TOT_EOY            = runif( n, 1e4, 3e6 ),
    F9_10_NAFB_UNRESTRICT_EOY     = runif( n, 5e3, 2e6 ),
    F9_10_NAFB_UNRESTRICT_BOY     = runif( n, 5e3, 2e6 ),
    F9_10_NAFB_TOT_EOY            = runif( n, 1e4, 4e6 ),
    F9_10_NAFB_TOT_BOY            = runif( n, 1e4, 4e6 ),
    stringsAsFactors              = FALSE
  )
}

# A small df with known zero-denominator cases for NaN testing
make_zero_denom_df <- function() {
  df <- make_test_df( n = 5 )
  df$F9_10_LIAB_TOT_EOY[1] <- 0   # debt_secured denominator
  df$F9_10_ASSET_TOT_EOY[2] <- 0  # debt_assets denominator
  df$F9_08_REV_TOT_TOT[3]   <- 0  # revenue denominator
  df$F9_09_EXP_TOT_TOT[4]   <- 0  # expense denominator
  df
}
