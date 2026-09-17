#' Fiscal metric registry
#'
#' Returns the canonical relationship between calculation functions, output
#' prefixes, conceptual categories, and score direction. `direction` is
#' `"higher"`, `"lower"`, or `"context"`; context-dependent composition
#' measures should not be included in a directional score without an explicit
#' scoring rule.
#'
#' @return A data frame with one row per fiscal metric.
#' @export
fiscal_metrics <- function() {
  function_name <- c(
    "get_current_ratio", "get_quick_ratio", "get_cash_liquidity_ratio",
    "get_cash_assets_ratio", "get_cash_on_hand", "get_cash_burn_ratio",
    "get_days_cash_operations", "get_days_cash_investments",
    "get_months_cash_operations", "get_liquid_assets_months",
    "get_debt_assets_ratio", "get_debt_equity_ratio", "get_debt_netassets_ratio",
    "get_debt_shortterm_ratio", "get_debt_secured_ratio", "get_debt_unsecured_ratio",
    "get_equity_ratio", "get_surplus_margin_ratio", "get_return_assets_ratio",
    "get_return_netassets_ratio", "get_profit_margin_postdepr",
    "get_profit_margin_predepr", "get_self_sufficiency_ratio",
    "get_program_expenses_ratio", "get_overhead_ratio", "get_expenses_admin_ratio",
    "get_fundraising_efficiency_ratio", "get_expenses_compensation_ratio",
    "get_expenses_grants_ratio", "get_expenses_membbenefits_ratio",
    "get_expenses_feesforservice_ratio", "get_expenses_affiliates_ratio",
    "get_grants_govt_ratio", "get_donations_revenue_ratio",
    "get_earned_income_ratio", "get_revenue_programs_ratio",
    "get_revenue_fedcampaign_ratio", "get_revenue_membdues_ratio",
    "get_revenue_fundevents_ratio", "get_revenue_reltdorgs_ratio",
    "get_investment_income_ratio", "get_netassets_composition_ratio",
    "get_netassets_growth_ratio", "get_operating_reserve_ratio",
    "get_investments_assets_ratio", "get_land_assets_net_ratio",
    "get_land_assets_gross_ratio", "get_assets_revenue_ratio"
  )
  metric <- c(
    "current", "quick", "cash_liq", "cash_assets", "cash_on_hand", "cash_burn",
    "days_cash_ops", "days_cash_inv", "months_cash_ops", "liquid_assets_months",
    "debt_assets", "debt_equity", "debt_netassets", "debt_shortterm",
    "debt_secured", "debt_unsecured", "equity", "surplus_margin",
    "return_assets", "return_netassets", "profit_postdepr", "profit_predepr",
    "self_suff", "prog_exp", "overhead", "expenses_admin", "fundr_eff",
    "expenses_compensation", "expenses_grants", "expenses_membbenefits",
    "expenses_feesforservice", "expenses_affiliates", "grants_govt",
    "donations_rev", "earned_income", "revenue_programs", "revenue_fedcampaign",
    "revenue_membdues", "revenue_fundevents", "revenue_reltdorgs",
    "invest_income", "netassets_comp", "netassets_growth", "op_reserve",
    "investments_assets", "land_assets_net", "land_assets_gross", "assets_rev"
  )
  category <- c(
    rep("liquidity", 10), rep("solvency", 7), rep("performance", 6),
    rep("expense_structure", 9), rep("revenue_structure", 9),
    rep("asset_structure", 7)
  )
  # Named by metric so each direction can be checked against its function's
  # documentation. "lower" = lower is healthier; "context" = no documented
  # benchmark direction (composition measures).
  direction <- c(
    # liquidity
    current = "higher", quick = "higher", cash_liq = "higher",
    cash_assets = "higher", cash_on_hand = "higher", cash_burn = "higher",
    days_cash_ops = "higher", days_cash_inv = "higher",
    months_cash_ops = "higher", liquid_assets_months = "higher",
    # solvency
    debt_assets = "lower", debt_equity = "lower", debt_netassets = "lower",
    debt_shortterm = "lower",
    debt_secured = "context",    # liability composition; "no standard benchmark"
    debt_unsecured = "context",  # "not inherently negative"
    equity = "higher",
    # performance
    surplus_margin = "higher", return_assets = "higher",
    return_netassets = "higher", profit_postdepr = "higher",
    profit_predepr = "higher", self_suff = "higher",
    # expense structure
    prog_exp = "higher", overhead = "lower", expenses_admin = "lower",
    fundr_eff = "lower",         # cost per dollar raised
    expenses_compensation = "context", expenses_grants = "context",
    expenses_membbenefits = "context", expenses_feesforservice = "context",
    expenses_affiliates = "context",
    # revenue structure
    grants_govt = "context", donations_rev = "context",
    earned_income = "context", revenue_programs = "context",
    revenue_fedcampaign = "context", revenue_membdues = "context",
    revenue_fundevents = "context", revenue_reltdorgs = "context",
    invest_income = "context",
    # asset structure
    netassets_comp = "context", netassets_growth = "higher",
    op_reserve = "higher", investments_assets = "context",
    land_assets_net = "context", land_assets_gross = "context",
    assets_rev = "lower"
  )
  stopifnot(length(function_name) == length(metric),
            length(metric) == length(category),
            identical(names(direction), metric))
  data.frame(
    function_name = function_name,
    metric = metric,
    category = category,
    direction = unname(direction),
    raw = metric,
    winsorized = paste0(metric, "_w"),
    standardized = paste0(metric, "_z"),
    percentile = paste0(metric, "_p"),
    stringsAsFactors = FALSE
  )
}
