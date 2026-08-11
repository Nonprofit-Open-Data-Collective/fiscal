make_health_fit_data <- function(n = 400) {
  set.seed(20)
  liquidity <- rnorm(n)
  solvency <- rnorm(n)
  data.frame(
    current_z = liquidity + rnorm(n, sd = .2),
    quick_z = liquidity + rnorm(n, sd = .2),
    debt_assets_z = solvency + rnorm(n, sd = .2),
    debt_equity_z = solvency + rnorm(n, sd = .2),
    custom_indicator = rnorm(n)
  )
}

test_that("fiscal_health_fit creates a reusable named specification", {
  dat <- make_health_fit_data()
  groups <- list(
    liquidity = c("current_z", "quick_z"),
    solvency = c("debt_assets_z", "debt_equity_z")
  )
  fit <- fiscal_health_fit(
    dat, groups,
    scoring = c(liquidity = "standardized_mean", solvency = "pca"),
    standalone = "custom_indicator",
    indicator_direction = c(custom_indicator = "higher")
  )

  expect_s3_class(fit, "fiscal_health_fit")
  expect_equal(names(fit$dimensions), c("liquidity", "solvency"))
  expect_equal(fit$dimensions$liquidity$method, "standardized_mean")
  expect_equal(fit$dimensions$solvency$method, "pca")
  expect_equal(sum(fit$dimensions$liquidity$coefficients), 1)
  expect_true(fit$dimensions$solvency$indicator_multiplier[["debt_assets_z"]] < 0)
  expect_true("custom_indicator" %in% names(fit$standalone))
  expect_equal(nrow(fit$metadata), 3L)
})

test_that("fiscal_health_fit consumes discovered dimensions and renames them", {
  skip_if_not_installed("psych")
  dat <- make_health_fit_data()
  discovery <- fiscal_dimensions(
    dat, variables = c("current_z", "quick_z", "debt_assets_z", "debt_equity_z"),
    nfactors = 2, rotate = "varimax"
  )
  old <- discovery$multi_indicator_dimensions
  expect_length(old, 2L)
  new <- setNames(c("dimension_a", "dimension_b"), old)
  fit <- fiscal_health_fit(dat, discovery, dimension_names = new)

  expect_equal(sort(names(fit$dimensions)), sort(unname(new)))
  expect_identical(fit$discovery, discovery)
})

test_that("fiscal_health_fit stores explicit median treatment", {
  dat <- make_health_fit_data(100)
  dat$current_z[1:10] <- NA
  fit <- fiscal_health_fit(dat,
    list(liquidity = c("current_z", "quick_z")), missing = "median")

  spec <- fit$dimensions$liquidity
  expect_true(is.finite(spec$indicator_median[["current_z"]]))
  expect_length(spec$rows_used, 100L)
  expect_true(is.finite(spec$score_scale))
})

test_that("fiscal_health_fit handles direction uncertainty explicitly", {
  dat <- make_health_fit_data()
  groups <- list(custom = c("current_z", "custom_indicator"))
  fit <- fiscal_health_fit(dat, groups)
  expect_length(fit$warnings, 1L)
  expect_error(fiscal_health_fit(dat, groups, strict_direction = TRUE),
               "context-dependent or unknown")
  resolved <- fiscal_health_fit(dat, groups,
    indicator_direction = c(custom_indicator = "lower"), strict_direction = TRUE)
  expect_length(resolved$warnings, 0L)
  expect_equal(resolved$dimensions$custom$indicator_multiplier[["custom_indicator"]], -1)
})

test_that("fiscal_health_fit validates overlapping indicators", {
  dat <- make_health_fit_data()
  groups <- list(one = c("current_z", "quick_z"),
                 two = c("quick_z", "debt_assets_z"))
  expect_error(fiscal_health_fit(dat, groups), "multiple dimensions")
  expect_error(fiscal_health_fit(dat,
    list(one = c("current_z", "quick_z")), standalone = "current_z"),
    "both modeled and standalone")
})
