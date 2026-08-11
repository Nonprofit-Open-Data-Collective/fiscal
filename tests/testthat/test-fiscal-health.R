make_health_wrapper_data <- function(n = 150) {
  set.seed(40)
  f1 <- rnorm(n); f2 <- rnorm(n)
  data.frame(
    current_z = f1 + rnorm(n, sd = .2),
    quick_z = f1 + rnorm(n, sd = .2),
    debt_assets_z = f2 + rnorm(n, sd = .2),
    debt_equity_z = f2 + rnorm(n, sd = .2),
    custom = rnorm(n)
  )
}

test_that("fiscal_health fits, scores, and aggregates reviewed dimensions", {
  dat <- make_health_wrapper_data()
  groups <- list(liquidity = c("current_z", "quick_z"),
                 solvency = c("debt_assets_z", "debt_equity_z"))
  result <- fiscal_health(
    dat, dimensions = groups,
    fit_args = list(standalone = "custom",
                    indicator_direction = c(custom = "higher")),
    weights = c(liquidity = 2, solvency = 2, custom = 1),
    weight_args = list(min_coverage = .8)
  )

  expect_s3_class(result, "fiscal_health_result")
  expect_s3_class(result$fit, "fiscal_health_fit")
  expect_s3_class(result$weights, "fiscal_weights")
  expect_true(all(c("fh_liquidity", "fh_solvency", "fh_custom",
                    "fiscal_health") %in% names(result$data)))
  expect_equal(result$audit$mode, "fit_and_score")
  expect_equal(result$weights$weights, c(liquidity = .4, solvency = .4, custom = .2))
})

test_that("fiscal_health scores new data using an existing fit", {
  dat <- make_health_wrapper_data()
  groups <- list(liquidity = c("current_z", "quick_z"),
                 solvency = c("debt_assets_z", "debt_equity_z"))
  first <- fiscal_health(dat, dimensions = groups,
                         fit_args = list(standalone = "custom",
                           indicator_direction = c(custom = "higher")))
  shifted <- dat
  shifted$current_z <- shifted$current_z + 5
  second <- fiscal_health(shifted, fit = first$fit, weights = first$weights)

  expect_equal(second$audit$mode, "score_existing_fit")
  expect_identical(second$fit, first$fit)
  expect_gt(mean(second$data$fh_liquidity), mean(first$data$fh_liquidity))
})

test_that("fiscal_health can return dimension scores without an index", {
  dat <- make_health_wrapper_data()
  groups <- list(liquidity = c("current_z", "quick_z"))
  result <- fiscal_health(dat, dimensions = groups, index = FALSE)

  expect_null(result$weights)
  expect_true("fh_liquidity" %in% names(result$data))
  expect_false("fiscal_health" %in% names(result$data))
  expect_null(result$audit$index)
})

test_that("fiscal_health keeps custom score and index prefixes aligned", {
  dat <- make_health_wrapper_data()
  groups <- list(liquidity = c("current_z", "quick_z"))
  result <- fiscal_health(dat, dimensions = groups,
                          score_args = list(prefix = "score_"))
  expect_true(all(c("score_liquidity", "fiscal_health") %in% names(result$data)))
})

test_that("fiscal_health requires reviewed dimensions or an existing fit", {
  dat <- make_health_wrapper_data()
  expect_error(fiscal_health(dat), "reviewed `dimensions`")
  expect_error(fiscal_health(dat,
    dimensions = list(liquidity = c("current_z", "quick_z")),
    index = FALSE, weights = c(liquidity = 1)),
    "require `index = TRUE`")
})

test_that("fiscal_health protects reserved orchestration arguments", {
  dat <- make_health_wrapper_data()
  groups <- list(liquidity = c("current_z", "quick_z"))
  expect_error(fiscal_health(dat, dimensions = groups,
                             score_args = list(data = dat)),
               "reserved score")
  fit <- fiscal_health_fit(dat, groups)
  expect_error(fiscal_health(dat, dimensions = groups, fit = fit),
               "Do not supply `dimensions`")
})
