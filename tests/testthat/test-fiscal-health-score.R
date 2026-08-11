make_scoring_fit <- function(n = 200) {
  set.seed(30)
  f <- rnorm(n)
  dat <- data.frame(
    current_z = f + rnorm(n, sd = .2),
    quick_z = f + rnorm(n, sd = .2),
    custom = rnorm(n)
  )
  fit <- fiscal_health_fit(
    dat, list(liquidity = c("current_z", "quick_z")),
    standalone = "custom",
    indicator_direction = c(custom = "higher")
  )
  list(data = dat, fit = fit)
}

test_that("fiscal_health_score applies stored parameters", {
  fixture <- make_scoring_fit()
  scored <- fiscal_health_score(fixture$data, fixture$fit)

  expect_s3_class(scored, "fiscal_health_scores")
  expect_true(all(c("fh_liquidity", "fh_custom",
                    "fh_liquidity_coverage", "fh_custom_coverage") %in% names(scored)))
  expect_equal(mean(scored$fh_liquidity), 0, tolerance = 1e-10)
  expect_equal(sd(scored$fh_liquidity), 1, tolerance = 1e-10)
  expect_equal(mean(scored$fh_custom), 0, tolerance = 1e-10)
  expect_equal(sd(scored$fh_custom), 1, tolerance = 1e-10)
  expect_true(all(scored$fh_liquidity_coverage == 1))
})

test_that("fiscal_health_score never refits on new data", {
  fixture <- make_scoring_fit()
  shifted <- fixture$data
  shifted$current_z <- shifted$current_z + 10
  scored <- fiscal_health_score(shifted, fixture$fit, include_coverage = FALSE)

  expect_gt(mean(scored$fh_liquidity), 1)
  expect_identical(attr(scored, "fiscal_health_fit"), fixture$fit)
})

test_that("fiscal_health_score distinguishes median and complete rules", {
  fixture <- make_scoring_fit()
  new <- fixture$data[1:5, ]
  new$current_z[1] <- NA
  new$quick_z[2] <- NA

  median <- fiscal_health_score(new, fixture$fit, missing = "median",
                                min_coverage = .5)
  complete <- fiscal_health_score(new, fixture$fit, missing = "complete")
  strict <- fiscal_health_score(new, fixture$fit, missing = "median",
                                min_coverage = 1)

  expect_true(all(is.finite(median$fh_liquidity)))
  expect_true(all(is.na(complete$fh_liquidity[1:2])))
  expect_true(all(is.na(strict$fh_liquidity[1:2])))
  expect_equal(median$fh_liquidity_coverage[1:2], c(.5, .5))
})

test_that("fiscal_health_score returns audit information", {
  fixture <- make_scoring_fit()
  scored <- fiscal_health_score(fixture$data, fixture$fit, append = FALSE,
                                include_standalone = FALSE)
  audit <- attr(scored, "fiscal_health_audit")

  expect_equal(names(scored), c("fh_liquidity", "fh_liquidity_coverage"))
  expect_equal(audit$measure, "liquidity")
  expect_equal(audit$n_scored, nrow(fixture$data))
  expect_equal(audit$mean_coverage, 1)
})

test_that("fiscal_health_score protects existing columns", {
  fixture <- make_scoring_fit()
  fixture$data$fh_liquidity <- 999
  expect_error(fiscal_health_score(fixture$data, fixture$fit), "already exist")
  scored <- fiscal_health_score(fixture$data, fixture$fit, overwrite = TRUE)
  expect_false(all(scored$fh_liquidity == 999))
})

test_that("fiscal_health_score validates required indicators", {
  fixture <- make_scoring_fit()
  bad <- fixture$data[names(fixture$data) != "quick_z"]
  expect_error(fiscal_health_score(bad, fixture$fit), "missing required")
  expect_error(fiscal_health_score(fixture$data, fixture$fit, min_coverage = 1.1),
               "between 0 and 1")
})
