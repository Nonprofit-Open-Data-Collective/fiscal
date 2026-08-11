make_weights_fixture <- function(n = 100) {
  set.seed(31)
  f <- rnorm(n)
  dat <- data.frame(current_z = f + rnorm(n, sd = .2),
                    quick_z = f + rnorm(n, sd = .2),
                    custom = rnorm(n))
  fit <- fiscal_health_fit(
    dat, list(liquidity = c("current_z", "quick_z")),
    standalone = "custom", indicator_direction = c(custom = "higher"))
  list(data = dat, fit = fit)
}

test_that("fiscal_weights creates equal fit-based weights", {
  fixture <- make_weights_fixture()
  w <- fiscal_weights(fixture$fit)

  expect_s3_class(w, "fiscal_weights")
  expect_equal(names(w$weights), c("liquidity", "custom"))
  expect_equal(unname(w$weights), c(.5, .5))
  expect_equal(w$table$measure_type, c("dimension", "standalone"))
})

test_that("fiscal_weights validates and normalizes custom weights", {
  fixture <- make_weights_fixture()
  w <- fiscal_weights(fixture$fit, weights = c(liquidity = 3, custom = 1),
                      missing = "reweight", min_coverage = .5)

  expect_equal(unname(w$weights), c(.75, .25))
  expect_equal(w$table$original_weight, c(3, 1))
  expect_equal(w$min_coverage, .5)
  expect_error(fiscal_weights(fixture$fit, weights = c(liquidity = 1)),
               "every included measure")
  expect_error(fiscal_weights(weights = c(a = -1, b = 2)), "nonnegative")
})

test_that("fiscal_weights can exclude standalone indicators", {
  fixture <- make_weights_fixture()
  w <- fiscal_weights(fixture$fit, include_standalone = FALSE)
  expect_equal(names(w$weights), "liquidity")
  expect_equal(unname(w$weights), 1)
})

test_that("fiscal_health_index applies custom weights", {
  dat <- data.frame(fh_a = c(1, 2), fh_b = c(3, 4))
  w <- fiscal_weights(weights = c(a = 3, b = 1))
  out <- fiscal_health_index(dat, w)

  expect_equal(out$fiscal_health, c(1.5, 2.5))
  expect_equal(out$fiscal_health_coverage, c(1, 1))
  expect_s3_class(attr(out, "fiscal_weights"), "fiscal_weights")
})

test_that("fiscal_health_index reports and enforces weighted coverage", {
  dat <- data.frame(fh_a = c(1, NA, 1), fh_b = c(3, 3, NA))
  w <- fiscal_weights(weights = c(a = 3, b = 1),
                      missing = "reweight", min_coverage = .7)
  out <- fiscal_health_index(dat, w)

  expect_equal(out$fiscal_health[1], 1.5)
  expect_true(is.na(out$fiscal_health[2]))
  expect_equal(out$fiscal_health[3], 1)
  expect_equal(out$fiscal_health_coverage, c(1, .25, .75))

  complete_w <- fiscal_weights(weights = c(a = 3, b = 1),
                               missing = "complete", min_coverage = 0)
  complete <- fiscal_health_index(dat, complete_w)
  expect_true(all(is.na(complete$fiscal_health[2:3])))
})

test_that("fiscal_health_index protects its output contract", {
  dat <- data.frame(fh_a = 1:3, fiscal_health = 999)
  w <- fiscal_weights(weights = c(a = 1))
  expect_error(fiscal_health_index(dat, w), "already exist")
  out <- fiscal_health_index(dat, w, overwrite = TRUE, append = FALSE)
  expect_equal(out$fiscal_health, 1:3)
  expect_equal(names(out), c("fiscal_health", "fiscal_health_coverage"))
})
