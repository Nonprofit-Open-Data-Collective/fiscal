test_that("fiscal_dimensions proposes loading-based PCA clusters", {
  skip_if_not_installed("psych")
  set.seed(10)
  n <- 500
  f1 <- rnorm(n); f2 <- rnorm(n)
  dat <- data.frame(
    liquidity_a = f1 + rnorm(n, sd = .15),
    liquidity_b = f1 + rnorm(n, sd = .15),
    solvency_a = f2 + rnorm(n, sd = .15),
    solvency_b = f2 + rnorm(n, sd = .15)
  )
  out <- fiscal_dimensions(dat, variables = names(dat), method = "pca",
                           nfactors = 2, rotate = "varimax")

  expect_s3_class(out, "fiscal_dimensions")
  expect_equal(out$nfactors, 2L)
  expect_equal(nrow(out$assignments), 4L)
  expect_equal(length(out$multi_indicator_dimensions), 2L)
  expect_length(out$unassigned_indicators, 0L)
  expect_true(all(c("Dimension1", "Dimension2") %in% names(out$loadings)))
})

test_that("fiscal_dimensions carries triage standalones separately", {
  skip_if_not_installed("psych")
  set.seed(11)
  n <- 300
  f1 <- rnorm(n); f2 <- rnorm(n)
  dat <- data.frame(a = f1 + rnorm(n, sd = .2),
                    b = f1 + rnorm(n, sd = .2),
                    c = f2 + rnorm(n, sd = .2),
                    d = f2 + rnorm(n, sd = .2),
                    special = rnorm(n))
  diag <- fiscal_diagnostics(dat, corr_cutoff = .99)
  tri <- fiscal_triage(diag, overrides = c(special = "standalone"))
  out <- fiscal_dimensions(dat, triage = tri, nfactors = 2, rotate = "varimax")

  expect_true("special" %in% out$triage_standalone)
  expect_false("special" %in% out$variables)
  expect_false("special" %in% out$assignments$variable)
})

test_that("fiscal_dimensions records missing-data handling", {
  skip_if_not_installed("psych")
  set.seed(12)
  dat <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  dat$a[1:5] <- NA

  complete <- fiscal_dimensions(dat, nfactors = 1, rotate = "none",
                                missing = "complete")
  median <- fiscal_dimensions(dat, nfactors = 1, rotate = "none",
                              missing = "median")

  expect_length(complete$rows_used, 95L)
  expect_length(median$rows_used, 100L)
  expect_true(is.finite(median$missing$medians[["a"]]))
  expect_true(is.na(complete$missing$medians[["a"]]))
})

test_that("fiscal_dimensions validates its contract", {
  skip_if_not_installed("psych")
  dat <- data.frame(a = 1:20, b = (1:20) + rep(c(0, .1), 10))
  expect_error(fiscal_dimensions(dat, variables = "a", nfactors = 1),
               "At least two")
  expect_error(fiscal_dimensions(dat, variables = c("a", "missing"), nfactors = 1),
               "Unknown variables")
})
