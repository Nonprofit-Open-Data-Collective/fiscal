test_that("fiscal_diagnostics excludes self-correlations", {
  set.seed(1)
  x <- rnorm(200)
  dat <- data.frame(a = x, b = x + rnorm(200, sd = 0.01), c = rnorm(200))
  out <- fiscal_diagnostics(dat, corr_cutoff = 0.9)

  expect_s3_class(out, "fiscal_diagnostics")
  expect_equal(out$variables$mean_abs_cor[out$variables$variable == "a"],
               mean(abs(c(cor(dat$a, dat$b), cor(dat$a, dat$c)))),
               tolerance = 1e-8)
  expect_equal(nrow(out$redundancy_pairs), 1L)
  expect_equal(sort(unname(unlist(out$redundancy_pairs[1, c("var1", "var2")]))), c("a", "b"))
})

test_that("fiscal_diagnostics retains zero-variance variables in its audit", {
  dat <- data.frame(a = 1:20, b = seq(2, 40, 2), constant = 1)
  out <- fiscal_diagnostics(dat)
  row <- out$variables[out$variables$variable == "constant", ]

  expect_true(row$flag_zero_variance)
  expect_true(is.na(row$mean_abs_cor))
  expect_equal(row$n_observed, 20L)
})

test_that("fiscal_triage separates dimension, standalone, and excluded indicators", {
  set.seed(2)
  base <- rnorm(300)
  dat <- data.frame(
    a = base + rnorm(300, sd = .1),
    b = base + rnorm(300, sd = .1),
    c = rnorm(300),
    constant = 1
  )
  diag <- fiscal_diagnostics(dat, corr_cutoff = .9)
  triage <- fiscal_triage(diag, overrides = c(c = "standalone"))

  expect_s3_class(triage, "fiscal_triage")
  expect_length(triage$excluded_redundant, 1L)
  expect_true("constant" %in% triage$excluded_noise)
  expect_true("c" %in% triage$retained_standalone)
  expect_true("c" %in% triage$retained_for_final_system)
  expect_false("c" %in% triage$selected_for_dimensions)
  expect_true(triage$decisions$overridden[triage$decisions$variable == "c"])
})

test_that("fiscal_triage validates overrides", {
  dat <- data.frame(a = 1:20, b = (1:20) + rep(c(0, .1), 10))
  diag <- fiscal_diagnostics(dat)
  expect_error(fiscal_triage(diag, overrides = c(unknown = "retain")),
               "Unknown override")
  expect_error(fiscal_triage(diag, overrides = c(a = "drop")),
               "Override decisions")
})
