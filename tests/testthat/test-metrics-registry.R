test_that("fiscal metric registry is complete and internally consistent", {
  registry <- fiscal_metrics()
  expect_equal(nrow(registry), 48L)
  expect_length(unique(registry$function_name), nrow(registry))
  expect_length(unique(registry$metric), nrow(registry))
  expect_true(all(registry$direction %in% c("higher", "lower", "context")))
  expect_equal(registry$winsorized, paste0(registry$metric, "_w"))
  expect_equal(registry$standardized, paste0(registry$metric, "_z"))
  expect_equal(registry$percentile, paste0(registry$metric, "_p"))
})

test_that("registered functions produce registered output fields", {
  data <- make_test_df()
  registry <- fiscal_metrics()
  for (i in seq_len(nrow(registry))) {
    result <- suppressMessages(
      do.call(get(registry$function_name[[i]]), list(data, sanitize = TRUE))
    )
    expected <- unlist(registry[i, c("raw", "winsorized", "standardized", "percentile")])
    expect_true(all(expected %in% names(result)), info = registry$function_name[[i]])
  }
})
