test_that("fiscal get_panel delegates to efileR and returns manifests", {
  root <- tempfile("fiscal-efile-source-")
  cache <- tempfile("fiscal-efile-cache-")
  dir.create(root)
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  for (year in 2021:2022) {
    utils::write.csv(
      data.frame(EIN2 = "EIN-12-3456789", OBJECTID = paste0("O", year),
                 amount = year),
      file.path(root, paste0("F9-P00-T00-HEADER-", year, ".CSV")),
      row.names = FALSE
    )
  }
  out <- get_panel(
    2021:2022, tables = "P00", include_bmf = FALSE, efile_root = root,
    path = cache, verbose = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$TAX_YEAR, 2021:2022)
  expect_s3_class(attr(out, "download_manifest"), "data.frame")
  expect_s3_class(attr(out, "table_manifest"), "data.frame")
})
