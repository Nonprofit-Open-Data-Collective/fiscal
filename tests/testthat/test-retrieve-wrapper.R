make_retrieve_source <- function() {
  root <- tempfile("retrieve-source-")
  dir.create(root)
  header <- data.frame(
    EIN2 = c("EIN-12-3456789", "EIN-98-7654321"),
    OBJECTID = c("O1", "O2"), RETURN_TYPE = c("990", "990"),
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    EIN2 = header$EIN2, OBJECTID = header$OBJECTID,
    amount = c(10, 20), stringsAsFactors = FALSE
  )
  repeated <- data.frame(
    EIN2 = c("EIN-12-3456789", "EIN-12-3456789"),
    OBJECTID = c("O1", "O1"), item = c("a", "b"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(header, file.path(root, "F9-P00-T00-HEADER-2022.CSV"),
                   row.names = FALSE)
  utils::write.csv(summary, file.path(root, "F9-P01-T00-SUMMARY-2022.CSV"),
                   row.names = FALSE)
  utils::write.csv(repeated, file.path(root, "CUSTOM-P01-T01-ROWS-2022.CSV"),
                   row.names = FALSE)
  root
}

test_that("single-year wrapper returns data and structured manifests", {
  root <- make_retrieve_source()
  cache <- tempfile("retrieve-cache-")
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  out <- retrieve_efile_data(
    2022, tables = c("P00", "P01"), include_bmf = FALSE,
    efile_root = root, path = cache, verbose = FALSE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("amount", "TAX_YEAR") %in% names(out)))
  expect_s3_class(attr(out, "download_status"), "data.frame")
  expect_s3_class(attr(out, "join_manifest"), "data.frame")
  expect_equal(attr(out, "download_dir"), cache)
})

test_that("single-year wrapper pushes source filters", {
  root <- make_retrieve_source()
  cache <- tempfile("retrieve-cache-")
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  out <- retrieve_efile_data(
    2022, tables = c("P00", "P01"), include_bmf = FALSE,
    efile_root = root, path = cache,
    filters = list(EIN2 = "EIN-12-3456789"), verbose = FALSE
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$EIN2, "EIN-12-3456789")
  expect_equal(attr(out, "table_manifest")$rows_selected, c(1L, 1L))
})

test_that("single-year wrapper gates one-to-many joins", {
  root <- make_retrieve_source()
  cache <- tempfile("retrieve-cache-")
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  safe <- retrieve_efile_data(
    2022, tables = c("P00", "CUSTOM-P01-T01-ROWS"), include_bmf = FALSE,
    efile_root = root, path = cache, verbose = FALSE
  )
  expanded <- retrieve_efile_data(
    2022, tables = c("P00", "CUSTOM-P01-T01-ROWS"), include_bmf = FALSE,
    efile_root = root, path = cache, join_1xm = TRUE, verbose = FALSE
  )
  expect_equal(nrow(safe), 2L)
  expect_equal(nrow(expanded), 3L)
})

test_that("single-year wrapper attaches native BMF fields", {
  root <- make_retrieve_source()
  cache <- tempfile("retrieve-cache-")
  on.exit(unlink(c(root, cache), recursive = TRUE), add = TRUE)
  bmf <- data.frame(
    EIN2 = "EIN-12-3456789", org_name_display = "Alpha Org",
    bmf_source = "current", bmf_vintage_ym = "2026-06",
    stringsAsFactors = FALSE
  )
  out <- suppressWarnings(retrieve_efile_data(
    2022, tables = "P00", include_bmf = TRUE,
    bmf_vars = c("org_name_display", "bmf_source", "bmf_vintage_ym"),
    bmf_url = bmf, efile_root = root, path = cache, verbose = FALSE
  ))
  expect_equal(out$org_name_display[out$EIN2 == "EIN-12-3456789"], "Alpha Org")
  expect_true(is.list(attr(out, "bmf_status")))
})
