make_bmf_fixture <- function() {
  data.frame(
    EIN2 = c("EIN-12-3456789", "EIN-12-3456789", "EIN-98-7654321"),
    org_name_display = c("Old Name", "Current Name", "Second Org"),
    ntee_code_clean = c("A10", "A20", "B30"),
    nteev2 = c("ART-A10-RG", "ART-A20-RG", "EDU-B30-RG"),
    nteev2_subsector = c("ART", "ART", "EDU"),
    nteev2_org_type = c("RG", "RG", "RG"),
    subsection_code = c("03", "03", "04"),
    foundation_code = c("15", "15", "00"),
    ruling_date = c("1990-01-01", "1900-01-01", "2005-06-01"),
    ruling_date_is_missing = c(FALSE, TRUE, FALSE),
    revenue_amount = c(10, 20, 30),
    asset_amount = c(100, 200, 300),
    income_amount = c(11, 21, 31),
    geo_state_abbr = c("AZ", "AZ", "NM"),
    geo_county = c("Old County", "New County", "Other County"),
    bmf_source = c("legacy", "current", "current"),
    bmf_vintage_ym = c("2025-01", "2026-06", "2026-06"),
    stringsAsFactors = FALSE
  )
}

test_that("BMF defaults use the native master schema", {
  vars <- get_bmf_vars()

  expect_true(all(c(
    "EIN2", "ntee_code_clean", "nteev2", "subsection_code",
    "revenue_amount", "geo_state_abbr", "bmf_vintage_ym"
  ) %in% vars))
  expect_false(any(grepl("^(CENSUS_|F990_|NTEE_|BMF_|ORG_)", vars)))
})

test_that("BMF preparation resolves duplicates by vintage and derives ruling year", {
  bmf <- make_bmf_fixture()
  out <- fiscal:::.prepare_bmf(
    bmf,
    bmf_vars = c("org_name_display", "ruling_date", "ruling_year",
                 "bmf_source", "bmf_vintage_ym"),
    verbose = FALSE
  )
  diag <- attr(out, "bmf_diagnostics")

  expect_equal(nrow(out), 2L)
  expect_equal(out$org_name_display[out$EIN2 == "EIN-12-3456789"], "Current Name")
  expect_true(is.na(out$ruling_year[out$EIN2 == "EIN-12-3456789"]))
  expect_equal(out$ruling_year[out$EIN2 == "EIN-98-7654321"], 2005L)
  expect_equal(diag$duplicate_rows_resolved, 1L)
})

test_that("BMF preparation reports unavailable requested fields", {
  expect_warning(
    out <- fiscal:::.prepare_bmf(
      make_bmf_fixture(),
      bmf_vars = c("org_name_display", "does_not_exist"),
      verbose = FALSE
    ),
    regexp = "does_not_exist"
  )

  expect_false("does_not_exist" %in% names(out))
  expect_equal(attr(out, "bmf_diagnostics")$missing_requested_fields,
               "does_not_exist")
})

test_that("BMF join is many-to-one and preserves input row order", {
  panel <- data.frame(
    EIN2 = c("EIN-98-7654321", "EIN-00-0000001", "EIN-12-3456789",
             "EIN-12-3456789"),
    TAX_YEAR = c(2022, 2022, 2021, 2022),
    marker = 1:4,
    stringsAsFactors = FALSE
  )

  out <- fiscal:::.join_bmf(
    panel,
    make_bmf_fixture(),
    bmf_vars = c("org_name_display", "revenue_amount", "bmf_vintage_ym"),
    verbose = FALSE
  )
  diag <- attr(out, "bmf_diagnostics")

  expect_equal(nrow(out), nrow(panel))
  expect_equal(out$marker, panel$marker)
  expect_equal(out$org_name_display[3:4], rep("Current Name", 2L))
  expect_true(is.na(out$org_name_display[2]))
  expect_equal(diag$matched_rows, 3L)
  expect_equal(diag$unmatched_distinct_eins, 1L)
})

test_that("BMF join replaces existing native BMF fields", {
  panel <- data.frame(
    EIN2 = "EIN-98-7654321",
    org_name_display = "Stale Name",
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- fiscal:::.join_bmf(
      panel, make_bmf_fixture(), bmf_vars = "org_name_display",
      verbose = FALSE
    ),
    regexp = "Replacing existing BMF"
  )
  expect_equal(out$org_name_display, "Second Org")
})

test_that("merge_bmf accepts a local current-schema master", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(make_bmf_fixture(), path, row.names = FALSE, na = "")

  panel <- data.frame(
    EIN2 = c("EIN-12-3456789", "EIN-00-0000001"),
    TAX_YEAR = 2022,
    stringsAsFactors = FALSE
  )

  out <- suppressWarnings(merge_bmf(panel, bmf_path = path, verbose = FALSE))

  expect_equal(nrow(out), 2L)
  expect_equal(out$org_name_display[1], "Current Name")
  expect_true(is.na(out$org_name_display[2]))
  expect_true(is.list(attr(out, "bmf_diagnostics")))
})
