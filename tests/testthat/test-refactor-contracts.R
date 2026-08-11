# Behavioral contracts to preserve while extracting efileR from fiscal.

test_that("efile table catalog partitions cleanly by cardinality", {
  all_tables <- efile_tables("all")
  one_to_one <- efile_tables("1x1")
  one_to_many <- efile_tables("1xm")
  supplemental <- efile_tables("supplemental")

  expect_type(all_tables, "character")
  expect_true(length(all_tables) > 0L)
  expect_length(unique(all_tables), length(all_tables))

  expect_length(intersect(one_to_one, one_to_many), 0L)
  expect_length(intersect(one_to_one, supplemental), 0L)
  expect_length(intersect(one_to_many, supplemental), 0L)
  expect_setequal(
    all_tables,
    c(one_to_one, one_to_many, supplemental)
  )
})

test_that("known table cardinalities remain stable", {
  expect_true("F9-P00-T00-HEADER" %in% efile_tables("1x1"))
  expect_true("F9-P07-T01-COMPENSATION" %in% efile_tables("1xm"))
  expect_true("SO-T99-SUPPLEMENTAL-INFO" %in% efile_tables("supplemental"))
})

test_that("efile_tables rejects unknown cardinality labels", {
  expect_error(
    efile_tables("many"),
    regexp = "cardinality must be one of"
  )
})

test_that("panel pattern taxonomy remains stable", {
  panel_years <- 2019:2023

  full <- fiscal:::.classify_panel_pattern(2019:2023, panel_years)
  entry <- fiscal:::.classify_panel_pattern(2021:2023, panel_years)
  exit <- fiscal:::.classify_panel_pattern(2019:2021, panel_years)
  fragmented <- fiscal:::.classify_panel_pattern(c(2019, 2021, 2023), panel_years)

  expect_equal(full$panel_type, "full")
  expect_equal(entry$panel_type, "entry")
  expect_equal(exit$panel_type, "exit")
  expect_equal(fragmented$panel_type, "full")

  expect_equal(full$spell_balance, "contiguous")
  expect_equal(entry$spell_balance, "contiguous")
  expect_equal(exit$spell_balance, "contiguous")
  expect_equal(fragmented$spell_balance, "fragmented")

  expect_equal(full$gap_count, 0L)
  expect_equal(fragmented$gap_count, 2L)
  expect_equal(fragmented$gap_size_max, 1L)
})

test_that("panel_composition returns a per-ID classification", {
  panel <- data.frame(
    ORG = c(rep("A", 3L), "B", "B", "C", "C"),
    YEAR = c(2020:2022, 2021:2022, 2020, 2022),
    value = seq_len(7L),
    stringsAsFactors = FALSE
  )

  classification <- panel_composition(
    panel, time = "YEAR", id = "ORG",
    append_classification = FALSE, print_table = FALSE
  )

  expect_s3_class(classification, "data.frame")
  expect_true(all(c("ORG", "panel_type", "panel_spell") %in% names(classification)))
  expect_equal(classification$panel_type[classification$ORG == "A"], "persistent")
  expect_equal(classification$panel_type[classification$ORG == "B"], "entrant")
  expect_equal(classification$panel_spell[classification$ORG == "C"], "segmented")
})

test_that("panel_composition appends classifications with a custom ID", {
  panel <- data.frame(
    ORG = c("A", "A", "B"),
    YEAR = c(2020, 2021, 2021),
    stringsAsFactors = FALSE
  )

  out <- panel_composition(
    panel, time = "YEAR", id = "ORG",
    append_classification = TRUE, print_table = FALSE
  )

  expect_equal(nrow(out), nrow(panel))
  expect_true(all(c("panel_type", "panel_spell") %in% names(out)))
  expect_equal(unique(out$panel_type[out$ORG == "A"]), "persistent")
  expect_equal(unique(out$panel_type[out$ORG == "B"]), "entrant")
})

test_that("panel filtering uses panel type and spell labels", {
  panel <- data.frame(
    EIN2 = rep(c("A", "B", "C"), each = 2L),
    TAX_YEAR = rep(2021:2022, times = 3L),
    stringsAsFactors = FALSE
  )
  classification <- data.frame(
    EIN2 = c("A", "B", "C"),
    panel_type = c("persistent", "entrant", "exit"),
    panel_spell = c("seamless", "seamless", "segmented"),
    stringsAsFactors = FALSE
  )

  expect_setequal(
    unique(panel_filter_types(panel, classification, keep = c("entrant", "exit"))$EIN2),
    c("B", "C")
  )
  expect_setequal(
    unique(panel_filter_types(
      panel, classification, spell_balance = "segmented"
    )$EIN2),
    "C"
  )
  expect_setequal(
    unique(panel_filter_types(
      panel, classification, keep = "exit", spell_balance = "segmented"
    )$EIN2),
    "C"
  )
})

test_that("panel_impute can obtain classifications automatically", {
  panel <- data.frame(
    EIN2 = c("A", "A", "B", "B", "B"),
    TAX_YEAR = c(2020, 2022, 2020:2022),
    amount = c(10, 30, 1, 2, 3),
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(panel_impute(panel, vars = "amount"))
  added <- out[out$EIN2 == "A" & out$TAX_YEAR == 2021, , drop = FALSE]

  expect_equal(nrow(added), 1L)
  expect_equal(added$amount, 20)
  expect_true(added$imputed_row)
})

test_that("deduplicate recognizes date and datetime timestamps", {
  panel <- data.frame(
    EIN2 = c("A", "A", "B", "B"),
    TAX_YEAR = 2021,
    RETURN_TIME_STAMP = c(
      "2022-01-01", "2022-12-31",
      "2022-01-01 08:00:00", "2022-01-01T09:30:00"
    ),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(deduplicate(panel, verbose = FALSE))

  expect_equal(out$value[out$EIN2 == "A"], 2)
  expect_equal(out$value[out$EIN2 == "B"], 4)
})

test_that("normalization handles small and entirely missing samples", {
  small <- normalize_x(c(1, 2, NA, 4, 5), range = "np", verbose = FALSE)
  missing <- apply_transformations(c(NA_real_, NaN, NA_real_), range = "np")

  expect_length(small, 5L)
  expect_true(is.na(small[3]))
  expect_true(all(is.na(missing$winsorized)))
  expect_true(all(is.na(missing$z)))
  expect_true(all(is.na(missing$pctile)))
})
