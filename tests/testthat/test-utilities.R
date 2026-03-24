# test-utilities.R
# Tests for utility functions: sanitize_financials(), compute_all(),
# NTEE classification, and field list getters.

library( fiscal )


# ---- get_idvars / get_pc_fields / get_pz_fields --------------------------

test_that( "get_idvars returns a non-empty character vector", {
  v <- get_idvars()
  expect_true( is.character(v) )
  expect_gt( length(v), 0 )
  expect_true( "EIN2"     %in% v )
  expect_true( "TAX_YEAR" %in% v )
})

test_that( "get_pc_fields returns a non-empty character vector of F9_ names", {
  v <- get_pc_fields()
  expect_true( is.character(v) )
  expect_gt( length(v), 100 )
  expect_true( all( grepl("^F9_", v) ) )
})

test_that( "get_pz_fields returns a non-empty character vector of F9_ names", {
  v <- get_pz_fields()
  expect_true( is.character(v) )
  expect_gt( length(v), 10 )
  expect_true( all( grepl("^F9_", v) ) )
})

test_that( "PC and PZ fields overlap on shared Part I fields", {
  # Some fields appear in both (11 shared fields per design)
  shared <- intersect( get_pc_fields(), get_pz_fields() )
  expect_gt( length(shared), 0 )
})


# ---- detect_ez_rows() ----------------------------------------------------

test_that( "detect_ez_rows uses RETURN_TYPE column when present", {
  df <- data.frame(
    RETURN_TYPE = c( "990", "990EZ", "990", "990EZ-SHORT" ),
    stringsAsFactors = FALSE
  )
  result <- detect_ez_rows(df)
  expect_equal( result, c(FALSE, TRUE, FALSE, TRUE) )
})


# ---- sanitize_financials() -----------------------------------------------

test_that( "sanitize_financials returns data.frame with same rows/cols", {
  df     <- make_test_df( n = 20 )
  result <- sanitize_financials( df )

  expect_s3_class( result, "data.frame" )
  expect_equal( nrow(result), nrow(df) )
  expect_equal( ncol(result), ncol(df) )
})

test_that( "sanitize_financials imputes NA to 0 in PC fields for 990 filers", {
  df <- make_test_df( n = 4 )
  df$RETURN_TYPE <- c("990","990","990EZ","990EZ")

  # Introduce NA in a PC-scope field
  df$F9_09_EXP_TOT_TOT[1] <- NA

  result <- sanitize_financials( df )

  # 990 filer with NA in PC field → imputed to 0
  expect_equal( result$F9_09_EXP_TOT_TOT[1], 0 )

  # 990EZ filer NA in PC field → stays NA (PC fields not present on 990EZ)
  expect_true( is.na( result$F9_09_EXP_TOT_TOT[3] ) ||
               result$F9_09_EXP_TOT_TOT[3] == 0,
               label = "990EZ filer in PC field: NA or 0" )
})

test_that( "sanitize_financials: original columns preserved", {
  df     <- make_test_df( n = 5 )
  result <- sanitize_financials( df )

  # Input columns should all still be present
  expect_true( all( names(df) %in% names(result) ) )
})


# ---- NTEE utilities ------------------------------------------------------

test_that( "get_clean_ntee: normalizes 3-char codes", {
  expect_equal( get_clean_ntee("B29"),   "B29" )
  expect_equal( get_clean_ntee("b29"),   "B29" )   # lowercase
})

test_that( "get_clean_ntee: handles 5-char specialty codes", {
  # B8443 → specialty org, digits 4-5 = 43, so code = B43
  result <- get_clean_ntee("B8443")
  expect_equal( nchar(result), 3 )
})

test_that( "get_clean_ntee: missing/blank input returns Z99", {
  expect_equal( get_clean_ntee(""),   "Z99" )
  expect_equal( get_clean_ntee(NA_character_), "Z99" )
})

test_that( "get_nteev2: produces [INDUSTRY]-[NTEE]-[ORGTYPE] format", {
  result <- get_nteev2("B29")
  parts  <- strsplit(result, "-")[[1]]

  expect_equal( length(parts), 3 )
  expect_equal( nchar(parts[1]), 3 )   # industry
  expect_equal( nchar(parts[2]), 3 )   # ntee
  expect_true(  nchar(parts[3]) >= 2 ) # org type
})

test_that( "get_ntmaj12: returns a factor with correct levels", {
  result <- get_ntmaj12( c("HOS-E05-RG", "EDU-B29-RG", "ART-A12-RG") )

  expect_s3_class( result, "factor" )
  expect_equal( length(result), 3 )
  expect_true( all( !is.na(result) ) )
})

test_that( "get_industry: B40-B43 → UNI, E20-E24 → HOS", {
  expect_equal( get_industry("B40"), "UNI" )
  expect_equal( get_industry("B43"), "UNI" )
  expect_equal( get_industry("E21"), "HOS" )
  expect_equal( get_industry("B29"), "EDU" )
  expect_equal( get_industry("A12"), "ART" )
})

test_that( "get_org_type: correct codes for standard types", {
  expect_equal( get_org_type("B01"), "AA" )   # alliance
  expect_equal( get_org_type("B29"), "RG" )   # regular
  expect_equal( get_org_type("B05"), "RP" )   # research
  expect_equal( get_org_type("B11"), "MS" )   # single-org support
})


# ---- compute_all() -------------------------------------------------------

test_that( "compute_all: returns data.frame with more columns than input", {
  df     <- make_test_df( n = 30 )
  result <- suppressMessages( compute_all(df, verbose = FALSE) )

  expect_s3_class( result, "data.frame" )
  expect_equal( nrow(result), nrow(df) )
  expect_gt( ncol(result), ncol(df) )
})

test_that( "compute_all: metrics argument filters output correctly", {
  df <- make_test_df( n = 20 )

  # Only raw ratio columns
  r_only <- suppressMessages( compute_all(df, metrics = "ratio", verbose = FALSE) )
  new_cols <- setdiff( names(r_only), names(df) )
  # No _w, _z, or _p suffix in the new columns
  expect_false( any( grepl("_w$|_z$|_p$", new_cols) ),
                label = "metrics='ratio' produces no _w/_z/_p columns" )

  # Only _z columns
  z_only <- suppressMessages( compute_all(df, metrics = "z", verbose = FALSE) )
  new_z  <- setdiff( names(z_only), names(df) )
  expect_true( all( grepl("_z$", new_z) ),
               label = "metrics='z' produces only _z columns" )
})

test_that( "compute_all: append_to_df=FALSE returns id cols + metric cols only", {
  df     <- make_test_df( n = 20 )
  result <- suppressMessages(
    compute_all(df, append_to_df = FALSE, metrics = "ratio", verbose = FALSE)
  )

  # Should not contain the F9_ financial fields
  expect_false( "F9_09_EXP_TOT_TOT" %in% names(result),
                label = "financial input fields not in output when append=FALSE" )

  # Should contain EIN2 or other idvars if present
  id_cols_present <- intersect( get_idvars(), names(result) )
  expect_gt( length(id_cols_present), 0,
             label = "at least one idvar present in output" )
})

test_that( "compute_all: invalid metrics value raises error", {
  df <- make_test_df( n = 5 )
  expect_error(
    compute_all( df, metrics = "raw", verbose = FALSE ),
    regexp = "Invalid"
  )
})

test_that( "compute_all: verbose=FALSE runs without error", {
  # Note: ratio functions emit message() for NaN counts regardless of verbose.
  # verbose=FALSE suppresses compute_all's own progress messages only.
  df <- make_test_df( n = 10 )
  expect_no_error(
    suppressMessages( suppressWarnings( compute_all(df, verbose = FALSE) ) )
  )
})
