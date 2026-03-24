# test-panel-functions.R
# Tests for panel data utilities: deduplicate() and panel_smooth()
# (get_panel() is network-dependent and not tested here)

library( fiscal )


# ---- deduplicate() -------------------------------------------------------

# Build a test panel with known duplicates
make_panel_df <- function() {
  data.frame(
    EIN2             = c( "A", "A", "A", "B", "B", "C", "D" ),
    TAX_YEAR         = c( 2021, 2021, 2021, 2021, 2021, 2021, 2021 ),
    RETURN_GROUP_X   = c( "",  "",  "",  "X", "X",  "",  "" ),
    RETURN_PARTIAL_X = c( "X", "",  "",  "",  "",   "",  "" ),
    RETURN_TIME_STAMP= c( "2022-01-01", "2022-06-15", "2022-09-01",
                          "2022-03-01", "2022-04-01",
                          "2022-05-01",
                          "2022-02-01" ),
    revenue          = c( 100, 200, 300, 400, 500, 600, 700 ),
    stringsAsFactors = FALSE
  )
}

test_that( "deduplicate: returns one row per EIN2-TAX_YEAR", {
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  keys <- paste( result$EIN2, result$TAX_YEAR )
  expect_true( !any( duplicated(keys) ),
               label = "no duplicate EIN2-TAX_YEAR pairs in output" )
})

test_that( "deduplicate: output is a data.frame", {
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  expect_s3_class( result, "data.frame" )
})

test_that( "deduplicate: same columns as input (minus ..key..)", {
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  expect_equal( sort(names(result)), sort(names(df)) )
})

test_that( "deduplicate: step 1 drops group returns", {
  # EIN B has only group returns — should be rescued (kept as 1 row)
  # EIN A has mixed — group return rows absent, partial+regular remain
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  # EIN B row should be present (rescued)
  expect_true( "B" %in% result$EIN2 )

  b_row <- result[ result$EIN2 == "B", ]
  expect_equal( nrow(b_row), 1L )
})

test_that( "deduplicate: step 2 drops partial returns when regular exists", {
  # EIN A: has partial (row 1, stamp 2022-01-01) + 2 regular rows
  # After step 1: partial still present (not a group return)
  # After step 2: partial dropped, 2 regular remain
  # After step 3: most recent regular kept (stamp 2022-09-01, revenue=300)
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  a_row <- result[ result$EIN2 == "A", ]
  expect_equal( nrow(a_row), 1L )
  expect_equal( a_row$revenue, 300,
                label = "most recent non-partial record kept for EIN A" )
})

test_that( "deduplicate: step 3 keeps most recent timestamp", {
  df <- data.frame(
    EIN2             = c( "X", "X" ),
    TAX_YEAR         = c( 2021, 2021 ),
    RETURN_GROUP_X   = c( "", "" ),
    RETURN_PARTIAL_X = c( "", "" ),
    RETURN_TIME_STAMP= c( "2022-01-01", "2022-12-31" ),
    value            = c( 10, 99 ),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages( deduplicate(df, verbose = FALSE) )

  expect_equal( nrow(result), 1L )
  expect_equal( result$value, 99,
                label = "most recent timestamp (Dec) selected" )
})

test_that( "deduplicate: at least one record per org-year always kept", {
  # All rows for EIN B are group returns — should be rescued
  df     <- make_panel_df()
  result <- suppressWarnings( suppressMessages( deduplicate(df, verbose = FALSE) ) )

  expect_true( "B" %in% result$EIN2,
               label = "org with only group returns still has 1 row" )
})

test_that( "deduplicate: missing flag columns skip steps gracefully", {
  df <- data.frame(
    EIN2     = c( "A", "A" ),
    TAX_YEAR = c( 2021, 2021 ),
    RETURN_TIME_STAMP = c( "2022-01-01", "2022-06-01" ),
    value    = c( 1, 2 ),
    stringsAsFactors = FALSE
    # No RETURN_GROUP_X or RETURN_PARTIAL_X
  )
  # Should not error, just skip steps 1 and 2
  expect_message(
    result <- deduplicate(df, verbose = TRUE),
    regexp = "not found"
  )
  expect_equal( nrow(result), 1L )
  expect_equal( result$value, 2L )   # most recent kept
})

test_that( "deduplicate: single row per org-year passes through unchanged", {
  df <- data.frame(
    EIN2             = c("A","B","C"),
    TAX_YEAR         = c(2021,2021,2021),
    RETURN_GROUP_X   = c("","",""),
    RETURN_PARTIAL_X = c("","",""),
    RETURN_TIME_STAMP= c("2022-01-01","2022-02-01","2022-03-01"),
    value            = c(1,2,3),
    stringsAsFactors = FALSE
  )
  result <- suppressMessages( deduplicate(df, verbose = FALSE) )

  expect_equal( nrow(result), 3L )
  expect_equal( sort(result$EIN2), c("A","B","C") )
})

test_that( "deduplicate: rejects missing id/year columns", {
  df <- data.frame( x = 1:3 )
  expect_error( deduplicate(df, by_id = "EIN2"), regexp = "by_id" )
  expect_error( deduplicate(df, by_id = "x", by_year = "TAX_YEAR"), regexp = "by_year" )
})


# ---- panel_smooth() ------------------------------------------------------

make_smooth_panel <- function() {
  data.frame(
    EIN2     = c( rep("A",4), rep("B",4) ),
    TAX_YEAR = c( 2020:2023,  2020:2023  ),
    x        = c( 1, 2, 3, 4,  10, 20, 30, 40 ),
    y        = c( 0, 0, 1, 1,   1,  1,  0,  0 ),
    stringsAsFactors = FALSE
  )
}

test_that( "panel_smooth: output dimensions equal input dimensions", {
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = c("x","y"), window = 3,
                       time = "TAX_YEAR", id = "EIN2" )

  expect_equal( nrow(out), nrow(df) )
  expect_equal( ncol(out), ncol(df) )
  expect_equal( names(out), names(df) )
})

test_that( "panel_smooth: identifier columns are unchanged", {
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = "x", window = 3,
                       time = "TAX_YEAR", id = "EIN2" )

  expect_equal( out$EIN2,     df$EIN2 )
  expect_equal( out$TAX_YEAR, df$TAX_YEAR )
  expect_equal( out$y,        df$y )   # y not in vars, should be unchanged
})

test_that( "panel_smooth: equal weighting, window=3, interior point", {
  # EIN A, year 2021 (index 2): window [1,2,3] → mean(1,2,3) = 2
  # EIN A, year 2022 (index 3): window [2,3,4] → mean(2,3,4) = 3
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = "x", window = 3, weights = "equal",
                       time = "TAX_YEAR", id = "EIN2" )

  a_rows <- out[ out$EIN2 == "A", ]
  a_rows <- a_rows[ order(a_rows$TAX_YEAR), ]

  # window=3, n=4: edge observations use a shifted window (not a shrunk window)
  # i=1: window=[1,2,3] → mean(1,2,3) = 2.0
  # i=2: window=[1,2,3] → mean(1,2,3) = 2.0
  # i=3: window=[2,3,4] → mean(2,3,4) = 3.0
  # i=4: window=[2,3,4] → mean(2,3,4) = 3.0
  expect_equal( a_rows$x[1], 2,  tolerance = 1e-9 )
  expect_equal( a_rows$x[2], 2,  tolerance = 1e-9 )
  expect_equal( a_rows$x[3], 3,  tolerance = 1e-9 )
  expect_equal( a_rows$x[4], 3,  tolerance = 1e-9 )
})

test_that( "panel_smooth: half weighting gives center weight=0.5", {
  # EIN A, year 2021 (index 2, interior): center=2, neighbors=1,3
  # half: center=0.5, each neighbor=0.25 → 0.5*2 + 0.25*1 + 0.25*3 = 2
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = "x", window = 3, weights = "half",
                       time = "TAX_YEAR", id = "EIN2" )

  a_rows <- out[ out$EIN2 == "A", ]
  a_rows <- a_rows[ order(a_rows$TAX_YEAR), ]

  expect_equal( a_rows$x[2], 2, tolerance = 1e-9 )
  expect_equal( a_rows$x[3], 3, tolerance = 1e-9 )
})

test_that( "panel_smooth: decay weighting produces center-heavy result", {
  # For x = 1,2,3,4, window=3:
  # At index 2 (interior): decay weights for d=1,0,1 → 0.5,1.0,0.5 / 2.0 = 0.25,0.5,0.25
  # Result = 0.25*1 + 0.5*2 + 0.25*3 = 2.0  (same as equal for symmetric data)
  # At index 1 (edge, shifted window [1,2,3]):
  # center_pos=1, d=[0,1,2]: 1.0, 0.5, 0.25 / 1.75 → heavier weight on first element
  df  <- make_smooth_panel()
  out_d <- panel_smooth( df, vars = "x", window = 3, weights = "decay",
                         time = "TAX_YEAR", id = "EIN2" )
  out_e <- panel_smooth( df, vars = "x", window = 3, weights = "equal",
                         time = "TAX_YEAR", id = "EIN2" )

  # Decay smoothed edge values should differ from equal (more weight on focal)
  # For linear x, interior values coincide, but edges differ
  a_d <- out_d[ out_d$EIN2 == "A", ]$x
  a_e <- out_e[ out_e$EIN2 == "A", ]$x
  expect_false( identical(a_d[1], a_e[1]),
                label = "decay and equal give different edge results" )
})

test_that( "panel_smooth: window=1 returns unchanged values", {
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = c("x","y"), window = 1,
                       time = "TAX_YEAR", id = "EIN2" )

  expect_equal( out$x, df$x )
  expect_equal( out$y, df$y )
})

test_that( "panel_smooth: NA values excluded from window", {
  df <- data.frame(
    EIN2     = rep("A", 4),
    TAX_YEAR = 2020:2023,
    x        = c( 1, NA, 3, 4 ),
    stringsAsFactors = FALSE
  )
  out <- panel_smooth( df, vars = "x", window = 3, weights = "equal",
                       time = "TAX_YEAR", id = "EIN2" )

  # Year 2021 (NA): window [1,2,3] → NA excluded → mean(1,3) = 2
  expect_equal( out$x[2], 2, tolerance = 1e-9 )
})

test_that( "panel_smooth: vars='PZ' smooths only PZ-scope numeric columns", {
  df  <- make_test_df( n = 8 )
  # Add a second year so we have a panel
  df2 <- df
  df2$TAX_YEAR <- 2022L
  panel <- rbind(df, df2)

  out <- panel_smooth( panel, vars = "PZ", window = 3,
                       time = "TAX_YEAR", id = "EIN2" )

  expect_equal( nrow(out), nrow(panel) )
  expect_equal( ncol(out), ncol(panel) )

  # Non-PZ columns should be unchanged
  pz_cols <- intersect( get_pz_fields(), names(panel) )
  expect_true( length(pz_cols) > 0,
               label = "at least some PZ fields present" )
})

test_that( "panel_smooth: custom vars vector works", {
  df  <- make_smooth_panel()
  out <- panel_smooth( df, vars = "x", window = 3,
                       time = "TAX_YEAR", id = "EIN2" )

  # y should be unchanged
  expect_equal( out$y, df$y )
})

test_that( "panel_smooth: missing id column raises error", {
  df <- make_smooth_panel()
  expect_error(
    panel_smooth( df, vars = "x", id = "NONEXISTENT" ),
    regexp = "id.*column not found"
  )
})

test_that( "panel_smooth: missing vars raises error for custom vector", {
  df <- make_smooth_panel()
  expect_error(
    panel_smooth( df, vars = c("x", "does_not_exist") ),
    regexp = "not found in df"
  )
})

test_that( "panel_smooth: even window rejected", {
  df <- make_smooth_panel()
  expect_error(
    panel_smooth( df, vars = "x", window = 4 ),
    regexp = "odd"
  )
})

test_that( "panel_smooth: invalid weights rejected", {
  df <- make_smooth_panel()
  expect_error(
    panel_smooth( df, vars = "x", weights = "triangle" ),
    regexp = "weights"
  )
})
