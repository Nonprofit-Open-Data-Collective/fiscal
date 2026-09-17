# =====================================================================
#  build-metrics-catalog.R
#  ---------------------------------------------------------------------
#  Regenerates vignettes/metrics-catalog.Rmd from the package sources so
#  the catalog never drifts from the function help files.
#
#  For every get_*() metric it extracts, verbatim, the roxygen
#  @description block (which already carries Description, Formula,
#  Definitional Range, Benchmarks, and Calculated For), then splices in
#  the per-metric "input variables" table parsed from README.md. Metrics
#  are ordered and grouped using fiscal_metrics().
#
#  Run from the package root:
#      Rscript dev/build-metrics-catalog.R
#  (or)  source("dev/build-metrics-catalog.R")
# =====================================================================

suppressMessages(
  devtools::load_all(rprojroot::find_root(rprojroot::is_r_package), quiet = TRUE)
)

pkg_root  <- rprojroot::find_root(rprojroot::is_r_package)
readme    <- readLines(file.path(pkg_root, "README.md"), warn = FALSE)
out_file  <- file.path(pkg_root, "vignettes", "metrics-catalog.Rmd")

reg <- fiscal_metrics()

category_labels <- c(
  liquidity         = "Liquidity",
  solvency          = "Solvency & leverage",
  performance       = "Performance & margins",
  expense_structure = "Expense structure",
  revenue_structure = "Revenue structure",
  asset_structure   = "Asset structure"
)

# ---- helpers --------------------------------------------------------

# strip the roxygen comment prefix ("#' " / "#'") from a line
strip_rox <- function( x ) sub( "^#'[ ]?", "", x )

# convert Rd/roxygen cross-reference links -- [fn()] and [fn] -- into
# inline code, since a plain vignette does not resolve the [ ] link syntax
clean_rd_links <- function( x ) {
  x <- gsub( "\\[([A-Za-z0-9_.]+\\(\\))\\]", "`\\1`", x )                 # [fn()] -> `fn()`
  x <- gsub( "\\[([A-Za-z0-9_.]+)\\](?!\\()", "`\\1`", x, perl = TRUE )   # bare [fn] (not a md link) -> `fn`

  # Unescape roxygen/Rd square-bracket escaping. In the .Rd help these read as
  # literal brackets, but copied into a plain-markdown vignette pandoc reads
  # `\[ ... \]` as LaTeX display-math delimiters (centred, brackets stripped).
  # Restore literal brackets, which render verbatim in markdown.
  x <- gsub( "\\[", "[", x, fixed = TRUE )   # \[ -> [
  x <- gsub( "\\]", "]", x, fixed = TRUE )   # \] -> ]
  x
}

# pull the @title text and the full @description block from a source file
extract_roxygen <- function( fn ) {

  path  <- file.path( pkg_root, "R", paste0( gsub( "_", "-", fn ), ".R" ) )
  lines <- readLines( path, warn = FALSE )
  rox   <- lines[ grepl( "^#'", lines ) ]
  rox   <- strip_rox( rox )

  tag_at <- grep( "^@[a-zA-Z]+", rox )

  # title: first non-empty line after @title
  t_idx  <- grep( "^@title", rox )[1]
  title  <- trimws( rox[ t_idx + 1 ] )

  # description: from @description up to the next @tag
  d_idx  <- grep( "^@description", rox )[1]
  d_end  <- tag_at[ tag_at > d_idx ][1] - 1
  desc   <- rox[ ( d_idx + 1 ):d_end ]

  # drop leading/trailing blank lines
  desc   <- desc[ cumsum( nzchar( trimws( desc ) ) ) > 0 ]
  desc   <- rev( rev( desc )[ cumsum( nzchar( trimws( rev( desc ) ) ) ) > 0 ] )

  list( title = title, desc = desc )
}

# grab the "| Argument | efile Variable | Description |" table for a
# function from README.md (the block of consecutive table rows under the
# "## get_xxx()" heading)
extract_readme_table <- function( fn ) {

  head_pat <- paste0( "^##[ ]+", fn, "\\(\\)[ ]*$" )
  h        <- grep( head_pat, readme )
  if ( length( h ) == 0 ) return( character( 0 ) )

  # scan forward to the next "## " heading
  nxt   <- grep( "^##[ ]", readme )
  stop_at <- nxt[ nxt > h[1] ][1]
  if ( is.na( stop_at ) ) stop_at <- length( readme )
  block <- readme[ ( h[1] + 1 ):( stop_at - 1 ) ]

  tbl <- block[ grepl( "^\\|", block ) ]
  tbl
}

# split a description block into (a) everything through the first fenced
# code block (Description + Formula) and (b) the remainder (Definitional
# Range, Benchmarks, Calculated For)
split_after_formula <- function( desc ) {
  fences <- grep( "^```", desc )
  if ( length( fences ) >= 2 ) {
    close_at <- fences[2]
    list( head = desc[ 1:close_at ], tail = desc[ -( 1:close_at ) ] )
  } else {
    list( head = desc, tail = character( 0 ) )
  }
}

# ---- assemble one catalog entry -------------------------------------

entry_md <- function( fn ) {

  rox   <- extract_roxygen( fn )
  rox$desc <- clean_rd_links( rox$desc )
  tbl   <- extract_readme_table( fn )
  parts <- split_after_formula( rox$desc )

  md <- c(
    sprintf( "### %s", rox$title ),
    "",
    sprintf( "`%s()`", fn ),
    "",
    parts$head
  )

  if ( length( tbl ) > 0 ) {
    md <- c( md, "", "**Input variables**", "", tbl )
  }

  if ( length( parts$tail ) > 0 ) {
    md <- c( md, "", parts$tail )
  }

  md <- c( md, "", "<hr>", "" )
  md
}

# ---- header ---------------------------------------------------------

header <- c(
  "---",
  "title: \"Fiscal metrics catalog\"",
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{Fiscal metrics catalog}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "```{r, include = FALSE}",
  "knitr::opts_chunk$set( collapse = TRUE, comment = \"#>\" )",
  "```",
  "",
  "<!-- ================================================================= -->",
  "<!-- GENERATED FILE - do not edit by hand.                             -->",
  "<!-- Regenerate with: Rscript dev/build-metrics-catalog.R              -->",
  "<!-- Source: roxygen @description blocks in R/get-*.R + README tables. -->",
  "<!-- ================================================================= -->",
  "",
  "This catalog documents every fiscal-health ratio in the package. Each entry",
  "mirrors the structure of the function help file: a short **description**, the",
  "**formula**, the **input variables** (with their default IRS 990 e-file field",
  "names), the **definitional range**, any **benchmarks and rules of thumb**, and",
  "the filer scope the metric is **calculated for**.",
  "",
  "Every `get_*()` function appends four columns to your data --- the raw ratio",
  "and its winsorized (`_w`), standardized (`_z`), and percentile-rank (`_p`)",
  "versions --- or run the whole battery at once with `compute_all()`. See",
  "`vignette(\"fiscal\")` to get started and `vignette(\"normalization\")` for what",
  "the transformed versions mean.",
  "",
  "```{r, eval = FALSE}",
  "library( fiscal )",
  "help( get_program_expenses_ratio )   # the same content, per function",
  "```",
  ""
)

# ---- body: grouped by category --------------------------------------

body <- character( 0 )

for ( cat in names( category_labels ) ) {

  fns <- reg$function_name[ reg$category == cat ]
  if ( length( fns ) == 0 ) next

  body <- c(
    body,
    sprintf( "## %s", category_labels[[ cat ]] ),
    ""
  )

  for ( fn in fns ) {
    body <- c( body, entry_md( fn ) )
  }
}

writeLines( c( header, body ), out_file )

n_metrics <- sum( reg$category %in% names( category_labels ) )
message( sprintf( "Wrote %s (%d metrics across %d categories).",
                  out_file, n_metrics, length( category_labels ) ) )
