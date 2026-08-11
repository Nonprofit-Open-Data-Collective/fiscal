# See what needs fixing
hits <- check_ascii("R")
hits   # inspect the offending lines

# Fix everything in one pass
check_ascii("R", fix = TRUE)

# Then re-document cleanly
devtools::document()

# Install fresh copy
setwd("..")
devtools::install("fiscal")


########
########
########

# check on hidden internal functions:

?fiscal:::.parse_stamp

#  THESE WON'T WORK:
#   help(".parse_stamp")
#  .parse_stamp


########
########
########


#' Check and fix non-ASCII characters in R source files
#'
#' Scans all `.R` files in a directory for non-ASCII characters, reports
#' findings, and optionally replaces them with hyphens in-place.
#'
#' @param path Directory containing `.R` files. Default `"R"`.
#' @param fix Logical. If `TRUE`, overwrite files with non-ASCII characters
#'   replaced by hyphens. If `FALSE` (default), report only.
#' @param verbose Logical. Print per-file summary. Default `TRUE`.
#'
#' @return Invisibly returns a data frame with columns `file`, `line`, and
#'   `text` for every offending line found across all files.
#'
#' @examples
#' \dontrun{
#' # Report only
#' check_ascii("R")
#'
#' # Report and fix
#' check_ascii("R", fix = TRUE)
#' }
#'
#' @export
check_ascii <- function(path = "R", fix = FALSE, verbose = TRUE) {

  if (!dir.exists(path))
    stop("Directory not found: ", path)

  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)

  if (length(files) == 0L) {
    message("No .R files found in: ", path)
    return(invisible(data.frame(file = character(), line = integer(),
                                text = character(), encoding = character())))
  }

  all_hits <- vector("list", length(files))

  for (i in seq_along(files)) {

    f <- files[i]

    # -- Try UTF-8 first, fall back to latin1 (Windows-1252) ------------------
    x <- tryCatch(
      readLines(f, encoding = "UTF-8", warn = FALSE),
      error = function(e) NULL
    )

    file_encoding <- "UTF-8"

    # Check if UTF-8 read produced invalid strings
    if (is.null(x) || any(is.na(iconv(x, from = "UTF-8", to = "UTF-8")))) {
      x <- tryCatch(
        readLines(f, encoding = "latin1", warn = FALSE),
        error = function(e) {
          warning("Could not read: ", f, " -- ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      file_encoding <- "latin1"
    }

    if (is.null(x)) next

    # Normalize to UTF-8 for consistent grep
    x_utf8 <- iconv(x, from = file_encoding, to = "UTF-8", sub = "byte")

    hit_idx <- grep("[^\x01-\x7F]", x_utf8)

    if (length(hit_idx) == 0L) {
      if (verbose)
        message("OK  ", basename(f))
      next
    }

    if (verbose)
      message("HIT ", basename(f),
              " [", file_encoding, "]",
              " -- ", length(hit_idx), " line(s) with non-ASCII characters")

    all_hits[[i]] <- data.frame(
      file     = basename(f),
      line     = hit_idx,
      text     = x_utf8[hit_idx],
      encoding = file_encoding,
      stringsAsFactors = FALSE
    )

    if (isTRUE(fix)) {
      x_fixed <- iconv(x_utf8, from = "UTF-8", to = "ASCII", sub = "-")

      tryCatch({
        con <- file(f, open = "w", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        writeLines(x_fixed, con = con, useBytes = FALSE)
      }, error = function(e) {
        warning("Could not write: ", f, " -- ", conditionMessage(e), call. = FALSE)
      })

      # Verify
      x_verify  <- readLines(f, encoding = "UTF-8", warn = FALSE)
      remaining  <- grep("[^\x01-\x7F]", x_verify)
      if (length(remaining) > 0L) {
        warning(
          "Fix incomplete for: ", basename(f), " -- ",
          length(remaining), " non-ASCII line(s) remain.",
          call. = FALSE
        )
      } else if (verbose) {
        message("     Fixed and verified: ", f)
      }
    }
  }

  hits_df <- do.call(rbind, Filter(Negate(is.null), all_hits))

  if (is.null(hits_df))
    hits_df <- data.frame(file = character(), line = integer(),
                          text = character(), encoding = character(),
                          stringsAsFactors = FALSE)

  if (nrow(hits_df) == 0L) {
    message("\nAll files clean -- no non-ASCII characters found.")
  } else {
    message(
      "\nSummary: ", length(unique(hits_df$file)), " file(s) with non-ASCII characters, ",
      nrow(hits_df), " total line(s)."
    )
    if (!isTRUE(fix))
      message("Run with fix = TRUE to replace non-ASCII characters in place.")
  }

  invisible(hits_df)
}




########
########
########

# PRINTS ANY NON-ASCII CHARACTERS

f <- "R/panel-describe.R"
x <- readLines(f, encoding = "UTF-8")
hits <- grep("[^\x01-\x7F]", x)
data.frame(line = hits, text = x[hits])

# FIXES ANY NON-ASCII CHARACTERS

f <- "R/panel-impute.R"
x <- readLines(f, encoding = "UTF-8")
x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "-")
writeLines(x, f, useBytes = FALSE)

