#' Retrieve and merge IRS 990 efile tables for one tax year
#'
#' Compatibility wrapper around `panel990::panelize()`. It returns a data frame
#' and attaches structured download, table, join, and BMF diagnostics.
#'
#' @param year One tax year.
#' @param tables Aliases or literal canonical table names.
#' @param include_bmf Attach current geocoded BMF fields.
#' @param bmf_vars Native BMF fields to retain; `NULL` retains all available.
#' @param join_1xm Join one-to-many and supplemental tables.
#' @param efile_root Efile URL or local source directory.
#' @param bmf_url BMF URL, path, or data frame.
#' @param timeout Download timeout.
#' @param retry_max Download attempts.
#' @param path Retained CSV cache directory.
#' @param keep_files Retain downloaded CSVs. When false, use a temporary cache.
#' @param verbose Print progress.
#' @param backend `"memory"` or `"duckdb"`.
#' @param cache Optional explicit cache mode: `"retain"`, `"temporary"`, or
#'   `"none"`. By default it is derived from `keep_files`.
#' @param filters Optional named source filters.
#' @param columns Optional source fields to retain.
#' @param collision Non-key collision policy.
#' @param overwrite Replace cached files.
#' @return A data frame with structured retrieval attributes.
#' @export
retrieve_efile_data <- function(
    year,
    tables = c("P00", "P01", "P08", "P09", "P10"),
    include_bmf = TRUE,
    bmf_vars = .BMF_VARS,
    join_1xm = FALSE,
    efile_root = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/",
    bmf_url = .BMF_URL,
    timeout = 300,
    retry_max = 3L,
    path = "efdata",
    keep_files = TRUE,
    verbose = TRUE,
    backend = "memory",
    cache = NULL,
    filters = NULL,
    columns = NULL,
    collision = "error",
    overwrite = FALSE
) {
  if (!is.numeric(year) || length(year) != 1L || is.na(year))
    stop("`year` must be a single integer.")
  if (!is.logical(keep_files) || length(keep_files) != 1L || is.na(keep_files))
    stop("`keep_files` must be TRUE or FALSE.")
  if (is.null(cache)) cache <- if (keep_files) "retain" else "temporary"
  # panelize() auto-assigns the filing keys and returns a `panel` object; BMF is
  # attached explicitly so the caller's `bmf_url`/`bmf_vars` still apply.
  p <- panel990::panelize(
    tables = tables, years = as.integer(year),
    source = panel990::data_source(efile_root), bmf = FALSE, path = path,
    cache = cache, backend = backend, filters = filters, columns = columns,
    include_many = join_1xm, collision = collision, overwrite = overwrite,
    retry_max = retry_max, timeout = timeout, verbose = verbose
  )
  out <- as.data.frame(p)
  bmf_diagnostics <- NULL
  if (include_bmf) {
    out <- panel990::bmf_merge(
      out, source = bmf_url, vars = bmf_vars, verbose = verbose
    )
    bmf_diagnostics <- attr(out, "bmf_diagnostics")
  }
  attr(out, "download_dir") <- path
  attr(out, "download_status") <- p$download_manifest
  attr(out, "table_manifest") <- p$table_manifest
  attr(out, "join_manifest") <- p$join_manifest
  attr(out, "bmf_status") <- bmf_diagnostics
  out
}
