#' Summarize panel composition over time
#'
#' Compatibility wrapper around panel990's classification verbs. Classification
#' uses the `persistent`, `entrant`, `exit`, `transient`, and `empty` boundary
#' types and the independent `seamless`/`segmented` spell dimension.
#'
#' @param df A panel data frame.
#' @param time Name of the panel-time column.
#' @param id Name of the panel-ID column.
#' @param append_classification Append classification fields to every row
#'   (delegates to [panel990::panel_label()]).
#' @param return_classification Return the per-ID classification.
#' @param print_table Print the year-by-type summary
#'   (delegates to [panel990::panel_describe()]).
#' @return An enriched panel, a per-ID classification, or (invisibly) the input.
#' @export
panel_composition <- function(
    df,
    time = "TAX_YEAR",
    id = "EIN2",
    append_classification = FALSE,
    return_classification = TRUE,
    print_table = TRUE
) {
  labeled <- panel990::panel_label(df, time = time, id = id)
  if (print_table)
    panel990::panel_describe(df, time = time, id = id, print = TRUE)
  if (append_classification) return(labeled)
  if (return_classification) {
    cls_cols <- intersect(
      c(id, "panel_type", "panel_spell", "panel_year_first", "panel_year_last",
        "panel_year_count", "panel_gap_count", "panel_gap_size_max"),
      names(labeled)
    )
    return(unique(labeled[cls_cols]))
  }
  invisible(df)
}

#' Select one filing per organization-year
#'
#' Compatibility wrapper around `panel990::panel_deduplicate()`.
#'
#' @param df Filing data frame.
#' @param by_id Organization identifier column.
#' @param by_year Filing-year column.
#' @param col_group Group-return flag column.
#' @param col_partial Partial-return flag column.
#' @param col_amended Amended-return flag column.
#' @param col_stamp Filing timestamp column.
#' @param verbose Print a summary.
#' @return A data frame with one filing per organization-year.
#' @export
deduplicate <- function(
    df,
    by_id = "EIN2",
    by_year = "TAX_YEAR",
    col_group = "RETURN_GROUP_X",
    col_partial = "RETURN_PARTIAL_X",
    col_amended = "RETURN_AMENDED_X",
    col_stamp = "RETURN_TIME_STAMP",
    verbose = TRUE
) {
  if (!by_id %in% names(df))
    stop("`by_id` column not found: ", by_id)
  if (!by_year %in% names(df))
    stop("`by_year` column not found: ", by_year)
  panel990::panel_deduplicate(
    data = df, id = by_id, year = by_year, group = col_group,
    partial = col_partial, amended = col_amended, timestamp = col_stamp,
    verbose = verbose
  )
}

#' Impute missing panel years
#'
#' Compatibility wrapper around `panel990::panel_impute()`.
#'
#' @param df Panel data frame.
#' @param panel_types Optional panel classification.
#' @param types Eligible panel boundary types (default `"persistent"`).
#' @param max_gap_size Maximum single gap length.
#' @param max_gap_count Maximum gaps per organization.
#' @param vars Numeric fields to fill.
#' @param time Panel-time column.
#' @param id Panel-ID column.
#' @param as_integers Round integer field imputations.
#' @return Panel data with inserted rows flagged by `imputed_row`.
#' @export
panel_impute <- function(
    df, panel_types = NULL, types = "persistent", max_gap_size = Inf,
    max_gap_count = Inf, vars = NULL, time = "TAX_YEAR", id = "EIN2",
    as_integers = FALSE
) {
  panel990::panel_impute(
    data = df, classification = panel_types, types = types,
    max_gap_size = max_gap_size, max_gap_count = max_gap_count, vars = vars,
    time = time, id = id, as_integers = as_integers
  )
}

#' Smooth financial variables within panel organizations
#'
#' Compatibility wrapper around `panel990::panel_smooth()`. Fiscal retains
#' the `PZ`, `PC`, and `ALL` field shortcuts.
#'
#' @param df Panel data frame.
#' @param vars `"PZ"`, `"PC"`, `"ALL"`, or numeric field names.
#' @param window Odd rolling-window width.
#' @param time Panel-time column.
#' @param id Panel-ID column.
#' @param weights Weighting mode.
#' @param verbose Print progress.
#' @param engine Must currently be `"memory"`.
#' @return Smoothed panel data.
#' @export
panel_smooth <- function(
    df, vars = "PZ", window = 3, time = "TAX_YEAR", id = "EIN2",
    weights = "equal", verbose = TRUE, engine = "memory"
) {
  if (!identical(engine, "memory"))
    stop('`engine` must currently be "memory".')
  resolved <- if (length(vars) == 1L && vars == "PZ") get_pz_fields() else
    if (length(vars) == 1L && vars == "PC") get_pc_fields() else
      if (length(vars) == 1L && vars == "ALL")
        union(get_pz_fields(), get_pc_fields()) else vars
  if (length(vars) == 1L && vars %in% c("PZ", "PC", "ALL")) {
    resolved <- intersect(resolved, names(df))
    resolved <- resolved[vapply(df[resolved], is.numeric, logical(1L))]
  }
  missing_vars <- setdiff(resolved, names(df))
  if (length(missing_vars))
    stop("Variable(s) not found in df: ", paste(missing_vars, collapse = ", "))
  if (!weights %in% c("equal", "half", "decay"))
    stop("`weights` must be one of: equal, half, decay")
  panel990::panel_smooth(
    data = df, vars = resolved, window = window, weights = weights,
    time = time, id = id, verbose = verbose
  )
}

#' Retrieve a multi-year IRS 990 efile panel
#'
#' Fiscal convenience wrapper around `panel990::panelize()`, optionally
#' attaching the current geocoded BMF master.
#'
#' @param years Tax years.
#' @param tables Aliases or literal table names.
#' @param include_bmf Attach BMF fields.
#' @param efile_root Efile URL or local fixture directory.
#' @param bmf_url BMF URL, local path, or in-memory data frame.
#' @param timeout Download timeout.
#' @param retry_max Download attempts.
#' @param verbose Print progress.
#' @param backend Retrieval backend; `"memory"` or `"duckdb"`.
#' @param path Retained CSV cache directory.
#' @param cache `"retain"` or `"temporary"`.
#' @param filters Optional named source filters.
#' @param columns Optional fields to retain.
#' @param include_many Join one-to-many and supplemental tables.
#' @param collision Non-key collision policy.
#' @param overwrite Replace cached CSVs.
#' @return A data frame with retrieval manifests attached as attributes.
#' @export
get_panel <- function(
    years,
    tables = c("P00", "P01", "P08", "P09", "P10", "P11", "P12", "A01"),
    include_bmf = TRUE,
    efile_root = "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/",
    bmf_url = .BMF_URL,
    timeout = 300,
    retry_max = 3L,
    verbose = TRUE,
    backend = "memory",
    path = "efdata",
    cache = "retain",
    filters = NULL,
    columns = NULL,
    include_many = FALSE,
    collision = "error",
    overwrite = FALSE
) {
  if (!backend %in% c("memory", "duckdb"))
    stop("`backend` must be 'memory' or 'duckdb'.")
  # panelize() sets the filing keys automatically and returns a `panel` object;
  # BMF is attached explicitly below so the caller's `bmf_url` still applies.
  p <- panel990::panelize(
    tables = tables, years = years,
    source = panel990::data_source(efile_root),
    bmf = FALSE, path = path, cache = cache, filters = filters,
    columns = columns, include_many = include_many, collision = collision,
    overwrite = overwrite, retry_max = retry_max, timeout = timeout,
    verbose = verbose, backend = backend
  )
  out <- as.data.frame(p)
  if (include_bmf)
    out <- panel990::bmf_merge(out, source = bmf_url, verbose = verbose)
  attr(out, "download_manifest") <- p$download_manifest
  attr(out, "table_manifest") <- p$table_manifest
  attr(out, "join_manifest") <- p$join_manifest
  out
}

#' Merge current geocoded BMF fields onto a panel
#'
#' Compatibility wrapper around `panel990::bmf_merge()`.
#'
#' @param df Panel data containing `EIN2`.
#' @param bmf_path BMF URL, local CSV path, or data frame.
#' @param timeout Retained for compatibility; URL timeout uses the active R
#'   download settings.
#' @param verbose Print progress.
#' @return Panel rows with native BMF fields and diagnostics attached.
#' @export
merge_bmf <- function(df, bmf_path = .BMF_URL, timeout = 300, verbose = TRUE) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)
  panel990::bmf_merge(df, source = bmf_path, verbose = verbose)
}
