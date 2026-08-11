#' Summarize panel composition over time
#'
#' Classifies each organization in a panel dataset according to its pattern
#' of presence across the observed time window, then returns a year-by-type
#' count table and (optionally) appends or attaches the full ID-level
#' classification.
#'
#' @section Panel types:
#' Every organization is assigned one of five mutually exclusive types based
#' solely on whether it is observed in the **first** and **last** year of the
#' panel:
#'
#' \describe{
#'   \item{`full`}{Present in both the first and last panel year. May have
#'     interior gaps.}
#'   \item{`entry`}{Absent in the first panel year, present in the last.
#'     Contiguous or fragmented interior.}
#'   \item{`exit`}{Present in the first panel year, absent in the last.
#'     Contiguous or fragmented interior.}
#'   \item{`interior`}{Absent from both the first and last panel year.
#'     Observed only within the interior of the panel window.}
#'   \item{`empty`}{No observations found for this ID within the panel years.
#'     Only possible when the ID universe is supplied externally (e.g. a
#'     factor with unused levels).}
#' }
#'
#' @section Spell balance:
#' Within each type, organizations are further characterized by whether their
#' observed years form a single unbroken run:
#'
#' \describe{
#'   \item{`contiguous`}{All years from first to last observation are present;
#'     no interior gaps.}
#'   \item{`fragmented`}{One or more interior years are missing between the
#'     first and last observation.}
#' }
#'
#' @section Gap measures:
#' Two fields quantify fragmentation. Both are measured over the interior span
#' only (first to last observed year), so leading or trailing absences that
#' define the panel type are excluded:
#'
#' \describe{
#'   \item{`panel_gap_count`}{Number of distinct missing-year runs within the
#'     interior span. Zero for contiguous organizations.}
#'   \item{`panel_gap_size_max`}{Length of the longest single missing-year run.
#'     Zero for contiguous organizations.}
#' }
#'
#' Examples (4-year panel, x = observed, o = absent):
#'
#' \tabular{lllll}{
#'   Pattern \tab panel_type \tab panel_spell_balance \tab gap_count \tab gap_size_max \cr
#'   xxxx \tab full     \tab contiguous \tab 0 \tab 0 \cr
#'   xoxx \tab full     \tab fragmented \tab 1 \tab 1 \cr
#'   xoox \tab full     \tab fragmented \tab 1 \tab 2 \cr
#'   ooxx \tab entry    \tab contiguous \tab 0 \tab 0 \cr
#'   oxox \tab entry    \tab fragmented \tab 1 \tab 1 \cr
#'   xxoo \tab exit     \tab contiguous \tab 0 \tab 0 \cr
#'   xoxo \tab exit     \tab fragmented \tab 1 \tab 1 \cr
#'   oxoo \tab interior \tab contiguous \tab 0 \tab 0 \cr
#'   oxxo \tab interior \tab fragmented \tab 0 \tab 0 \cr
#' }
#'
#' @section Chaining:
#' When `append_classification = TRUE`, the function returns the original
#' data frame `df` with seven new `panel_*` columns merged in by `id`,
#' making it easy to chain into downstream steps:
#'
#' ```r
#' d <- panel_composition(d, append_classification = TRUE)
#' d <- panel_impute(d, xox = TRUE, max_contiguous_missing = 1)
#' ```
#'
#' The summary table is still printed. Any pre-existing `panel_*` columns
#' are replaced automatically to avoid conflicts when re-running.
#'
#' @param df A data frame containing panel data.
#' @param time Name of the time variable column. Default `"TAX_YEAR"`.
#' @param id Name of the organization identifier column. Default `"EIN2"`.
#' @param append_classification Logical. If `TRUE`, merge the seven
#'   `panel_*` classification columns directly onto `df` and return the
#'   enriched data frame. Intended for pipeline use. Pre-existing `panel_*`
#'   columns are dropped before merging to prevent duplicates. The summary
#'   table is still printed. Default `FALSE`.
#' @param return_classification Logical. When `append_classification = FALSE`,
#'   attach the per-ID classification data frame as the `"classification"`
#'   attribute of the returned summary table. Default `TRUE`.
#' @param print_table Logical. If `TRUE`, print the year-by-type summary
#'   table with comma-formatted counts. Default `TRUE`.
#'
#' @return
#' **When `append_classification = FALSE` (default):**
#' A data frame with one row per panel year and columns `year`, `full`,
#' `entry`, `exit`, `interior`, and `empty`. Each cell is the count of
#' organizations of that type observed in that year. If
#' `return_classification = TRUE`, the per-ID classification is accessible
#' via `attr(result, "classification")`.
#'
#' **When `append_classification = TRUE`:**
#' The original `df` returned invisibly with seven additional columns:
#'
#' \describe{
#'   \item{`panel_year_first`}{First panel year the organization was observed.}
#'   \item{`panel_year_last`}{Last panel year the organization was observed.}
#'   \item{`panel_year_count`}{Number of panel years the organization was observed.}
#'   \item{`panel_type`}{One of `full`, `entry`, `exit`, `interior`, `empty`.}
#'   \item{`panel_spell_balance`}{One of `contiguous` or `fragmented`.}
#'   \item{`panel_gap_count`}{Number of interior missing-year spells.}
#'   \item{`panel_gap_size_max`}{Length of the longest interior missing-year run.}
#' }
#'
#' @seealso [panel_impute()] for imputing missing years identified by this
#'   function; [get_panel()] for assembling the underlying panel dataset.
#'
#' @examples
#' \dontrun{
#' # Basic summary table
#' pc <- panel_composition(d)
#'
#' # Pipeline usage: classify and continue
#' d <- panel_composition(d, append_classification = TRUE)
#' table(d$panel_type)
#' table(d$panel_type, d$panel_spell_balance)
#'
#' # Inspect fragmented full-span organizations with large gaps
#' subset(d, panel_type == "full" & panel_spell_balance == "fragmented" &
#'            panel_gap_size_max >= 2)
#'
#' # Backward-compatible attribute access
#' pc <- panel_composition(d, return_classification = TRUE)
#' ptypes <- attr(pc, "classification")
#'
#' # Non-default column names
#' pc <- panel_composition(d, time = "FISCAL_YEAR", id = "ORG_ID",
#'                         append_classification = TRUE)
#' }
#'
#' @noRd
.panel_composition_legacy <- function(
    df,
    time = "TAX_YEAR",
    id   = "EIN2",
    append_classification = FALSE,
    return_classification = TRUE,
    print_table = TRUE
) {

  if (!is.data.frame(df))           stop("`df` must be a data.frame.")
  if (!time %in% names(df))         stop(paste0("`time` column not found in `df`: ", time))
  if (!id   %in% names(df))         stop(paste0("`id` column not found in `df`: ", id))
  if (!is.logical(append_classification) || length(append_classification) != 1L ||
      is.na(append_classification))
    stop("`append_classification` must be TRUE or FALSE.")
  if (!is.logical(return_classification) || length(return_classification) != 1L ||
      is.na(return_classification))
    stop("`return_classification` must be TRUE or FALSE.")
  if (!is.logical(print_table) || length(print_table) != 1L || is.na(print_table))
    stop("`print_table` must be TRUE or FALSE.")

  # -- 1. Minimal copy ----------------------------------------------------------
  dt <- data.table::as.data.table(df)[
    !is.na(get(id)) & !is.na(get(time)),
    .SD,
    .SDcols = c(id, time)
  ]
  dt <- unique(dt)
  data.table::setkeyv(dt, c(id, time))

  if (nrow(dt) == 0L) stop("No non-missing ID/time observations found.")

  panel_years   <- sort(unique(dt[[time]]))
  n_panel_years <- length(panel_years)
  if (n_panel_years == 0L) stop("No panel years found.")

  first_panel <- panel_years[1L]
  last_panel  <- panel_years[n_panel_years]

  # -- 2. Map years -> positions -------------------------------------------------
  year_lookup <- data.table::data.table(
    year_val = panel_years,
    year_pos = seq_along(panel_years)
  )
  dt <- year_lookup[dt, on = c(year_val = time)]

  # -- 3. Classify per-ID -------------------------------------------------------
  pos_by_id <- dt[, .(positions = list(sort(year_pos))), by = id]

  classifications <- lapply(pos_by_id$positions, function(pos) {
    obs_years_local <- panel_years[pos]
    .classify_panel_pattern(obs_years_local, panel_years)
  })

  class_dt <- cbind(
    pos_by_id[, id, with = FALSE],
    data.table::rbindlist(classifications)
  )

  summary_dt <- dt[,
    .(
      year_first = panel_years[min(year_pos)],
      year_last  = panel_years[max(year_pos)],
      year_count = .N
    ),
    by = id
  ]

  class_dt <- summary_dt[class_dt, on = id]

  # -- 4. Build summary output table --------------------------------------------
  # Join panel_type back to the per-row dt; then count by year - panel_type
  # class_dt has one row per ID with panel_type; dt has one row per ID x year
  # Use id column name consistently
  type_lookup <- unique(class_dt[, c(id, "panel_type"), with = FALSE])

  dt_typed <- type_lookup[dt, on = id]

  out <- dt_typed[
    !is.na(panel_type) & panel_type != "empty",
    .N,
    by = .(year = year_val, panel_type)
  ]

  # Pivot wide
  out <- data.table::dcast(out, year ~ panel_type, value.var = "N", fill = 0L)

  # Ensure all type columns present even if a type has zero obs
  for (ptype in .PANEL_TYPES) {
    if (!ptype %in% names(out)) out[[ptype]] <- 0L
  }

  # empty orgs have no rows in dt so report as totals only
  empty_total <- class_dt[panel_type == "empty", .N]
  out[, empty := empty_total]   # same count every year, or handle separately

  data.table::setcolorder(out, intersect(
    c("year", "full", "entry", "exit", "interior", "empty"),
    names(out)
  ))

  out_df <- as.data.frame(out)

# -- 5. Build classification data frame --------------------------------------
  class_df <- as.data.frame(class_dt)

  # Rename to panel_* namespace
  name_map <- c(
    year_first    = "panel_year_first",
    year_last     = "panel_year_last",
    year_count    = "panel_year_count",
    panel_type    = "panel_type",
    spell_balance = "panel_spell_balance",
    gap_count     = "panel_gap_count",
    gap_size_max  = "panel_gap_size_max"
  )

  for (old in names(name_map)) {
    if (old %in% names(class_df) && old != name_map[[old]]) {
      names(class_df)[names(class_df) == old] <- name_map[[old]]
    }
  }

  # -- 6. Print summary table ---------------------------------------------------
  if (isTRUE(print_table))
    print(.format_panel_composition_table(out_df), row.names = FALSE)

  # -- 7. Return ----------------------------------------------------------------
  panel_cols <- unname(name_map)

  if (isTRUE(append_classification)) {

    # Drop any pre-existing panel_* columns from df to avoid conflicts
    existing_panel_cols <- intersect(panel_cols, names(df))
    if (length(existing_panel_cols) > 0L)
      df <- df[, setdiff(names(df), existing_panel_cols), drop = FALSE]

    # Merge classification onto df by id - one row per org so left join is safe
    merge_cols <- c(id, panel_cols)
    merge_cols <- intersect(merge_cols, names(class_df))

    df <- merge(
      df,
      class_df[, merge_cols, drop = FALSE],
      by = id,
      all.x = TRUE,
      sort = FALSE
    )

    return(invisible(df))
  }

  if (isTRUE(return_classification)) {
    attr(out_df, "classification") <- class_df
  }

  out_df

}


#' Format panel composition table for printing
#'
#' @keywords internal
.format_panel_composition_table <- function(x) {

  out <- x

  # Detect count columns: integer or numeric columns that aren't the year column
  count_cols <- names(out)[
    sapply(out, is.numeric) & names(out) != "year"
  ]

  for (v in count_cols) {
    out[[v]] <- format(out[[v]], big.mark = ",", trim = TRUE, scientific = FALSE)
  }

  out
}




#' Classify one panel membership pattern
#'
#' Internal helper used by `panel_composition()`.
#'
#' @param obs_years Sorted unique years observed for one ID.
#' @param panel_years Sorted unique years in the full panel.
#'
#' @return A list containing `panel_type` (`"full"`, `"entry"`, `"exit"`,
#'   `"interior"`, or `"empty"`), `spell_balance` (`"contiguous"` or
#'   `"fragmented"`), and gap counts.
#'
#' @keywords internal
.classify_panel_pattern <- function(obs_years, panel_years) {

  obs_years   <- sort(unique(obs_years))
  panel_years <- sort(unique(panel_years))

  first_panel <- panel_years[1L]
  last_panel  <- panel_years[length(panel_years)]

  # ?? empty ????????????????????????????????????????????????????????????????????
  if (length(obs_years) == 0L) {
    return(list(
      panel_type    = "empty",
      spell_balance = NA_character_,
      gap_count     = NA_integer_,
      gap_size_max  = NA_integer_
    ))
  }

  obs_idx   <- match(obs_years, panel_years)
  first_pos <- min(obs_idx)
  last_pos  <- max(obs_idx)
  n_obs     <- length(obs_idx)
  span      <- last_pos - first_pos + 1L

  first_obs <- obs_years[1L]
  last_obs  <- obs_years[n_obs]

  # ?? panel_type (boundary membership only) ???????????????????????????????????
  touches_first <- (first_obs == first_panel)
  touches_last  <- (last_obs  == last_panel)

  panel_type <- if      ( touches_first &&  touches_last) "full"
                else if (!touches_first &&  touches_last) "entry"
                else if ( touches_first && !touches_last) "exit"
                else                                      "interior"

  # ?? spell_balance ????????????????????????????????????????????????????????????
  is_contiguous <- (span == n_obs)
  spell_balance <- if (is_contiguous) "contiguous" else "fragmented"

  # ?? gap_count and gap_size_max (interior span only, censored tails excluded) ?
  if (span <= 1L || is_contiguous) {
    gap_count    <- 0L
    gap_size_max <- 0L
  } else {
    presence     <- as.integer((first_pos:last_pos) %in% obs_idx)
    rle_result   <- rle(presence)
    gap_lengths  <- rle_result$lengths[rle_result$values == 0L]
    gap_count    <- length(gap_lengths)
    gap_size_max <- if (gap_count > 0L) max(gap_lengths) else 0L
  }

  list(
    panel_type    = panel_type,
    spell_balance = spell_balance,
    gap_count     = gap_count,
    gap_size_max  = gap_size_max
  )
}

#' Print a panel composition summary from appended classification columns
#'
#' @description
#' Produces a year-by-type count table from a data frame that already has
#' `panel_*` columns appended by `panel_composition(append_classification =
#' TRUE)`. Useful for quickly re-checking composition after imputation or
#' other panel manipulations without re-running the full classification.
#'
#' @param df A data frame with `panel_type` and the `time` column present.
#' @param time Name of the time variable column. Default `"TAX_YEAR"`.
#' @param id Name of the organization identifier column. Default `"EIN2"`.
#' @param print_table Logical. If `TRUE`, print the formatted table.
#'   Default `TRUE`.
#'
#' @return A data frame with one row per panel year and one column per
#'   panel type, invisibly. Same structure as the summary table returned
#'   by `panel_composition()`.
#'
#' @seealso [panel_composition()]
#'
#' @examples
#' \dontrun{
#' d <- panel_composition(d, append_classification = TRUE)
#' d <- panel_impute(d)
#' panel_summary(d)
#' }
#'
#' @export
panel_summary <- function(
    df,
    time        = "TAX_YEAR",
    id          = "EIN2",
    print_table = TRUE
) {

  if (!is.data.frame(df))
    stop("`df` must be a data.frame.")
  if (!time %in% names(df))
    stop(paste0("`time` column not found in `df`: ", time))
  if (!"panel_type" %in% names(df))
    stop("`panel_type` column not found. Run panel_composition(append_classification = TRUE) first.")

  dt <- data.table::as.data.table(df)

  # One row per id-year (guard against duplicates)
  dt_unique <- unique(dt[!is.na(get(id)) & !is.na(get(time)),
                         c(id, time, "panel_type"), with = FALSE])

  panel_years <- sort(unique(dt_unique[[time]]))
  types       <- .PANEL_TYPES

  # Count observed rows per year per type
  counts <- dt_unique[
    panel_type %in% types,
    .N,
    by = .(year = get(time), panel_type)
  ]

  out <- data.table::dcast(counts, year ~ panel_type, value.var = "N", fill = 0L)

  # Ensure all type columns present even if a type has zero observations
  for (ptype in types) {
    if (!ptype %in% names(out))
      out[[ptype]] <- 0L
  }

  # Empty orgs have no rows to count by year -- report total as a scalar footer
  empty_total <- length(unique(dt[panel_type == "empty", get(id)]))

  data.table::setcolorder(
    out,
    intersect(c("year", types), names(out))
  )

  out_df <- as.data.frame(out)

  if (isTRUE(print_table)) {
    print(.format_panel_composition_table(out_df), row.names = FALSE)
    if (empty_total > 0L)
      message("  empty        ", format(empty_total, big.mark = ","),
              "  (no panel-year rows; total unique IDs)")
  }

  invisible(out_df)
}
