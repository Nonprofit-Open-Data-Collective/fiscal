#' Summarize panel composition over time
#'
#' Classifies organizations in a panel into four groups:
#'
#' - balanced: present in every period
#' - entrants: enter after the first panel year and remain through the end
#' - exits: present in the first panel year and leave before the final year
#' - interlopers: observed intermittently, with one or more missing years in the middle
#'
#' If an organization could qualify as an entrant or exit but has a gap in the
#' middle of its observed years, it is classified as an interloper.
#'
#' The returned table has one row per year and four count columns:
#'
#' - `balanced`: repeated in every year, equal to the total number of balanced organizations
#' - `entrants`: counted only in the year of first appearance
#' - `exits`: counted only in the year of last appearance
#' - `interlopers`: counted in each year they are observed
#'
#' @param df A data frame containing panel data.
#' @param time Name of the time variable. Default is `"TAX_YEAR"`.
#' @param id Name of the organization identifier variable. Default is `"EIN2"`.
#' @param return_classification Logical. If `TRUE`, attach the ID-level
#'   classification table as the `"classification"` attribute. Default is `TRUE`.
#' @param print_table Logical. If `TRUE`, print the summary table with
#'   comma-formatted counts. Default is `TRUE`.
#'
#' @return A data frame with one row per year and columns:
#' \describe{
#'   \item{year}{Panel year.}
#'   \item{balanced}{Count of balanced organizations, repeated in each year.}
#'   \item{entrants}{Count of entrant organizations in the year they first appear.}
#'   \item{exits}{Count of exit organizations in the year they last appear.}
#'   \item{interlopers}{Count of interloper organizations observed in each year.}
#' }
#'
#' If `return_classification = TRUE`, the returned object also includes an
#' attribute named `"classification"` with one row per ID and columns for
#' ID, first year, last year, number of years observed, contiguity, and group.
#'
#' @export
panel_composition <- function(
    df,
    time = "TAX_YEAR",
    id   = "EIN2",
    return_classification = FALSE,
    print_table = TRUE
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( !time %in% names( df ) ) {
    stop( paste0( "`time` column not found in `df`: ", time ) )
  }

  if ( !id %in% names( df ) ) {
    stop( paste0( "`id` column not found in `df`: ", id ) )
  }

  dt <- data.table::as.data.table( df )

  dt <- dt[
    !is.na( get( id ) ) & !is.na( get( time ) ),
    .SD,
    .SDcols = c( id, time )
  ]

  if ( nrow( dt ) == 0L ) {
    stop( "No non-missing ID/time observations found." )
  }

  # one row per id-year
  dt <- unique( dt )

  panel_years <- sort( unique( dt[[ time ]] ) )

  if ( length( panel_years ) == 0L ) {
    stop( "No panel years found." )
  }

  first_panel   <- min( panel_years )
  last_panel    <- max( panel_years )
  n_panel_years <- length( panel_years )

  # map each observed year to its position in the full panel
  year_lookup <- data.table::data.table(
    year_value = panel_years,
    year_pos   = seq_along( panel_years )
  )

  dt_pos <- data.table::copy( dt )
  data.table::setnames( year_lookup, "year_value", time )

  dt_pos <- year_lookup[
    dt_pos,
    on = time
  ]

  # summarize to org level using panel-year positions
  class_dt <- dt_pos[ ,
    .(
      first_year = min( get( time ) ),
      last_year  = max( get( time ) ),
      n_years    = data.table::uniqueN( get( time ) ),
      first_pos  = min( year_pos ),
      last_pos   = max( year_pos )
    ),
    by = id
  ]

  # robust contiguity check based on observed positions in the panel-year sequence
  class_dt[ ,
    is_contiguous := ( last_pos - first_pos + 1L ) == n_years
  ]

  class_dt[ ,
    group := data.table::fcase(
      n_years == n_panel_years,
        "balanced",

      is_contiguous & first_year > first_panel & last_year == last_panel,
        "entrant",

      is_contiguous & first_year == first_panel & last_year < last_panel,
        "exit",

      default = "interloper"
    )
  ]

  out <- data.table::data.table(
    year        = panel_years,
    balanced    = 0L,
    entrants    = 0L,
    exits       = 0L,
    interlopers = 0L
  )

  n_balanced <- class_dt[ group == "balanced", .N ]
  out[ , balanced := as.integer( n_balanced ) ]

  entrant_counts <- class_dt[
    group == "entrant",
    .( entrants = .N ),
    by = .( year = first_year )
  ]

  if ( nrow( entrant_counts ) > 0L ) {
    out[ entrant_counts, entrants := i.entrants, on = "year" ]
  }

  exit_counts <- class_dt[
    group == "exit",
    .( exits = .N ),
    by = .( year = last_year )
  ]

  if ( nrow( exit_counts ) > 0L ) {
    out[ exit_counts, exits := i.exits, on = "year" ]
  }

  class_key <- class_dt[ , c( id, "group" ), with = FALSE ]

  inter_counts <- dt[
    class_key,
    on = id,
    nomatch = 0L
  ][
    group == "interloper",
    .( interlopers = .N ),
    by = .( year = get( time ) )
  ]

  if ( nrow( inter_counts ) > 0L ) {
    out[ inter_counts, interlopers := i.interlopers, on = "year" ]
  }

  out[ is.na( entrants ), entrants := 0L ]
  out[ is.na( exits ), exits := 0L ]
  out[ is.na( interlopers ), interlopers := 0L ]

  out_df <- as.data.frame( out )

  if ( isTRUE( return_classification ) ) {
    class_df <- as.data.frame( class_dt )
    class_df$first_pos <- NULL
    class_df$last_pos  <- NULL
    attr( out_df, "classification" ) <- class_df
  }

  if ( isTRUE( print_table ) ) {
    print( .format_panel_composition_table( out_df ), row.names = FALSE )
  }

  out_df
}


#' Format panel composition table for printing
#'
#' @keywords internal
.format_panel_composition_table <- function( x ) {

  out <- x

  count_cols <- c( "balanced", "entrants", "exits", "interlopers" )
  count_cols <- intersect( count_cols, names( out ) )

  for ( v in count_cols ) {
    out[[ v ]] <- format( out[[ v ]], big.mark = ",", trim = TRUE, scientific = FALSE )
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
#' @return One of: `"balanced"`, `"entrant"`, `"exit"`, or `"interloper"`.
#'
#' @keywords internal
.classify_panel_pattern <- function(
    obs_years,
    panel_years
) {

  obs_years <- sort( unique( obs_years ) )
  panel_years <- sort( unique( panel_years ) )

  if ( length( obs_years ) == 0L ) {
    return( "interloper" )
  }

  if ( identical( obs_years, panel_years ) ) {
    return( "balanced" )
  }

  obs_idx <- match( obs_years, panel_years )

  is_contiguous <- identical(
    obs_idx,
    seq.int( from = min( obs_idx ), to = max( obs_idx ) )
  )

  first_obs   <- min( obs_years )
  last_obs    <- max( obs_years )
  first_panel <- min( panel_years )
  last_panel  <- max( panel_years )

  if ( is_contiguous &&
       first_obs > first_panel &&
       last_obs == last_panel ) {
    return( "entrant" )
  }

  if ( is_contiguous &&
       first_obs == first_panel &&
       last_obs < last_panel ) {
    return( "exit" )
  }

  "interloper"
}

