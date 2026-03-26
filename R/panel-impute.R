#' Impute missing interior years for interlopers
#'
#' Completes missing interior years for organizations classified as
#' `"interloper"` and imputes selected variables using the average of the
#' nearest prior and subsequent observed values.
#'
#' Missing years are filled only between each organization's `first_year` and
#' `last_year`, as defined in the classification table. Leading and trailing
#' years outside an organization's observed span are not created.
#'
#' For runs of multiple consecutive missing years, each missing year is imputed
#' as the average of the nearest observed value before the gap and the nearest
#' observed value after the gap. This means all missing years in the same gap
#' receive the same imputed value.
#'
#' @param df A panel data frame.
#' @param panel_types Output from `panel_composition()`, or a classification
#' data frame with columns for `id`, `first_year`, `last_year`, and `group`.
#' @param vars Character vector of variables to impute. Default is `NULL`, which
#' selects all numeric columns except `id` and `time`.
#' @param time Name of the time variable. Default is `"TAX_YEAR"`.
#' @param id Name of the ID variable. Default is `"EIN2"`.
#'
#' @return A data frame with missing interior years inserted for interlopers.
#' Added rows are flagged with `imputed_row = TRUE`.
#'
#' @export
panel_impute_interlopers <- function(
    df,
    panel_types,
    vars = NULL,
    time = "TAX_YEAR",
    id   = "EIN2"
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( !id %in% names( df ) ) {
    stop( paste0( "`id` column not found in `df`: ", id ) )
  }

  if ( !time %in% names( df ) ) {
    stop( paste0( "`time` column not found in `df`: ", time ) )
  }

  class_df <- .get_panel_classification(
    x  = panel_types,
    id = id
  )

  need_cols <- c( id, "first_year", "last_year", "group" )
  missing_class_cols <- need_cols[ !need_cols %in% names( class_df ) ]
  if ( length( missing_class_cols ) > 0L ) {
    stop(
      paste0(
        "Classification table is missing required column(s): ",
        paste( missing_class_cols, collapse = ", " )
      )
    )
  }

  if ( is.null( vars ) ) {
    vars <- names( df )[ vapply( df, is.numeric, logical( 1 ) ) ]
    vars <- setdiff( vars, time )
  }

  if ( !is.character( vars ) || length( vars ) == 0L ) {
    stop( "`vars` must be NULL or a non-empty character vector." )
  }

  missing_vars <- vars[ !vars %in% names( df ) ]
  if ( length( missing_vars ) > 0L ) {
    stop(
      paste0(
        "Variable(s) in `vars` not found in `df`: ",
        paste( missing_vars, collapse = ", " )
      )
    )
  }

  non_numeric <- vars[ !vapply( df[ vars ], is.numeric, logical( 1 ) ) ]
  if ( length( non_numeric ) > 0L ) {
    stop(
      paste0(
        "All variables in `vars` must be numeric. Non-numeric: ",
        paste( non_numeric, collapse = ", " )
      )
    )
  }

  dt <- data.table::as.data.table( df )
  dt[ , imputed_row := FALSE ]

  inter_dt <- data.table::as.data.table(
    class_df[ class_df$group == "interloper", c( id, "first_year", "last_year" ), drop = FALSE ]
  )

  if ( nrow( inter_dt ) == 0L ) {
    return( as.data.frame( dt ) )
  }

  # ensure one row per id-time among interlopers
  dupes <- dt[
    get( id ) %in% inter_dt[[ id ]],
    .N,
    by = c( id, time )
  ][ N > 1L ]

  if ( nrow( dupes ) > 0L ) {
    stop(
      paste0(
        "`panel_impute_interlopers()` requires one row per `id`-`time` pair ",
        "for interloper records. Found duplicates."
      )
    )
  }

  # build completed interior panel for interlopers only
  inter_ranges <- inter_dt[ ,
    .( year = seq.int( first_year, last_year ) ),
    by = id
  ]

  data.table::setnames( inter_ranges, "year", time )

  inter_obs <- dt[ get( id ) %in% inter_dt[[ id ]] ]
  non_inter <- dt[ !get( id ) %in% inter_dt[[ id ]] ]

  inter_full <- inter_ranges[
    inter_obs,
    on = c( id, time )
  ]

  # rows newly created by the completion step
  inter_full[ , imputed_row := is.na( imputed_row ) ]
  inter_full[ is.na( imputed_row ), imputed_row := TRUE ]

  data.table::setorderv( inter_full, c( id, time ) )

  # fill selected vars using nearest observed values around each gap
  for ( v in vars ) {

    inter_full[ ,
      paste0( v, "__prev" ) := data.table::nafill( get( v ), type = "locf" ),
      by = id
    ]

    inter_full[ ,
      paste0( v, "__next" ) := data.table::nafill( get( v ), type = "nocb" ),
      by = id
    ]

    prev_col <- paste0( v, "__prev" )
    next_col <- paste0( v, "__next" )

    inter_full[
      imputed_row == TRUE &
      is.na( get( v ) ) &
      !is.na( get( prev_col ) ) &
      !is.na( get( next_col ) ),
      ( v ) := ( get( prev_col ) + get( next_col ) ) / 2
    ]
  }

  # drop helper columns
  helper_cols <- grep( "__prev$|__next$", names( inter_full ), value = TRUE )
  if ( length( helper_cols ) > 0L ) {
    inter_full[ , ( helper_cols ) := NULL ]
  }

  out <- data.table::rbindlist(
    list( non_inter, inter_full ),
    use.names = TRUE,
    fill = TRUE
  )

  data.table::setorderv( out, c( id, time ) )

  rownames( out ) <- NULL
  as.data.frame( out )
}

#' Normalize panel group labels
#'
#' @keywords internal
.normalize_panel_groups <- function( x ) {

  if ( !is.character( x ) || length( x ) == 0L ) {
    stop( "`keep` must be a non-empty character vector." )
  }

  x <- tolower( x )

  map <- c(
    "balanced"    = "balanced",
    "entrant"     = "entrant",
    "entrants"    = "entrant",
    "exit"        = "exit",
    "exits"       = "exit",
    "interloper"  = "interloper",
    "interlopers" = "interloper"
  )

  bad <- x[ !x %in% names( map ) ]
  if ( length( bad ) > 0L ) {
    stop(
      paste0(
        "Unknown group label(s): ",
        paste( unique( bad ), collapse = ", " )
      )
    )
  }

  unname( unique( map[ x ] ) )
}

#' Extract panel classification table
#'
#' @keywords internal
.get_panel_classification <- function(
    x,
    id = "EIN2"
) {

  # case 1: panel_composition() output
  class_df <- attr( x, "classification" )

  if ( is.data.frame( class_df ) ) {
    if ( !id %in% names( class_df ) ) {
      stop( paste0( "Classification table is missing ID column: ", id ) )
    }
    return( class_df )
  }

  # case 2: classification data frame passed directly
  if ( is.data.frame( x ) && "group" %in% names( x ) && id %in% names( x ) ) {
    return( x )
  }

  stop(
    paste0(
      "`panel_types` must be either:\n",
      "- output from `panel_composition()` with a `classification` attribute, or\n",
      "- a classification data frame containing `", id, "` and `group`."
    )
  )
}

