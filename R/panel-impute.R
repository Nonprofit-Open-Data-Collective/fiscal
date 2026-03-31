#' Impute missing panel waves for interlopers
#'
#' @description
#' Completes missing interior panel waves for organizations classified as
#' `"interloper"` and imputes selected numeric variables using the average of
#' the nearest prior and subsequent observed values.
#'
#' Missing rows are inserted only for years that actually exist in the observed
#' panel, so the function is robust to non-contiguous panel years.
#'
#' For runs of multiple consecutive missing panel waves, each missing wave is
#' imputed as the average of the nearest observed value before the gap and the
#' nearest observed value after the gap. This means all missing waves inside
#' the same gap receive the same imputed value.
#'
#' @param df A panel data frame.
#' @param panel_types Output from `panel_composition()`, or a classification
#'   data frame with columns for `id`, `first_year`, `last_year`, and `group`.
#' @param vars Character vector of variables to impute. Default is `NULL`, which
#'   selects all numeric columns except `id` and `time`.
#' @param time Name of the time variable. Default is `"TAX_YEAR"`.
#' @param id Name of the ID variable. Default is `"EIN2"`.
#' @param as_integers Logical. If `TRUE`, imputed values are rounded and cast
#'   back to integer-like storage where practical. If `FALSE` (default),
#'   imputed values are kept as numeric doubles.
#'
#' @return A data frame with missing interior panel waves inserted for
#'   interlopers. Added rows are flagged with `imputed_row = TRUE`.
#'
#' @export
panel_impute_interlopers <- function(
    df,
    panel_types,
    vars = NULL,
    time = "TAX_YEAR",
    id   = "EIN2",
    as_integers = FALSE
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

  if ( !is.logical( as_integers ) || length( as_integers ) != 1L || is.na( as_integers ) ) {
    stop( "`as_integers` must be TRUE or FALSE." )
  }

  class_df <- .get_panel_classification( x = panel_types, id = id )

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
    rownames( dt ) <- NULL
    return( as.data.frame( dt ) )
  }

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

  panel_years <- sort( unique( dt[[ time ]] ) )

  if ( length( panel_years ) == 0L ) {
    stop( "No panel years found." )
  }

  year_lookup <- data.table::data.table(
    panel_year_value = panel_years,
    panel_year_pos   = seq_along( panel_years )
  )

  year_lookup_join <- data.table::copy( year_lookup )
  data.table::setnames( year_lookup_join, "panel_year_value", time )

  keep_cols <- unique( c( id, time, vars, "imputed_row" ) )
  inter_obs <- dt[ get( id ) %in% inter_dt[[ id ]], ..keep_cols ]
  inter_obs <- year_lookup_join[ inter_obs, on = time ]

  first_lookup <- data.table::copy( year_lookup )
  data.table::setnames(
    first_lookup,
    c( "panel_year_value", "panel_year_pos" ),
    c( "first_year", "first_pos" )
  )

  last_lookup <- data.table::copy( year_lookup )
  data.table::setnames(
    last_lookup,
    c( "panel_year_value", "panel_year_pos" ),
    c( "last_year", "last_pos" )
  )

  inter_dt <- first_lookup[ inter_dt, on = "first_year" ]
  inter_dt <- last_lookup[ inter_dt, on = "last_year" ]

  obs_pos <- unique(
    inter_obs[ , c( id, "panel_year_pos" ), with = FALSE ]
  )

  span_pos <- inter_dt[ ,
    .( panel_year_pos = seq.int( first_pos, last_pos ) ),
    by = id
  ]

  missing_pos <- span_pos[
    !obs_pos,
    on = c( id, "panel_year_pos" )
  ]

  if ( nrow( missing_pos ) == 0L ) {
    rownames( dt ) <- NULL
    return( as.data.frame( dt ) )
  }

  missing_rows <- year_lookup[ missing_pos, on = "panel_year_pos" ]
  data.table::setnames( missing_rows, "panel_year_value", time )

  missing_rows <- missing_rows[ , c( id, time, "panel_year_pos" ), with = FALSE ]
  missing_rows[ , ( vars ) := NA_real_ ]
  missing_rows[ , imputed_row := TRUE ]

  inter_work <- data.table::rbindlist(
    list(
      inter_obs[ , c( id, time, "panel_year_pos", vars, "imputed_row" ), with = FALSE ],
      missing_rows
    ),
    use.names = TRUE,
    fill = TRUE
  )

  # convert imputation vars to double so fractional averages are legal
  # and do not trigger integer64 truncation warnings
  original_classes <- lapply( vars, function( v ) class( df[[ v ]] ) )
  names( original_classes ) <- vars

  for ( v in vars ) {
    data.table::set(
      inter_work,
      j = v,
      value = as.numeric( inter_work[[ v ]] )
    )
  }

  data.table::setorderv( inter_work, c( id, "panel_year_pos" ) )

  for ( v in vars ) {

    prev_col <- paste0( v, "__prev" )
    next_col <- paste0( v, "__next" )

    inter_work[ ,
      ( prev_col ) := data.table::nafill( get( v ), type = "locf" ),
      by = id
    ]

    inter_work[ ,
      ( next_col ) := data.table::nafill( get( v ), type = "nocb" ),
      by = id
    ]

    inter_work[
      imputed_row == TRUE &
      is.na( get( v ) ) &
      !is.na( get( prev_col ) ) &
      !is.na( get( next_col ) ),
      ( v ) := ( get( prev_col ) + get( next_col ) ) / 2
    ]

    if ( isTRUE( as_integers ) ) {
      # round only the newly imputed values
      inter_work[
        imputed_row == TRUE & !is.na( get( v ) ),
        ( v ) := round( get( v ) )
      ]
    }
  }

  helper_cols <- grep( "__prev$|__next$", names( inter_work ), value = TRUE )
  if ( length( helper_cols ) > 0L ) {
    inter_work[ , ( helper_cols ) := NULL ]
  }

  non_inter <- dt[ !get( id ) %in% inter_dt[[ id ]] ]

  inter_original_full <- dt[ get( id ) %in% inter_dt[[ id ]] ]

  inserted_keys <- inter_work[
    imputed_row == TRUE,
    c( id, time ),
    with = FALSE
  ]

  inter_original_full <- inter_original_full[
    !inserted_keys,
    on = c( id, time )
  ]

  missing_full <- inter_work[
    imputed_row == TRUE,
    c( id, time, vars, "imputed_row" ),
    with = FALSE
  ]

  # optionally cast imputed columns back toward integer-like storage
  if ( isTRUE( as_integers ) ) {
    for ( v in vars ) {
      cls <- original_classes[[ v ]]

      if ( "integer64" %in% cls ) {
        if ( requireNamespace( "bit64", quietly = TRUE ) ) {
          missing_full[[ v ]] <- bit64::as.integer64( missing_full[[ v ]] )
        }
      } else if ( "integer" %in% cls ) {
        missing_full[[ v ]] <- as.integer( missing_full[[ v ]] )
      }
    }
  }

  other_cols <- setdiff( names( dt ), names( missing_full ) )
  for ( v in other_cols ) {
    missing_full[[ v ]] <- NA
  }

  data.table::setcolorder( missing_full, names( dt ) )

  out <- data.table::rbindlist(
    list(
      non_inter,
      inter_original_full,
      missing_full
    ),
    use.names = TRUE,
    fill = TRUE
  )

  data.table::setorderv( out, c( id, time ) )

  rownames( out ) <- NULL
  as.data.frame( out )
}
