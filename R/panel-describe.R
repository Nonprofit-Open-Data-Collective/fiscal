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
#' A detailed ID-level classification table is attached as the
#' `"classification"` attribute of the returned data frame.
#'
#' @param df A data frame containing panel data.
#' @param time Name of the time variable. Default is `"TAX_YEAR"`.
#' @param id Name of the organization identifier variable. Default is `"EIN2"`.
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
#' The returned object also includes an attribute:
#'
#' \describe{
#'   \item{classification}{A data frame with one row per ID and columns for ID, group, first year, and last year.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   EIN2 = c(
#'     "A","A","A","A",
#'     "B","B","B",
#'     "C","C","C",
#'     "D","D","D"
#'   ),
#'   TAX_YEAR = c(
#'     2021,2022,2023,2024,
#'     2022,2023,2024,
#'     2021,2022,2023,
#'     2021,2023,2024
#'   )
#' )
#'
#' out <- panel_composition( df )
#' out
#'
#' attr( out, "classification" )
#'
#' @export
panel_composition <- function(
    df,
    time = "TAX_YEAR",
    id   = "EIN2"
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

  dat <- unique( df[ , c( id, time ), drop = FALSE ] )
  dat <- dat[ !is.na( dat[[ id ]] ) & !is.na( dat[[ time ]] ), , drop = FALSE ]

  panel_years <- sort( unique( dat[[ time ]] ) )

  if ( length( panel_years ) == 0L ) {
    stop( "No non-missing panel years found." )
  }

  ids <- unique( dat[[ id ]] )

  class_list <- lapply(
    ids,
    function( this_id ) {

      obs_years <- dat[ dat[[ id ]] == this_id, time ]
      obs_years <- sort( unique( obs_years ) )

      group <- .classify_panel_pattern(
        obs_years    = obs_years,
        panel_years  = panel_years
      )

      data.frame(
        org_id     = this_id,
        group      = group,
        first_year = min( obs_years ),
        last_year  = max( obs_years ),
        stringsAsFactors = FALSE
      )
    }
  )

  class_df <- do.call( rbind, class_list )
  names( class_df )[ names( class_df ) == "org_id" ] <- id

  out <- data.frame(
    year        = panel_years,
    balanced    = 0L,
    entrants    = 0L,
    exits       = 0L,
    interlopers = 0L
  )

  # balanced: same count repeated in each year
  n_balanced <- sum( class_df$group == "balanced" )
  out$balanced <- as.integer( n_balanced )

  # entrants: count only in year of first appearance
  entrant_df <- class_df[ class_df$group == "entrant", , drop = FALSE ]
  if ( nrow( entrant_df ) > 0L ) {
    entrant_tab <- table( entrant_df$first_year )
    out$entrants <- as.integer(
      entrant_tab[ match( out$year, names( entrant_tab ) ) ]
    )
    out$entrants[ is.na( out$entrants ) ] <- 0L
  }

  # exits: count only in year of last appearance
  exit_df <- class_df[ class_df$group == "exit", , drop = FALSE ]
  if ( nrow( exit_df ) > 0L ) {
    exit_tab <- table( exit_df$last_year )
    out$exits <- as.integer(
      exit_tab[ match( out$year, names( exit_tab ) ) ]
    )
    out$exits[ is.na( out$exits ) ] <- 0L
  }

  # interlopers: count in each year observed
  inter_df <- class_df[ class_df$group == "interloper", , drop = FALSE ]
  if ( nrow( inter_df ) > 0L ) {

    inter_ids <- inter_df[[ id ]]

    inter_obs <- dat[ dat[[ id ]] %in% inter_ids, c( id, time ), drop = FALSE ]
    inter_tab <- table( inter_obs[[ time ]] )

    out$interlopers <- as.integer(
      inter_tab[ match( out$year, names( inter_tab ) ) ]
    )
    out$interlopers[ is.na( out$interlopers ) ] <- 0L
  }

  attr( out, "classification" ) <- class_df

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

