# ---- panel_smooth() --------------------------------------------------

#' @title
#' Smooth financial variables across time within a panel dataset
#'
#' @description
#' Applies a centered rolling-window average within each organization's
#' time series to reduce year-to-year noise in financial variables. The
#' data frame is returned with the same dimensions and column names; only
#' the selected financial columns are replaced with their smoothed values.
#'
#' **Window behavior at panel edges:** When an observation is within
#' `floor(window / 2)` periods of the start or end of an organization's
#' series, the window shifts to stay within the available data rather than
#' shrinking. This means every smoothed value is based on exactly
#' `min(window, n)` observations, where `n` is the panel length for that
#' organization.
#'
#' **Missing values:** `NA` observations are dropped from the window
#' before computing the weighted average. Their weight is reallocated
#' proportionally to the remaining non-missing observations so the weights
#' always sum to 1. If all observations in a window are `NA`, the smoothed
#' value is `NA`.
#'
#' **`NaN` values:** `NaN` values are treated as `NA` during smoothing and
#' are restored to `NaN` in their original positions in the output.
#'
#' @param df A `data.frame` containing a panel of financial data with one
#'   row per organization-year.
#' @param vars Which financial variables to smooth. One of:
#'   \describe{
#'     \item{`"PZ"`}{Smooth all PZ-scope fields returned by
#'       [get_pz_fields()] that are present in `df`.}
#'     \item{`"PC"`}{Smooth all PC-scope fields returned by
#'       [get_pc_fields()] that are present in `df`.}
#'     \item{`"ALL"`}{Smooth all PC- and PZ-scope fields present in `df`.}
#'     \item{character vector}{A custom list of column names to smooth.
#'       All names must be present in `df`.}
#'   }
#' @param window Odd integer >= 1. Width of the rolling window in time
#'   periods. Default `3` (current year plus one year on each side).
#'   `window = 1` returns the data unchanged.
#' @param time Name of the column identifying the time period. Default
#'   `"TAX_YEAR"`.
#' @param id Name of the column identifying the organization. Default
#'   `"EIN2"`.
#' @param weights Weighting scheme for the rolling window. One of:
#'   \describe{
#'     \item{`"equal"`}{Uniform weights - each observation in the window
#'       contributes equally. Equivalent to a simple moving average.}
#'     \item{`"half"`}{The center observation receives weight 1/2; the
#'       remaining 1/2 is split equally among all other observations in
#'       the window. Emphasizes the focal year while incorporating
#'       neighboring context.}
#'     \item{`"decay"`}{Half-life exponential decay from the center:
#'       distance 0 = weight 1, distance 1 = 1/2, distance 2 = 1/4,
#'       and so on. Strongly emphasizes the focal year.}
#'   }
#' @param verbose Logical. If `TRUE` (default), prints progress messages
#'   before and after smoothing. Set to `FALSE` to suppress messages.
#' @param engine Character scalar giving the smoothing backend. Currently
#'   only `"memory"` is supported.
#'
#' @return A `data.frame` with the same dimensions and column names as
#'   `df`. The columns identified by `vars` contain smoothed values;
#'   all other columns are returned unchanged.
#'
#' @details
#' ## Choosing a weighting scheme
#'
#' `"equal"` is the simplest and most transparent option - it is the
#' standard moving average and makes no assumptions about the relative
#' importance of focal vs. neighboring years.
#'
#' `"half"` is a reasonable default for nonprofit financial ratios where
#' the reported year is the most informative but a single year's value
#' may be noisy (for example, due to one-time gifts or accrual timing).
#'
#' `"decay"` is appropriate when recent-year data is believed to be
#' progressively more informative than distant years, for example when
#' smoothing raw financial levels that have secular trends.
#'
#' ## Panel length and the window
#'
#' Organizations with fewer years than `window` are smoothed over their
#' full available history rather than being dropped or truncated. This
#' means short panels are smoothed more aggressively than long ones when
#' the same `window` is requested. Use `window = 1` to apply no smoothing.
#'
#' ## Row order
#'
#' The output rows are returned in the same order as the input `df`.
#'
#' @seealso [get_panel()], [get_pz_fields()], [get_pc_fields()]
#'
#' @examples
#' \dontrun{
#' panel <- get_panel( years = 2019:2022 )
#'
#' # Smooth all PZ-scope fields
#' panel_s <- panel_smooth( panel, vars = "PZ" )
#'
#' # Smooth a custom set of columns with decay weighting
#' panel_s <- panel_smooth(
#'   panel,
#'   vars    = c( "F9_01_REV_TOT_CY", "F9_01_EXP_TOT_CY" ),
#'   window  = 5,
#'   weights = "decay"
#' )
#' }
#'
#' @export
panel_smooth <- function(
    df,
    vars    = "PZ",
    window  = 3,
    time    = "TAX_YEAR",
    id      = "EIN2",
    weights = "equal",
    verbose = TRUE,
    engine  = "memory"
) {

  prep <- .panel_smooth_prepare(
    df      = df,
    vars    = vars,
    window  = window,
    time    = time,
    id      = id,
    weights = weights
  )

  if ( engine == "memory" ) {
    out <- .panel_smooth_memory(
      prep    = prep,
      verbose = verbose
    )
    return( out )
  }

  stop( '`engine` must currently be "memory". Batch backend not implemented yet.' )
}

.panel_smooth_prepare <- function(
    df,
    vars,
    window,
    time,
    id,
    weights
) {

  if ( !is.data.frame( df ) ) {
    stop( "`df` must be a data.frame." )
  }

  if ( length( window ) != 1 || !is.numeric( window ) || is.na( window ) ) {
    stop( "`window` must be a single odd integer >= 1." )
  }

  window <- as.integer( window )

  if ( window < 1L || window %% 2L == 0L ) {
    stop( "`window` must be an odd integer >= 1." )
  }

  if ( !time %in% names( df ) ) {
    stop( paste0( "`time` column not found in df: \"", time, "\"" ) )
  }

  if ( !id %in% names( df ) ) {
    stop( paste0( "`id` column not found in df: \"", id, "\"" ) )
  }

  allowed_weights <- c( "equal", "half", "decay" )
  if ( length( weights ) != 1L || !weights %in% allowed_weights ) {
    stop(
      paste0(
        '`weights` must be one of: "',
        paste( allowed_weights, collapse = '", "' ),
        '".'
      )
    )
  }

  if ( identical( vars, "PZ" ) ) {
    candidate <- get_pz_fields()
  } else if ( identical( vars, "PC" ) ) {
    candidate <- get_pc_fields()
  } else if ( identical( vars, "ALL" ) ) {
    candidate <- union( get_pc_fields(), get_pz_fields() )
  } else {
    if ( !is.character( vars ) || length( vars ) == 0L ) {
      stop( '`vars` must be "PZ", "PC", "ALL", or a non-empty character vector.' )
    }

    missing_cols <- vars[ !vars %in% names( df ) ]
    if ( length( missing_cols ) > 0L ) {
      stop(
        paste0(
          "Column(s) in `vars` not found in df: ",
          paste( missing_cols, collapse = ", " )
        )
      )
    }

    candidate <- vars
  }

  smooth_cols <- intersect( candidate, names( df ) )
  smooth_cols <- smooth_cols[
    vapply(
      smooth_cols,
      function( v ) is.numeric( df[[ v ]] ),
      logical( 1 )
    )
  ]

  if ( length( smooth_cols ) == 0L ) {
    return(
      list(
        df          = df,
        df_sorted   = NULL,
        smooth_cols = character( 0 ),
        time        = time,
        id          = id,
        weights     = weights,
        window      = window,
        row_index   = NULL,
        grp_start   = integer( 0 ),
        grp_end     = integer( 0 ),
        grp_n       = integer( 0 )
      )
    )
  }

  df[["..row.."]] <- seq_len( nrow( df ) )

  ord <- order( df[[ id ]], df[[ time ]], df[["..row.."]] )
  df_sorted <- df[ ord, , drop = FALSE ]

  n <- nrow( df_sorted )
  idv <- df_sorted[[ id ]]

  grp_start <- c( 1L, which( idv[-1L] != idv[-n] ) + 1L )
  grp_end   <- c( grp_start[-1L] - 1L, n )
  grp_n     <- grp_end - grp_start + 1L

  list(
    df          = df,
    df_sorted   = df_sorted,
    smooth_cols = smooth_cols,
    time        = time,
    id          = id,
    weights     = weights,
    window      = window,
    row_index   = "..row..",
    grp_start   = grp_start,
    grp_end     = grp_end,
    grp_n       = grp_n
  )
}

.panel_smooth_memory <- function(
    prep,
    verbose = TRUE
) {

  if ( length( prep$smooth_cols ) == 0L ) {
    warning( "No numeric columns matched `vars` - returning df unchanged." )
    return( prep$df )
  }

  df_sorted   <- prep$df_sorted
  smooth_cols <- prep$smooth_cols
  grp_start   <- prep$grp_start
  grp_end     <- prep$grp_end
  grp_n       <- prep$grp_n
  window      <- prep$window
  weights     <- prep$weights
  row_index   <- prep$row_index

  n_rows <- nrow( df_sorted )
  n_cols <- length( smooth_cols )
  n_orgs <- length( grp_start )

  if ( verbose ) {
    message(
      "Smoothing ",
      format( n_cols, big.mark = "," ), " column(s) across ",
      format( n_orgs, big.mark = "," ), " organization(s) and ",
      format( n_rows, big.mark = "," ), " row(s) ..."
    )
  }

  # Pull only once
  X <- as.matrix( df_sorted[ , smooth_cols, drop = FALSE ] )
  storage.mode( X ) <- "double"

  # Pass through singleton groups unchanged
  keep_groups <- grp_n > 1L

  if ( any( keep_groups ) ) {
    X[ , ] <- .smooth_matrix_by_groups(
      X         = X,
      grp_start = grp_start,
      grp_end   = grp_end,
      grp_n     = grp_n,
      window    = window,
      weights   = weights,
      verbose   = verbose
    )
  }

  X[ abs( X ) < 1e-12 ] <- 0

  df_sorted[ , smooth_cols ] <- X

  out <- df_sorted[ order( df_sorted[[ row_index ]] ), , drop = FALSE ]
  out[[ row_index ]] <- NULL
  rownames( out ) <- NULL

  if ( verbose ) {
    message( "Done." )
  }

  out
}

.smooth_matrix_by_groups <- function(
    X,
    grp_start,
    grp_end,
    grp_n,
    window,
    weights,
    verbose = TRUE
) {

  n_rows <- nrow( X )
  n_cols <- ncol( X )

  out <- X

  # cache window plans by unique group length
  unique_n <- sort( unique( grp_n[ grp_n > 1L ] ) )
  plan_cache <- vector( "list", length( unique_n ) )
  names( plan_cache ) <- as.character( unique_n )

  for ( g_len in unique_n ) {
    plan_cache[[ as.character( g_len ) ]] <- .build_window_plan(
      n       = g_len,
      window  = window,
      weights = weights
    )
  }

  n_groups <- length( grp_start )

  for ( g in seq_len( n_groups ) ) {

    s <- grp_start[[ g ]]
    e <- grp_end[[ g ]]
    n <- grp_n[[ g ]]

    if ( n <= 1L ) {
      next
    }

    plan <- plan_cache[[ as.character( n ) ]]

    block <- X[ s:e, , drop = FALSE ]
    out[ s:e, ] <- .smooth_group_block(
      block = block,
      plan  = plan
    )
  }

  out
}

.build_window_plan <- function(
    n,
    window,
    weights
) {

  w <- min( window, n )
  half <- floor( w / 2 )

  idx_list <- vector( "list", n )
  wt_list  <- vector( "list", n )

  for ( i in seq_len( n ) ) {

    s <- i - half
    e <- i + half

    if ( s < 1L ) {
      shift <- 1L - s
      s <- 1L
      e <- min( n, e + shift )
    }

    if ( e > n ) {
      shift <- e - n
      e <- n
      s <- max( 1L, s - shift )
    }

    idx <- seq.int( s, e )
    d   <- abs( idx - i )

    if ( weights == "equal" ) {
      rw <- rep( 1, length( idx ) )
    } else if ( weights == "half" ) {
      k  <- length( idx )
      rw <- ifelse( d == 0, 1, if ( k > 1L ) 1 / ( k - 1L ) else 0 )
    } else if ( weights == "decay" ) {
      rw <- 0.5 ^ d
    } else {
      stop( "Unknown weighting scheme." )
    }

    idx_list[[ i ]] <- idx
    wt_list[[ i ]]  <- rw
  }

  list(
    idx = idx_list,
    wt  = wt_list
  )
}

.smooth_group_block <- function(
    block,
    plan
) {

  n <- nrow( block )
  p <- ncol( block )

  out <- matrix(
    NA_real_,
    nrow = n,
    ncol = p,
    dimnames = dimnames( block )
  )

  is_nan <- is.nan( block )
  block_work <- block
  block_work[ is_nan ] <- NA_real_

  for ( j in seq_len( p ) ) {

    x <- block_work[ , j ]
    nan_j <- is_nan[ , j ]

    for ( i in seq_len( n ) ) {

      idx <- plan$idx[[ i ]]
      rw  <- plan$wt[[ i ]]
      vals <- x[ idx ]

      keep <- !is.na( vals )

      if ( any( keep ) ) {
        w_use <- rw[ keep ]
        w_use <- w_use / sum( w_use )
        out[ i, j ] <- sum( vals[ keep ] * w_use )
      } else {
        out[ i, j ] <- NA_real_
      }
    }

    out[ nan_j, j ] <- NaN
  }

  out
}

