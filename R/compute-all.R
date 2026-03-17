###---------------------------------------------------
###   COMPUTE ALL FISCAL HEALTH METRICS
###---------------------------------------------------

#' Compute all fiscal health metrics in one call
#'
#' @description
#' Applies every \code{get_*()} ratio function in the \code{fiscal} package to
#' a data frame in a single call. The data frame is sanitized once up front
#' using \code{\link{sanitize_financials}}, then each function is called with
#' \code{sanitize = FALSE} to avoid redundant imputation passes.
#'
#' @param df A \code{data.frame} containing IRS 990 efile financial fields.
#'   Should include a \code{RETURN_TYPE} column for accurate 990 vs 990-EZ
#'   detection during sanitization.
#' @param metrics Character vector specifying which versions of each ratio to
#'   return. Any combination of \code{"ratio"} (raw value), \code{"w"}
#'   (winsorized), \code{"z"} (z-score), and \code{"p"} (percentile rank).
#'   Defaults to all four: \code{c("ratio","w","z","p")}.
#' @param append_to_df Logical. If \code{TRUE} (default), the selected metric
#'   columns are appended to the full original data frame. If \code{FALSE},
#'   returns only the \code{.IDVARS} identifier columns plus the selected
#'   metric columns.
#' @param winsorize The winsorization proportion passed to every ratio function
#'   (between 0 and 1, default \code{0.98}).
#' @param verbose Logical. If \code{TRUE} (default), prints a progress message
#'   as each ratio is computed.
#'
#' @return A \code{data.frame}. When \code{append_to_df = TRUE} (default),
#'   the original \code{df} with the selected metric columns appended.
#'   When \code{append_to_df = FALSE}, a slim data frame containing only the
#'   \code{.IDVARS} identifier columns and the selected metric columns.
#'   Column names follow the pattern \code{<abbr>} (raw), \code{<abbr>_w},
#'   \code{<abbr>_z}, \code{<abbr>_p}.
#'
#' @details
#' The sanitization step imputes zero for NA values in financial fields that
#' represent a genuinely missing zero (i.e., the organization did not report
#' the line because it was zero, not because the field is inapplicable).
#' See \code{\link{sanitize_financials}} for the rules governing which fields
#' are imputed for which filer types.
#'
#' Functions are called in a consistent order grouped by topic:
#' liquidity, leverage, revenue composition, expense ratios,
#' balance sheet structure, and performance ratios.
#'
#' Any function that errors (e.g., because required columns are absent from
#' \code{df}) is skipped with a warning rather than stopping the entire run,
#' so a partial dataset still returns the ratios that can be computed.
#'
#' @examples
#' library( fiscal )
#' data( dat10k )
#'
#' # Default: full df with all four metric variants appended
#' d <- compute_all( dat10k )
#' dim( d )
#'
#' # Return only raw ratios appended to the full df
#' d <- compute_all( dat10k, metrics = "ratio" )
#'
#' # Return ID columns + winsorized ratios only (slim output)
#' d <- compute_all( dat10k, metrics = "w", append_to_df = FALSE )
#' head( d )
#'
#' @export
compute_all <- function( df,
                         metrics       = c("ratio","w","z","p"),
                         append_to_df  = TRUE,
                         winsorize     = 0.98,
                         verbose       = TRUE ) {

  # ---- Validate metrics argument ----
  valid_metrics <- c("ratio","w","z","p")
  bad <- setdiff( metrics, valid_metrics )
  if ( length(bad) > 0 )
    stop( "Invalid metrics value(s): ", paste(bad, collapse=", "),
          ". Choose from: ", paste(valid_metrics, collapse=", ") )

  # ---- Sanitize once ----
  if (verbose) message( "Sanitizing financial fields ..." )
  df_sanitized <- sanitize_financials( df )

  # ---- Helper: call one get_* function safely ----
  # Always operates on df_sanitized; appends new ratio cols; returns updated df.
  .try_ratio <- function( fn, label, ... ) {
    if (verbose) message( "  Computing ", label, " ..." )
    tryCatch(
      fn( df_sanitized, ..., winsorize = winsorize, sanitize = FALSE ),
      error = function(e) {
        warning( "get_", label, "() skipped: ", conditionMessage(e), call. = FALSE )
        df_sanitized
      }
    )
  }

  # ---- Liquidity ----
  df_sanitized <- .try_ratio( get_cr,   "cr"   )
  df_sanitized <- .try_ratio( get_qr,   "qr"   )
  df_sanitized <- .try_ratio( get_casr, "casr" )
  df_sanitized <- .try_ratio( get_coh,  "coh"  )
  df_sanitized <- .try_ratio( get_brr,  "brr"  )
  df_sanitized <- .try_ratio( get_doch, "doch" )
  df_sanitized <- .try_ratio( get_moch, "moch" )

  # ---- Leverage & solvency ----
  df_sanitized <- .try_ratio( get_dar,  "dar"  )
  df_sanitized <- .try_ratio( get_der,  "der"  )
  df_sanitized <- .try_ratio( get_dmr,  "dmr"  )
  df_sanitized <- .try_ratio( get_doci, "doci" )
  df_sanitized <- .try_ratio( get_stdr, "stdr" )
  df_sanitized <- .try_ratio( get_lar,  "lar"  )

  # ---- Revenue composition ----
  df_sanitized <- .try_ratio( get_ssr,  "ssr"  )
  df_sanitized <- .try_ratio( get_ggr,  "ggr"  )
  df_sanitized <- .try_ratio( get_dgdr, "dgdr" )
  df_sanitized <- .try_ratio( get_eidr, "eidr" )
  df_sanitized <- .try_ratio( get_iidr, "iidr" )
  df_sanitized <- .try_ratio( get_arr,  "arr"  )

  # ---- Expense ratios ----
  df_sanitized <- .try_ratio( get_per,    "per"    )
  df_sanitized <- .try_ratio( get_aer,    "aer"    )
  df_sanitized <- .try_ratio( get_fer,    "fer"    )
  df_sanitized <- .try_ratio( get_podpm,  "podpm"  )
  df_sanitized <- .try_ratio( get_predpm, "predpm" )

  # ---- Balance sheet structure ----
  df_sanitized <- .try_ratio( get_er,   "er"   )
  df_sanitized <- .try_ratio( get_nacr, "nacr" )
  df_sanitized <- .try_ratio( get_orr,  "orr"  )
  df_sanitized <- .try_ratio( get_luna, "luna" )
  df_sanitized <- .try_ratio( get_or,   "or"   )

  # ---- Performance ----
  df_sanitized <- .try_ratio( get_sm,   "sm"   )
  df_sanitized <- .try_ratio( get_roa,  "roa"  )
  df_sanitized <- .try_ratio( get_rona, "rona" )

  # ---- Identify the ratio columns that were successfully added ----
  original_cols <- colnames( df )
  all_new_cols  <- setdiff( colnames( df_sanitized ), original_cols )

  # Each ratio produces up to four columns: abbr, abbr_w, abbr_z, abbr_p
  # Filter to only the variants the user requested
  suffix_pattern <- paste0(
    "(", 
    paste( c(
      if ( "ratio" %in% metrics ) "^[a-z]+$",        # bare name, no suffix
      if ( "w"     %in% metrics ) "_w$",
      if ( "z"     %in% metrics ) "_z$",
      if ( "p"     %in% metrics ) "_p$"
    ), collapse = "|" ),
    ")"
  )
  keep_cols <- all_new_cols[ grepl( suffix_pattern, all_new_cols ) ]

  if (verbose) message( "Done. ", length(keep_cols), " metric column(s) produced." )

  # ---- Assemble return value ----
  if ( append_to_df ) {
    result <- df                                        # original, unsanitized columns
    result[ , keep_cols ] <- dplyr::select( df_sanitized, dplyr::all_of( keep_cols ) )
    return( as.data.frame( result ) )
  } else {
    id_cols <- intersect( .IDVARS, colnames( df_sanitized ) )
    return( as.data.frame( dplyr::select( df_sanitized, dplyr::all_of( c( id_cols, keep_cols ) ) ) ) )
  }
}
