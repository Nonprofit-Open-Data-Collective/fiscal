###---------------------------------------------------
###   NORMALIZATION FUNCTIONS
###   (helpers, fit, apply, one-step wrapper, plot)
###---------------------------------------------------


# ---- internal helpers -----------------------------------------------

#' Parse a range specification string into a support list
#'
#' @param range Character string: `"np"`, `"zp"`, `"zo"`, `"nz"`, or `"lo;hi"`.
#' @param offset Numeric sentinel offset applied to fixed bounds. Default `0.001`.
#' @return A named list with elements `code`, `lo`, `hi`, `sentinel_lo`,
#'   `sentinel_hi`, and `offset`.
#' @keywords internal
parse_range_spec <- function( range, offset = 0.001 ) {

  range  <- trimws( tolower( range ) )
  custom <- grepl( "^-?[0-9.]+;-?[0-9.]+$", range )

  if( !range %in% c( "np", "zp", "zo", "nz" ) && !custom ) {
    stop(
      paste0(
        "`range` must be one of \"np\", \"zp\", \"zo\", \"nz\", or a custom ",
        "\"lo;hi\" pair such as \"0;10\". Got: \"", range, "\"."
      ),
      call. = FALSE
    )
  }

  if( custom ) {
    parts <- as.numeric( strsplit( range, ";", fixed = TRUE )[[1]] )
    lo    <- parts[1]
    hi    <- parts[2]
    return( list(
      code        = "custom",
      lo          = lo,
      hi          = hi,
      sentinel_lo = lo - offset,
      sentinel_hi = hi + offset,
      offset      = offset
    ) )
  }

  if( range == "zo" ) {
    return( list(
      code        = "zo",
      lo          = 0,
      hi          = 1,
      sentinel_lo = -offset,
      sentinel_hi = 1 + offset,
      offset      = offset
    ) )
  }

  if( range == "zp" ) {
    return( list(
      code        = "zp",
      lo          = 0,
      hi          = Inf,
      sentinel_lo = -offset,
      sentinel_hi = NA_real_,
      offset      = offset
    ) )
  }

  if( range == "nz" ) {
    return( list(
      code        = "nz",
      lo          = -Inf,
      hi          = 0,
      sentinel_lo = NA_real_,
      sentinel_hi = offset,
      offset      = offset
    ) )
  }

  if( range == "np" ) {
    return( list(
      code        = "np",
      lo          = -Inf,
      hi          = Inf,
      sentinel_lo = NA_real_,
      sentinel_hi = NA_real_,
      offset      = offset
    ) )
  }
}


#' Rescale a numeric vector to \[0, 1\] given explicit lo/hi
#' @keywords internal
rescale_01 <- function( x, lo, hi ) {
  out <- ( x - lo ) / ( hi - lo )
  out
}


#' Rescale a numeric vector to an arbitrary \[a, b\] range
#' @keywords internal
rescale_range <- function( x, to = c( 1, 100 ) ) {
  ok  <- !is.na( x )
  out <- rep( NA_real_, length( x ) )

  if( !any( ok ) ) return( out )

  xmin <- min( x[ok] )
  xmax <- max( x[ok] )

  if( isTRUE( all.equal( xmin, xmax ) ) ) {
    out[ok] <- mean( to )
    return( out )
  }

  out[ok] <- ( x[ok] - xmin ) / ( xmax - xmin ) * diff( to ) + to[1]
  out
}


#' Clip a vector to a support range (finite bounds only)
#' @keywords internal
clip_to_support <- function( x, support ) {

  y <- x

  if( is.finite( support$lo ) ) {
    y[!is.na( y ) & y < support$lo] <- support$lo
  }

  if( is.finite( support$hi ) ) {
    y[!is.na( y ) & y > support$hi] <- support$hi
  }

  y
}


#' Safe skewness (returns NA for n < 3, 0 for zero-variance)
#' @keywords internal
safe_skewness <- function( x ) {
  x <- x[is.finite( x )]
  if( length( x ) < 3 ) return( NA_real_ )
  s <- stats::sd( x )
  if( is.na( s ) || s == 0 ) return( 0 )
  mean( ((x - mean( x )) / s)^3 )
}


#' Safe excess kurtosis (returns NA for n < 4, 0 for zero-variance)
#' @keywords internal
safe_kurtosis <- function( x ) {
  x <- x[is.finite( x )]
  if( length( x ) < 4 ) return( NA_real_ )
  s <- stats::sd( x )
  if( is.na( s ) || s == 0 ) return( 0 )
  mean( ((x - mean( x )) / s)^4 ) - 3
}


#' Rank-based inverse normal transform (Blom approximation)
#' @keywords internal
rank_normal_stable <- function( x ) {
  ok  <- !is.na( x )
  out <- rep( NA_real_, length( x ) )
  n   <- sum( ok )
  if( n == 0 ) return( out )
  r       <- rank( x[ok], ties.method = "average" )
  p       <- ( r - 0.5 ) / n
  out[ok] <- stats::qnorm( p )
  out
}


# ---- winsorization ---------------------------------------------------

#' Compute the upper winsorization threshold for a given range specification
#'
#' @param x Numeric vector (NAs are ignored).
#' @param range Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or
#'   `"lo;hi"`).
#' @param winsorize Winsorization proportion (default `0.98`).
#' @param offset Sentinel offset for fixed bounds (default `0.001`).
#' @return A single numeric threshold value, or `NA` when no upper clipping
#'   applies.
#' @export
get_top_n <- function( x, range, winsorize = 0.98, offset = 0.001 ) {

  support <- parse_range_spec( range = range, offset = offset )
  x       <- x[!is.na( x )]

  if( length( x ) == 0 ) return( NA_real_ )

  if( support$code == "np" ) {
    top.p <- 1 - ( 1 - winsorize ) / 2
    top   <- as.numeric( stats::quantile( x, probs = top.p,
                                          na.rm = TRUE, type = 7 ) )
  } else if( support$code == "zp" ) {
    top <- as.numeric( stats::quantile( x, probs = winsorize,
                                        na.rm = TRUE, type = 7 ) )
  } else if( support$code == "zo" ) {
    top <- support$sentinel_hi
  } else if( support$code == "nz" ) {
    top <- support$sentinel_hi
  } else {
    top <- support$sentinel_hi
  }

  top
}


#' Compute the lower winsorization threshold for a given range specification
#'
#' @inheritParams get_top_n
#' @return A single numeric threshold value, or `NA` when no lower clipping
#'   applies.
#' @export
get_bottom_n <- function( x, range, winsorize = 0.98, offset = 0.001 ) {

  support <- parse_range_spec( range = range, offset = offset )
  x       <- x[!is.na( x )]

  if( length( x ) == 0 ) return( NA_real_ )

  if( support$code == "np" ) {
    bottom.p <- ( 1 - winsorize ) / 2
    bottom   <- as.numeric( stats::quantile( x, probs = bottom.p,
                                             na.rm = TRUE, type = 7 ) )
  } else if( support$code == "zp" ) {
    bottom <- support$sentinel_lo
  } else if( support$code == "zo" ) {
    bottom <- support$sentinel_lo
  } else if( support$code == "nz" ) {
    bottom <- as.numeric( stats::quantile( x, probs = 1 - winsorize,
                                           na.rm = TRUE, type = 7 ) )
  } else {
    bottom <- support$sentinel_lo
  }

  bottom
}


#' Winsorize a numeric vector according to a range specification
#'
#' @description
#' Clips `x` to `[bottom, top]` bounds determined by `range` and `winsorize`,
#' flags which observations were reassigned to a sentinel bound, and returns
#' a list suitable for downstream normalization.
#'
#' @inheritParams get_top_n
#' @return A named list with elements:
#'   - `x_w` — winsorized vector
#'   - `top`, `bottom` — the computed bounds
#'   - `is_top_sentinel`, `is_bottom_sentinel`, `is_sentinel` — logical vectors
#'   - `support` — parsed range specification list
#'   - `winsorize`, `offset` — echoed inputs
#' @examples
#' x <- c( NA, rnorm(200), 100, -100 )
#'
#' # winsorize with symmetric np bounds
#' w <- winsorize_x( x, range = "np", winsorize = 0.98 )
#' range( w$x_w, na.rm = TRUE )
#' sum( w$is_sentinel )
#' @export
winsorize_x <- function( x, range, winsorize = 0.98, offset = 0.001 ) {

  support <- parse_range_spec( range = range, offset = offset )
  top     <- get_top_n(    x = x, range = range, winsorize = winsorize, offset = offset )
  bottom  <- get_bottom_n( x = x, range = range, winsorize = winsorize, offset = offset )

  x.w <- x

  if( !is.na( top ) ) {
    x.w[!is.na( x.w ) & x.w > top] <- top
  }

  if( !is.na( bottom ) ) {
    x.w[!is.na( x.w ) & x.w < bottom] <- bottom
  }

  is_top_sentinel    <- rep( FALSE, length( x.w ) )
  is_bottom_sentinel <- rep( FALSE, length( x.w ) )

  if( !is.na( top ) ) {
    is_top_sentinel <- !is.na( x.w ) & x.w == top
  }

  if( !is.na( bottom ) ) {
    is_bottom_sentinel <- !is.na( x.w ) & x.w == bottom
  }

  is_sentinel <- is_top_sentinel | is_bottom_sentinel

  list(
    x_w                = x.w,
    top                = top,
    bottom             = bottom,
    is_top_sentinel    = is_top_sentinel,
    is_bottom_sentinel = is_bottom_sentinel,
    is_sentinel        = is_sentinel,
    support            = support,
    winsorize          = winsorize,
    offset             = offset
  )
}


# ---- type guessing ---------------------------------------------------

#' Guess the best normalization type from the stable interior of a distribution
#'
#' @description
#' Applies heuristics based on the support range, boundary mass, and zero
#' inflation to select among `"asinh"`, `"logit"`, `"rank_normal"`, and
#' `"hurdle"`.
#'
#' @param x Numeric vector of **stable** (non-sentinel, non-NA) winsorized
#'   values.
#' @param range Character range code used to clip the vector to its theoretical
#'   support before assessment.
#' @param boundary_mass_cutoff Proportion of values at or near zero (or one)
#'   that triggers the `"hurdle"` or `"rank_normal"` path. Default `0.10`.
#' @param zero_tol Tolerance for identifying values near zero. Default `1e-8`.
#' @param one_tol Tolerance for identifying values near one. Default `1e-8`.
#' @return A named list with elements `type` (character) and `reason`
#'   (character explanation).
#' @export
guess_normalize_type <- function(
    x,
    range,
    boundary_mass_cutoff = 0.10,
    zero_tol             = 1e-8,
    one_tol              = 1e-8
) {

  support <- parse_range_spec( range )
  x       <- x[!is.na( x )]

  if( length( x ) == 0 ) {
    return( list(
      type   = "rank_normal",
      reason = "No non-missing values available; defaulting to rank_normal."
    ) )
  }

  x.clipped <- clip_to_support( x, support )

  xmin       <- min( x.clipped )
  xmax       <- max( x.clipped )
  prop_zero  <- mean( abs( x.clipped ) <= zero_tol )
  prop_one   <- if( is.finite( support$hi ) && support$hi == 1 ) {
    mean( abs( x.clipped - 1 ) <= one_tol )
  } else {
    0
  }
  prop_in_01 <- mean( x.clipped >= 0 & x.clipped <= 1 )

  if( support$code == "zo" || prop_in_01 > 0.98 ) {
    if( prop_zero > boundary_mass_cutoff ) {
      return( list(
        type   = "hurdle",
        reason = paste0(
          "Variable is effectively in [0,1] with substantial zero mass (p0=",
          round( prop_zero, 3 ), ")."
        )
      ) )
    } else if( prop_one > boundary_mass_cutoff ) {
      return( list(
        type   = "rank_normal",
        reason = paste0(
          "Variable is effectively in [0,1] with strong upper boundary mass (p1=",
          round( prop_one, 3 ), ")."
        )
      ) )
    } else {
      return( list(
        type   = "logit",
        reason = "Variable is effectively in [0,1] with limited boundary pile-up."
      ) )
    }
  }

  if( support$code == "zp" || xmin >= 0 ) {
    return( list(
      type   = "asinh",
      reason = "Variable is nonnegative and not primarily bounded in [0,1]."
    ) )
  }

  list(
    type   = "rank_normal",
    reason = "Variable has mixed support; using rank-based inverse normal transform."
  )
}


# ---- fit step --------------------------------------------------------

#' Fit normalization parameters on the stable interior of a distribution
#'
#' @description
#' Winsorizes `x`, isolates the stable (non-sentinel, non-NA) interior,
#' selects a transformation type, fits its parameters and centering/scaling
#' constants, and returns a `normalize_x_fit` object that can be reused via
#' [apply_normalization()].
#'
#' Separating fitting from scoring allows the same fitted model to be applied
#' to new data (e.g. a holdout year) without re-estimating parameters, and
#' ensures that sentinel observations do not distort the center and spread
#' estimates used for standardization.
#'
#' @param x Numeric vector to fit on.
#' @param range Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or
#'   `"lo;hi"`). Required.
#' @param vtype Optional transformation type override. One of `NULL`
#'   (auto-detect), `"asinh"`, `"logit"`, `"rank_normal"`, `"hurdle"`.
#' @param winsorize Winsorization proportion. Default `0.98`.
#' @param offset Sentinel offset for fixed bounds. Default `0.001`.
#' @param standardize Logical. If `TRUE`, fit centering and scaling constants.
#'   Default `TRUE`.
#' @param robust Logical. If `TRUE`, use median/MAD; otherwise mean/SD.
#'   Default `TRUE`.
#' @param zero_tol Tolerance for zero detection. Default `1e-8`.
#' @param one_tol Tolerance for near-one detection. Default `1e-8`.
#' @param boundary_mass_cutoff Hurdle/rank_normal trigger threshold. Default
#'   `0.10`.
#' @param hurdle_trans Positive-part transformation for hurdle variables.
#'   `"rank"` (default) or `"logit"`.
#' @param hurdle_range Rescaled output range for hurdle positive component.
#'   Default `c(1, 100)`.
#' @param verbose Logical. If `TRUE`, print a diagnostic summary. Default
#'   `TRUE`.
#'
#' @return An object of class `normalize_x_fit` — a named list containing
#'   transformation type, parameters, centering/scaling constants, and
#'   diagnostics. Pass this object to [apply_normalization()].
#'
#' @seealso [apply_normalization()], [normalize_x()]
#' @examples
#' x <- c( rnorm(200), rep(0, 30) )
#'
#' # fit on training data
#' fit <- find_best_normalization( x, range = "np", verbose = FALSE )
#' fit$transform_type
#' fit$center
#' fit$scale
#' @export
find_best_normalization <- function(
    x,
    range,
    vtype                = NULL,
    winsorize            = 0.98,
    offset               = 0.001,
    standardize          = TRUE,
    robust               = TRUE,
    zero_tol             = 1e-8,
    one_tol              = 1e-8,
    boundary_mass_cutoff = 0.10,
    hurdle_trans         = c( "rank", "logit" ),
    hurdle_range         = c( 1, 100 ),
    verbose              = TRUE
) {

  hurdle_trans <- match.arg( hurdle_trans )

  w   <- winsorize_x( x = x, range = range, winsorize = winsorize, offset = offset )
  x.w <- w$x_w

  # stable fitting vector: drop NAs and sentinel-coded winsor values
  x.clean <- x.w[!is.na( x.w ) & !w$is_sentinel]

  if( length( x.clean ) < 3 ) {
    stop(
      "Fewer than 3 non-sentinel, non-missing observations available for fitting.",
      call. = FALSE
    )
  }

  support         <- w$support
  x.clean.support <- clip_to_support( x.clean, support )

  if( is.null( vtype ) ) {
    guess      <- guess_normalize_type(
      x                    = x.clean.support,
      range                = range,
      boundary_mass_cutoff = boundary_mass_cutoff,
      zero_tol             = zero_tol,
      one_tol              = one_tol
    )
    fit.type   <- guess$type
    fit.reason <- guess$reason
  } else {
    fit.type   <- match.arg( vtype, c( "asinh", "logit", "rank_normal", "hurdle" ) )
    fit.reason <- "User-specified transformation."
  }

  fit <- list()
  fit$range                <- range
  fit$support              <- support
  fit$winsorize            <- winsorize
  fit$offset               <- offset
  fit$standardize          <- standardize
  fit$robust               <- robust
  fit$transform_type       <- fit.type
  fit$guess_reason         <- fit.reason
  fit$zero_tol             <- zero_tol
  fit$one_tol              <- one_tol
  fit$boundary_mass_cutoff <- boundary_mass_cutoff
  fit$hurdle_trans         <- hurdle_trans
  fit$hurdle_range         <- hurdle_range
  fit$n_clean              <- length( x.clean.support )
  fit$raw_skewness         <- safe_skewness( x.clean.support )
  fit$raw_kurtosis         <- safe_kurtosis( x.clean.support )

  # fit transform-specific parameters on stable vector
  if( fit.type == "asinh" ) {

    x.nz <- x.clean.support[abs( x.clean.support ) > zero_tol]
    s    <- stats::median( abs( x.nz ), na.rm = TRUE )
    if( is.na( s ) || s == 0 ) s <- 1

    x.t <- asinh( x.clean.support / s )

    fit$transform_label <- "Asinh transformation."
    fit$transform_note  <- paste0( "Applied asinh(x / s) with s = ", signif( s, 5 ), "." )
    fit$params          <- list( scale_const = s )
  }

  if( fit.type == "logit" ) {

    lo <- if( is.finite( support$lo ) ) support$lo else min( x.clean.support, na.rm = TRUE )
    hi <- if( is.finite( support$hi ) ) support$hi else max( x.clean.support, na.rm = TRUE )

    if( !is.finite( lo ) || !is.finite( hi ) || lo >= hi ) {
      stop( "Invalid support for logit transformation.", call. = FALSE )
    }

    p   <- rescale_01( x.clean.support, lo = lo, hi = hi )
    eps <- 0.5 / length( p )
    p   <- pmin( pmax( p, eps ), 1 - eps )
    x.t <- stats::qlogis( p )

    fit$transform_label <- "Logit transformation."
    fit$transform_note  <- paste0(
      "Applied qlogis() after rescaling to [0,1] with eps = ", signif( eps, 5 ), "."
    )
    fit$params <- list( lo = lo, hi = hi, eps = eps )
  }

  if( fit.type == "rank_normal" ) {

    x.t <- rank_normal_stable( x.clean.support )

    fit$transform_label <- "Rank-based inverse normal transformation."
    fit$transform_note  <- "Applied rank-based inverse normal transformation on stable values."
    fit$params          <- list(
      x_sorted = sort( x.clean.support ),
      n        = length( x.clean.support )
    )
  }

  if( fit.type == "hurdle" ) {

    x.pos <- x.clean.support[x.clean.support > 0]

    if( length( x.pos ) == 0 ) {
      x.t <- ifelse( x.clean.support == 0, 0, NA_real_ )
      fit$transform_label <- "Hurdle transformation."
      fit$transform_note  <- "No positive stable values detected; all stable values are zero."
      fit$params <- list(
        positive_method = hurdle_trans,
        out_range       = hurdle_range,
        pos_sorted      = numeric( 0 ),
        pos_n           = 0
      )
    } else {

      if( hurdle_trans == "rank" ) {
        tp         <- rank_normal_stable( x.pos )
        pos_params <- list(
          positive_method = "rank",
          pos_sorted      = sort( x.pos ),
          pos_n           = length( x.pos ),
          out_range       = hurdle_range
        )
      } else {
        lo  <- if( is.finite( support$lo ) ) support$lo else min( x.pos, na.rm = TRUE )
        hi  <- if( is.finite( support$hi ) ) support$hi else max( x.pos, na.rm = TRUE )
        p   <- rescale_01( x.pos, lo = lo, hi = hi )
        eps <- 0.5 / length( p )
        p   <- pmin( pmax( p, eps ), 1 - eps )
        tp  <- stats::qlogis( p )
        pos_params <- list(
          positive_method = "logit",
          lo              = lo,
          hi              = hi,
          eps             = eps,
          out_range       = hurdle_range
        )
      }

      tp  <- rescale_range( tp, to = hurdle_range )
      x.t <- ifelse( x.clean.support == 0, 0, NA_real_ )
      x.t[x.clean.support > 0] <- tp

      fit$transform_label <- paste0(
        "Hurdle transformation using ", hurdle_trans, " positive component."
      )
      fit$transform_note <- paste0(
        "Zeros retained as 0; positive values transformed using ", hurdle_trans,
        " and rescaled to [", hurdle_range[1], ", ", hurdle_range[2], "]."
      )
      fit$params <- pos_params
    }
  }

  # fit centering/scaling on transformed stable vector
  if( standardize ) {
    if( robust ) {
      center              <- stats::median( x.t, na.rm = TRUE )
      scale               <- stats::mad( x.t, na.rm = TRUE, constant = 1.4826 )
      fit$standardization <- "robust"
    } else {
      center              <- mean( x.t, na.rm = TRUE )
      scale               <- stats::sd( x.t, na.rm = TRUE )
      fit$standardization <- "classical"
    }
    if( is.na( scale ) || scale == 0 ) scale <- NA_real_
  } else {
    center              <- NA_real_
    scale               <- NA_real_
    fit$standardization <- "none"
  }

  fit$center       <- center
  fit$scale        <- scale

  # quality diagnostics on stable transformed vector
  fit$fit_skewness <- safe_skewness( x.t )
  fit$fit_kurtosis <- safe_kurtosis( x.t )

  class( fit ) <- "normalize_x_fit"

  if( verbose ) {
    cat( "\n--- find_best_normalization() ---\n" )
    cat( "Transform type: ", fit$transform_type,           "\n", sep = "" )
    cat( "Reason:         ", fit$guess_reason,             "\n", sep = "" )
    cat( "N clean:        ", fit$n_clean,                  "\n", sep = "" )
    cat( "Raw skewness:   ", round( fit$raw_skewness, 4 ), "\n", sep = "" )
    cat( "Raw kurtosis:   ", round( fit$raw_kurtosis, 4 ), "\n", sep = "" )
    cat( "Fit skewness:   ", round( fit$fit_skewness, 4 ), "\n", sep = "" )
    cat( "Fit kurtosis:   ", round( fit$fit_kurtosis, 4 ), "\n", sep = "" )
    cat( "Center:         ", signif( fit$center, 5 ),      "\n", sep = "" )
    cat( "Scale:          ", signif( fit$scale,  5 ),      "\n", sep = "" )
    cat( "-------------------------------\n\n" )
  }

  fit
}


# ---- apply step ------------------------------------------------------

#' Apply a fitted normalization to a numeric vector
#'
#' @description
#' Given a `normalize_x_fit` object produced by [find_best_normalization()],
#' re-winsorizes `x` using the same settings, applies the fitted
#' transformation, and standardizes using the fitted center and scale.
#'
#' Using the fitted ECDF / parameters rather than re-fitting ensures that new
#' observations (e.g. from a different year) are scored on the same scale as
#' the training data.
#'
#' @param x Numeric vector to score.
#' @param fit A `normalize_x_fit` object returned by [find_best_normalization()].
#' @param verbose Logical. Print a brief summary. Default `FALSE`.
#'
#' @return A numeric vector of the same length as `x` with transformation
#'   metadata attached as attributes (`transform_type`, `transform_label`,
#'   `transform_note`, `guess_reason`, `center`, `scale`, `range`,
#'   `winsorize`, `offset`, `fit_object`).
#'
#' @seealso [find_best_normalization()], [normalize_x()]
#' @examples
#' x_train <- c( rnorm(200), rep(0, 30) )
#' x_new   <- rnorm(50)
#'
#' # fit on training data, score new data with same parameters
#' fit  <- find_best_normalization( x_train, range = "np", verbose = FALSE )
#' z    <- apply_normalization( x_new, fit = fit )
#' @export
apply_normalization <- function( x, fit, verbose = FALSE ) {

  if( !inherits( fit, "normalize_x_fit" ) ) {
    stop( "`fit` must be an object returned by find_best_normalization().",
          call. = FALSE )
  }

  # re-winsorize original x using fitted conventions
  w   <- winsorize_x( x = x, range = fit$range,
                      winsorize = fit$winsorize, offset = fit$offset )
  x.w <- w$x_w
  x.a <- clip_to_support( x.w, fit$support )

  x.t <- rep( NA_real_, length( x.a ) )

  if( fit$transform_type == "asinh" ) {
    s   <- fit$params$scale_const
    x.t <- asinh( x.a / s )
  }

  if( fit$transform_type == "logit" ) {
    lo  <- fit$params$lo
    hi  <- fit$params$hi
    eps <- fit$params$eps
    p   <- rescale_01( x.a, lo = lo, hi = hi )
    p   <- pmin( pmax( p, eps ), 1 - eps )
    x.t <- stats::qlogis( p )
  }

  if( fit$transform_type == "rank_normal" ) {
    x.sorted <- fit$params$x_sorted
    n        <- fit$params$n
    Fn       <- stats::ecdf( x.sorted )
    p        <- Fn( x.a )
    p        <- pmin( pmax( p, 0.5 / n ), 1 - 0.5 / n )
    x.t[!is.na( x.a )] <- stats::qnorm( p[!is.na( x.a )] )
  }

  if( fit$transform_type == "hurdle" ) {

    x.t[!is.na( x.a ) & x.a == 0] <- 0
    pos <- !is.na( x.a ) & x.a > 0

    if( any( pos ) ) {
      if( fit$params$positive_method == "rank" ) {
        pos.sorted <- fit$params$pos_sorted
        pos.n      <- fit$params$pos_n
        if( pos.n > 0 ) {
          Fn.pos <- stats::ecdf( pos.sorted )
          pp     <- Fn.pos( x.a[pos] )
          pp     <- pmin( pmax( pp, 0.5 / pos.n ), 1 - 0.5 / pos.n )
          tp     <- stats::qnorm( pp )
          tp     <- rescale_range( tp, to = fit$params$out_range )
          x.t[pos] <- tp
        }
      }

      if( fit$params$positive_method == "logit" ) {
        lo  <- fit$params$lo
        hi  <- fit$params$hi
        eps <- fit$params$eps
        pp  <- rescale_01( x.a[pos], lo = lo, hi = hi )
        pp  <- pmin( pmax( pp, eps ), 1 - eps )
        tp  <- stats::qlogis( pp )
        tp  <- rescale_range( tp, to = fit$params$out_range )
        x.t[pos] <- tp
      }
    }
  }

  # standardize using fitted center / scale
  if( fit$standardize ) {
    if( is.na( fit$scale ) || fit$scale == 0 ) {
      warning(
        "Scale from fitted normalization is zero or NA; standardized values set to NA.",
        call. = FALSE
      )
      x.z <- rep( NA_real_, length( x.t ) )
    } else {
      x.z <- ( x.t - fit$center ) / fit$scale
    }
  } else {
    x.z <- x.t
  }

  attr( x.z, "transform_type"  ) <- fit$transform_type
  attr( x.z, "transform_label" ) <- fit$transform_label
  attr( x.z, "transform_note"  ) <- fit$transform_note
  attr( x.z, "guess_reason"    ) <- fit$guess_reason
  attr( x.z, "center"          ) <- fit$center
  attr( x.z, "scale"           ) <- fit$scale
  attr( x.z, "range"           ) <- fit$range
  attr( x.z, "winsorize"       ) <- fit$winsorize
  attr( x.z, "offset"          ) <- fit$offset
  attr( x.z, "fit_object"      ) <- fit

  if( verbose ) {
    cat( "\n--- apply_normalization() ---\n" )
    cat( "Transform type: ", fit$transform_type, "\n", sep = "" )
    cat( "Range:          ", fit$range,          "\n", sep = "" )
    cat( "Winsorize:      ", fit$winsorize,       "\n", sep = "" )
    cat( "Standardize:    ", fit$standardize,     "\n", sep = "" )
    cat( "----------------------------\n\n" )
  }

  x.z
}


# ---- one-step wrapper ------------------------------------------------

#' Normalize a numeric vector for multivariate analysis
#'
#' @description
#' Convenience wrapper that calls [find_best_normalization()] and
#' [apply_normalization()] in a single step. For repeated application of
#' the same fitted model use those functions directly.
#'
#' The function winsorizes `x` according to `range` and `winsorize`, isolates
#' the stable interior (non-sentinel, non-NA values) to fit transformation
#' parameters, then scores all observations -- including sentinels -- using
#' those fitted parameters. This prevents boundary pile-up at winsorization
#' bounds from distorting the centering and spread estimates.
#'
#' Four transformation types are supported:
#'
#' - `"asinh"`: inverse hyperbolic sine, scaled by the median absolute
#'   nonzero value of the stable interior. Defined at zero, accommodates
#'   negatives, and behaves like log in the tails.
#' - `"logit"`: maps the stable interior to \[0, 1\] relative to its
#'   theoretical support, then applies `qlogis()`. Suited to variables bounded
#'   in \[0, 1\] without heavy boundary mass.
#' - `"rank_normal"`: rank-based inverse normal transform. Preserves ordering,
#'   not distances. Robust to irregular shapes and mixed support.
#' - `"hurdle"`: structural-zero mass receives 0; positive values are
#'   transformed separately (rank or logit) and rescaled to a positive interval.
#'
#' When `vtype = NULL` the type is auto-detected from the stable interior using
#' heuristics based on the support range and boundary mass.
#'
#' @param x A numeric vector.
#' @param range Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or
#'   `"lo;hi"`). Required.
#' @param vtype Optional transformation override. One of `NULL`, `"asinh"`,
#'   `"logit"`, `"rank_normal"`, `"hurdle"`.
#' @param winsorize Winsorization proportion. Default `0.98`.
#' @param offset Sentinel offset for fixed bounds. Default `0.001`.
#' @param standardize Logical. Standardize after transformation. Default `TRUE`.
#' @param robust Logical. Use median/MAD standardization. Default `TRUE`.
#' @param zero_tol Tolerance for zero detection. Default `1e-8`.
#' @param one_tol Tolerance for near-one detection. Default `1e-8`.
#' @param boundary_mass_cutoff Hurdle/rank_normal trigger threshold. Default
#'   `0.10`.
#' @param hurdle_trans Positive-part method for hurdle variables (`"rank"` or
#'   `"logit"`). Default `"rank"`.
#' @param hurdle_range Rescaled range for hurdle positive component. Default
#'   `c(1, 100)`.
#' @param verbose Print diagnostic summaries. Default `TRUE`.
#'
#' @return A numeric vector of the same length as `x`, with transformation
#'   metadata attached as attributes (see [apply_normalization()]).
#'
#' @seealso [find_best_normalization()], [apply_normalization()],
#'   [plot_normalize_x()]
#' @examples
#' x <- c( NA, rnorm(200), rep(0, 20), 50, -50 )
#'
#' # one-step normalize with auto-detected transformation
#' z <- normalize_x( x, range = "np", verbose = FALSE )
#' hist( z, breaks = 30, main = "Normalized" )
#'
#' # force a specific transformation
#' z_asinh <- normalize_x( x, range = "np", vtype = "asinh", verbose = FALSE )
#' @export
normalize_x <- function(
    x,
    range,
    vtype                = NULL,
    winsorize            = 0.98,
    offset               = 0.001,
    standardize          = TRUE,
    robust               = TRUE,
    zero_tol             = 1e-8,
    one_tol              = 1e-8,
    boundary_mass_cutoff = 0.10,
    hurdle_trans         = c( "rank", "logit" ),
    hurdle_range         = c( 1, 100 ),
    verbose              = TRUE
) {

  fit <- find_best_normalization(
    x                    = x,
    range                = range,
    vtype                = vtype,
    winsorize            = winsorize,
    offset               = offset,
    standardize          = standardize,
    robust               = robust,
    zero_tol             = zero_tol,
    one_tol              = one_tol,
    boundary_mass_cutoff = boundary_mass_cutoff,
    hurdle_trans         = hurdle_trans,
    hurdle_range         = hurdle_range,
    verbose              = verbose
  )

  apply_normalization( x = x, fit = fit, verbose = FALSE )
}


# ---- diagnostic plot -------------------------------------------------

#' Plot raw and normalized versions of a numeric vector
#'
#' @description
#' Diagnostic companion to [normalize_x()]. Plots side-by-side histograms
#' of the raw and normalized distributions.
#'
#' @param x A numeric vector (raw input, or already-normalized if
#'   `normalize = FALSE`).
#' @param range Character range code passed to [normalize_x()] when
#'   `normalize = TRUE`. Required unless `normalize = FALSE`.
#' @param normalize Logical. If `TRUE` (default), normalize `x` inside the
#'   function. If `FALSE`, treat `x` as already normalized.
#' @param breaks Number of histogram breaks. Default `40`.
#' @param col_raw Fill color for the raw histogram. Default `"steelblue"`.
#' @param col_norm Fill color for the normalized histogram. Default
#'   `"firebrick"`.
#' @param border Bar border color. Default `"white"`.
#' @param main_raw Title for the raw panel. Default `"Raw"`.
#' @param main_norm Optional title for the normalized panel. If `NULL`, uses
#'   the `transform_label` attribute when available.
#' @param ... Additional arguments passed to [normalize_x()] when
#'   `normalize = TRUE`.
#'
#' @return Invisibly returns a list with elements `raw` and `normalized`.
#'
#' @seealso [normalize_x()], [find_best_normalization()]
#' @export
plot_normalize_x <- function(
    x,
    range     = NULL,
    normalize = TRUE,
    breaks    = 40,
    col_raw   = "steelblue",
    col_norm  = "firebrick",
    border    = "white",
    main_raw  = "Raw",
    main_norm = NULL,
    ...
) {
  raw_x <- x

  if( normalize ) {
    if( is.null( range ) )
      stop( "`range` is required when `normalize = TRUE`.", call. = FALSE )
    norm_x <- normalize_x( x, range = range, verbose = FALSE, ... )
  } else {
    norm_x <- x
  }

  transform_label <- attr( norm_x, "transform_label" )
  if( is.null( main_norm ) )
    main_norm <- if( !is.null( transform_label ) ) transform_label else "Normalized"

  op <- graphics::par( mfrow = c( 1, 2 ) )
  on.exit( graphics::par( op ), add = TRUE )

  graphics::hist( raw_x,  breaks = breaks, col = col_raw,  border = border,
                  main = main_raw,  xlab = "x",            yaxt = "n" )
  graphics::hist( norm_x, breaks = breaks, col = col_norm, border = border,
                  main = main_norm, xlab = "normalized x", yaxt = "n" )

  invisible( list( raw = raw_x, normalized = norm_x ) )
}
