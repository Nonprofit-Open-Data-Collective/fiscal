# Normalize a numeric vector for multivariate analysis

Convenience wrapper that calls
[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md)
and
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md)
in a single step. For repeated application of the same fitted model use
those functions directly.

The function winsorizes `x` according to `range` and `winsorize`,
isolates the stable interior (non-sentinel, non-NA values) to fit
transformation parameters, then scores all observations – including
sentinels – using those fitted parameters. This prevents boundary
pile-up at winsorization bounds from distorting the centering and spread
estimates (for `"rank_normal"` the reference distribution keeps the
winsorized tails; see below).

Four transformation types are supported:

- `"asinh"`: inverse hyperbolic sine, scaled by the median absolute
  nonzero value of the stable interior. Defined at zero, accommodates
  negatives, and behaves like log in the tails.

- `"logit"`: maps the stable interior to \[0, 1\] relative to its
  theoretical support, then applies
  [`qlogis()`](https://rdrr.io/r/stats/Logistic.html). Suited to
  variables bounded in \[0, 1\] without heavy boundary mass.

- `"rank_normal"`: rank-based inverse normal transform. Preserves
  ordering, not distances. Robust to irregular shapes and mixed support.

- `"hurdle"`: structural-zero mass receives 0; positive values are
  transformed separately (rank or logit) and rescaled to a positive
  interval.

When `vtype = NULL` the type is auto-detected from the stable interior
using heuristics based on the support range and boundary mass
(`"hurdle"` is only used when requested explicitly).

Scores stay bounded at the edges of the distribution so that they
preserve the rank correlations of the raw data:

- `"rank_normal"` uses mid-ranks, so a pile-up of tied values (e.g.
  exact zeros, or program-expense ratios of exactly one) is scored at
  the middle of its block, and winsorized tails are scored at their true
  quantile.

- `"logit"` clamps to the `(1 - winsorize) / 2` quantiles of the
  strictly interior values, so values at 0 or 1 are not sent to
  `+/- log(2n)`.

- When more than half the transformed values are tied, the MAD is zero
  and robust standardization falls back to mean/SD instead of returning
  `NA`.

## Usage

``` r
normalize_x(
  x,
  range,
  vtype = NULL,
  winsorize = 0.98,
  offset = 0.001,
  standardize = TRUE,
  robust = TRUE,
  zero_tol = 1e-08,
  one_tol = 1e-08,
  boundary_mass_cutoff = 0.1,
  hurdle_trans = c("rank", "logit"),
  hurdle_range = c(1, 100),
  verbose = TRUE
)
```

## Arguments

- x:

  A numeric vector.

- range:

  Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or `"lo;hi"`).
  Required.

- vtype:

  Optional transformation override. One of `NULL`, `"asinh"`, `"logit"`,
  `"rank_normal"`, `"hurdle"`.

- winsorize:

  Winsorization proportion. Default `0.98`.

- offset:

  Sentinel offset for fixed bounds. Default `0.001`.

- standardize:

  Logical. Standardize after transformation. Default `TRUE`.

- robust:

  Logical. Use median/MAD standardization. Default `TRUE`.

- zero_tol:

  Tolerance for zero detection. Default `1e-8`.

- one_tol:

  Tolerance for near-one detection. Default `1e-8`.

- boundary_mass_cutoff:

  Share of values at zero (or one) above which a \[0, 1\] variable is
  given `"rank_normal"` instead of `"logit"`. Default `0.10`.

- hurdle_trans:

  Positive-part method for hurdle variables (`"rank"` or `"logit"`).
  Default `"rank"`.

- hurdle_range:

  Rescaled range for hurdle positive component. Default `c(1, 100)`.

- verbose:

  Print diagnostic summaries. Default `TRUE`.

## Value

A numeric vector of the same length as `x`, with transformation metadata
attached as attributes (see
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md)).

## See also

[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md),
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md),
[`plot_normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/plot_normalize_x.md)

## Examples

``` r
x <- c( NA, rnorm(200), rep(0, 20), 50, -50 )

# one-step normalize with auto-detected transformation
z <- normalize_x( x, range = "np", verbose = FALSE )
hist( z, breaks = 30, main = "Normalized" )


# force a specific transformation
z_asinh <- normalize_x( x, range = "np", vtype = "asinh", verbose = FALSE )
```
