# Fit normalization parameters on the stable interior of a distribution

Winsorizes `x`, isolates the stable (non-sentinel, non-NA) interior,
selects a transformation type, fits its parameters and centering/scaling
constants, and returns a `normalize_x_fit` object that can be reused via
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md).

Separating fitting from scoring allows the same fitted model to be
applied to new data (e.g. a holdout year) without re-estimating
parameters, and ensures that sentinel observations do not distort the
center and spread estimates used for standardization. The exception is
`"rank_normal"`, whose reference distribution (and therefore its center
and spread) keeps the winsorized tail observations so that they are
scored at their true quantile.

## Usage

``` r
find_best_normalization(
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

  Numeric vector to fit on.

- range:

  Character range code (`"np"`, `"zp"`, `"zo"`, `"nz"`, or `"lo;hi"`).
  Required.

- vtype:

  Optional transformation type override. One of `NULL` (auto-detect),
  `"asinh"`, `"logit"`, `"rank_normal"`, `"hurdle"`.

- winsorize:

  Winsorization proportion. Default `0.98`.

- offset:

  Sentinel offset for fixed bounds. Default `0.001`.

- standardize:

  Logical. If `TRUE`, fit centering and scaling constants. Default
  `TRUE`.

- robust:

  Logical. If `TRUE`, use median/MAD; otherwise mean/SD. Default `TRUE`.

- zero_tol:

  Tolerance for zero detection. Default `1e-8`.

- one_tol:

  Tolerance for near-one detection. Default `1e-8`.

- boundary_mass_cutoff:

  Share of values at zero (or one) above which a \[0, 1\] variable is
  given `"rank_normal"` instead of `"logit"`. Default `0.10`.

- hurdle_trans:

  Positive-part transformation for hurdle variables. `"rank"` (default)
  or `"logit"`.

- hurdle_range:

  Rescaled output range for hurdle positive component. Default
  `c(1, 100)`.

- verbose:

  Logical. If `TRUE`, print a diagnostic summary. Default `TRUE`.

## Value

An object of class `normalize_x_fit` — a named list containing
transformation type, parameters, centering/scaling constants, and
diagnostics. Pass this object to
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md).

## See also

[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md),
[`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md)

## Examples

``` r
x <- c( rnorm(200), rep(0, 30) )

# fit on training data
fit <- find_best_normalization( x, range = "np", verbose = FALSE )
fit$transform_type
#> [1] "rank_normal"
fit$center
#> [1] -0.02179849
fit$scale
#> [1] 1.000373
```
