# Apply a fitted normalization to a numeric vector

Given a `normalize_x_fit` object produced by
[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md),
re-winsorizes `x` using the same settings, applies the fitted
transformation, and standardizes using the fitted center and scale.

Using the fitted ECDF / parameters rather than re-fitting ensures that
new observations (e.g. from a different year) are scored on the same
scale as the training data.

## Usage

``` r
apply_normalization(x, fit, verbose = FALSE)
```

## Arguments

- x:

  Numeric vector to score.

- fit:

  A `normalize_x_fit` object returned by
  [`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md).

- verbose:

  Logical. Print a brief summary. Default `FALSE`.

## Value

A numeric vector of the same length as `x` with transformation metadata
attached as attributes (`transform_type`, `transform_label`,
`transform_note`, `guess_reason`, `center`, `scale`, `range`,
`winsorize`, `offset`, `fit_object`).

## See also

[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md),
[`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md)

## Examples

``` r
x_train <- c( rnorm(200), rep(0, 30) )
x_new   <- rnorm(50)

# fit on training data, score new data with same parameters
fit  <- find_best_normalization( x_train, range = "np", verbose = FALSE )
z    <- apply_normalization( x_new, fit = fit )
```
