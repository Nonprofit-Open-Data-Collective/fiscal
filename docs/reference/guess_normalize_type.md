# Guess the best normalization type from the stable interior of a distribution

Applies heuristics based on the support range, boundary mass, and zero
inflation to select among `"asinh"`, `"logit"`, and `"rank_normal"`.
Proportions with boundary mass above `boundary_mass_cutoff` (at zero or
one) get `"rank_normal"`; `"hurdle"` is never auto-selected and must be
requested through `vtype`.

## Usage

``` r
guess_normalize_type(
  x,
  range,
  boundary_mass_cutoff = 0.1,
  zero_tol = 1e-08,
  one_tol = 1e-08
)
```

## Arguments

- x:

  Numeric vector of **stable** (non-sentinel, non-NA) winsorized values.

- range:

  Character range code used to clip the vector to its theoretical
  support before assessment.

- boundary_mass_cutoff:

  Proportion of values at or near zero (or one) that triggers the
  `"rank_normal"` path. Default `0.10`.

- zero_tol:

  Tolerance for identifying values near zero. Default `1e-8`.

- one_tol:

  Tolerance for identifying values near one. Default `1e-8`.

## Value

A named list with elements `type` (character) and `reason` (character
explanation).
