# Validate numerator and denominator inputs for get\_\* functions

Checks that `winsorize` is in `[0, 1]` and that numerator and
denominator arguments are not in an inconsistent NULL state.

## Usage

``` r
validate_inputs(
  winsorize,
  num_args,
  den_args,
  num_name = "numerator",
  den_name = "denominator"
)
```

## Arguments

- winsorize:

  Numeric scalar.

- num_args:

  Numerator argument value (may be `NULL`).

- den_args:

  Denominator argument value (may be `NULL`).

- num_name:

  Name of numerator argument used in error messages.

- den_name:

  Name of denominator argument used in error messages.

## Value

Invisibly `NULL`; called for its side-effect of stopping on invalid
input.
