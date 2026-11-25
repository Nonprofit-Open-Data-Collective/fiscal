# Coerce selected columns to numeric

Converts specified columns in a data frame to numeric, with a guard
against non-digit content. Stops if letters are detected in any column;
warns if silent coercion was needed.

## Usage

``` r
coerce_numeric(d, vars)
```

## Arguments

- d:

  A `data.frame`.

- vars:

  Character vector of column names to coerce.

## Value

The data frame with the specified columns coerced to numeric.

## Details

bit64 `integer64` columns are converted to double. Columns that hold
integer64 bytes with the class attribute dropped (every nonzero value is
smaller than 1e-300 in absolute value) are detected and repaired, with a
warning.
