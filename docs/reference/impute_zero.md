# Impute zero for NA values in financial fields, respecting form scope

For a given set of column names and a data frame, imputes zero for NA
values in financial fields while respecting whether a field is available
to 990-EZ filers. PC-scope fields (990 only) are imputed only for rows
that are not 990-EZ filers. PZ-scope fields (990 + 990-EZ) are imputed
for all rows. Rows where every financial variable is NA are left
untouched.

## Usage

``` r
impute_zero(dat, vars, ez_rows)
```

## Arguments

- dat:

  A `data.frame` (the working subset, not the full dataset).

- vars:

  Character vector of financial column names to consider.

- ez_rows:

  Logical vector (length `nrow(dat)`) marking 990-EZ rows, as returned
  by
  [`detect_ez_rows()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/detect_ez_rows.md).

## Value

The modified `data.frame` with zeros imputed where appropriate.

## Examples

``` r
data( dat10k )
ez <- detect_ez_rows( dat10k )
dat_imp <- impute_zero( dat10k, vars = get_pc_fields(), ez_rows = ez )
```
