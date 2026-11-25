# Sanitize Financial Variables in an IRS 990 Dataset

Impute zero for NA values in IRS 990 financial fields, respecting the
distinction between fields available to all filers (990 + 990EZ) and
fields that only appear on the full 990 form.

## Usage

``` r
sanitize_financials(df, pz_vars = NULL, pc_vars = NULL)
```

## Arguments

- df:

  A `data.frame` containing IRS 990 efile financial fields. Should
  contain a `RETURN_TYPE` column (values `"990"` or `"990EZ"`) for
  accurate filer-type detection. If absent, filer type is inferred from
  field availability.

- pz_vars:

  Optional character vector of PZ-scope column names (present on both
  990 and 990EZ). When supplied, the local scope-aware imputer is used
  with exactly these fields. When `NULL` (default), scope is resolved
  from panel990 via
  [`panel990::panel_normalize()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_normalize.html).

- pc_vars:

  Optional character vector of PC-scope column names (full 990 only).
  When supplied, imputation is restricted to 990 filers for these
  fields. When `NULL` (default), scope is resolved from panel990.

## Value

A `data.frame` identical in structure to `df`, with NA values replaced
by zero in the applicable financial columns. The original `df` is not
modified.

## Details

In IRS 990 efile data, organizations often leave financial line items
blank when the value is zero rather than explicitly reporting zero.
These blanks are typically encoded as `NA` in processed datasets. For
ratio calculations this is problematic: a nonprofit with no investment
income genuinely has a zero in that field, not a missing value.

`sanitize_financials()` corrects this by imputing zero for NA values in
financial fields, subject to an important constraint: fields that only
exist on the full 990 form (PC scope; most of the Part VIII, IX, and X
detail, see
[`get_pc_fields()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_pc_fields.md))
should not be imputed to zero for 990EZ filers, since those fields
simply don't exist on the EZ form. Fields that appear on both forms (PZ
scope; the Part I summary plus lines with an EZ counterpart such as
total assets, see
[`get_pz_fields()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_pz_fields.md))
can be safely imputed for all filers. Rows in which every financial
field is `NA` are left untouched.

Filer type is determined from the `RETURN_TYPE` column when present
(`"990"` = full filer, `"990EZ"` = short-form filer). If `RETURN_TYPE`
is absent, filer type is inferred: rows with Part I data but missing
Part VIII data are treated as 990EZ filers.

As of the panel990 portage, the default path delegates to
[`panel990::panel_normalize()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_normalize.html),
which applies the same form-scoped, non-filer- protected zero imputation
over the shared concordance-derived field scopes. Supplying
`pz_vars`/`pc_vars` explicitly falls back to the local imputer for
backward compatibility.

Before imputing, bit64 `integer64` columns are converted to double.
Columns holding integer64 bytes with the class attribute dropped (every
nonzero value smaller than 1e-300 in absolute value) are repaired with a
warning; imputing them first would zero out negative amounts, which such
columns read as `NaN`.

## Examples

``` r
library( fiscal )
data( dat10k )

# Sanitize the full dataset before computing ratios
dat_clean <- sanitize_financials( dat10k )

# All ratios can then be computed without worrying about NA/zero ambiguity
dat_clean <- get_grants_govt_ratio( dat_clean )
#>    :: Total revenue equal to zero :: 64 case(s) replaced with NaN
dat_clean <- get_program_expenses_ratio( dat_clean )
#>    :: Total expenses equal to zero :: 206 case(s) replaced with NaN
```
