# Impute missing panel years

Compatibility wrapper around
[`panel990::panel_impute()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_impute.html).

## Usage

``` r
panel_impute(
  df,
  panel_types = NULL,
  types = "persistent",
  max_gap_size = Inf,
  max_gap_count = Inf,
  vars = NULL,
  time = "TAX_YEAR",
  id = "EIN2",
  as_integers = FALSE
)
```

## Arguments

- df:

  Panel data frame.

- panel_types:

  Optional panel classification.

- types:

  Eligible panel boundary types (default `"persistent"`).

- max_gap_size:

  Maximum single gap length.

- max_gap_count:

  Maximum gaps per organization.

- vars:

  Numeric fields to fill.

- time:

  Panel-time column.

- id:

  Panel-ID column.

- as_integers:

  Round integer field imputations.

## Value

Panel data with inserted rows flagged by `imputed_row`.
