# Filter a panel by composition type and spell balance

Compatibility wrapper around
[`panel990::panel_filter()`](https://nonprofit-open-data-collective.github.io/panel990/reference/panel_filter.html).

## Usage

``` r
panel_filter_types(
  df,
  panel_types,
  keep = .PANEL_TYPES,
  spell_balance = .PANEL_SPELL_BALANCE,
  id = "EIN2"
)
```

## Arguments

- df:

  A panel data frame.

- panel_types:

  A per-ID classification, classification summary, or data with appended
  `panel_*` columns.

- keep:

  Panel types to retain: `"persistent"`, `"entrant"`, `"exit"`,
  `"transient"`, or `"empty"`.

- spell_balance:

  Spell values to retain: `"seamless"` and/or `"segmented"`.

- id:

  Name of the ID variable.

## Value

Filtered input rows.
