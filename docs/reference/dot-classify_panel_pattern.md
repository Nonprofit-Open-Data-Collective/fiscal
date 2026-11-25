# Classify one panel membership pattern

Internal helper used by
[`panel_composition()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_composition.md).

## Usage

``` r
.classify_panel_pattern(obs_years, panel_years)
```

## Arguments

- obs_years:

  Sorted unique years observed for one ID.

- panel_years:

  Sorted unique years in the full panel.

## Value

A list containing `panel_type` (`"persistent"`, `"entrant"`, `"exit"`,
`"transient"`, or `"empty"`), `spell_balance` (`"seamless"` or
`"segmented"`), and gap counts.
