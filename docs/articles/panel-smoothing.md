# Panel smoothing

[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md)
applies a rolling window average within each organization’s time series
to reduce year-to-year noise in financial variables. It returns a data
frame with **identical dimensions and column names** — only the selected
numeric columns are replaced with smoothed values — so the smoothed
panel drops straight into
[`compute_all_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all_panel.md)
or any downstream step.

This article explains the window logic, the three weighting schemes, and
how missing values are handled. For where smoothing sits in the larger
retrieve → clean → score pipeline, see
[`vignette("panel-workflow")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/panel-workflow.md).

## Demo data

A tiny three-organization, four-year panel:

``` r

df <- data.frame(
  id   = rep(c("A", "B", "C"), each = 4),
  x    = 1:12,
  y    = c(-0.200, -0.864, -0.437, -0.975,
           -0.898,  0.223, -0.093,  1.588,
            0.527,  0.994, -0.139, -0.389),
  z    = c(0, 0, 1, 1, -1, 0, 1, 0, 0, 1, 1, 1),
  year = rep(2021:2024, times = 3)
)

knitr::kable(df)
```

| id  |   x |      y |   z | year |
|:----|----:|-------:|----:|-----:|
| A   |   1 | -0.200 |   0 | 2021 |
| A   |   2 | -0.864 |   0 | 2022 |
| A   |   3 | -0.437 |   1 | 2023 |
| A   |   4 | -0.975 |   1 | 2024 |
| B   |   5 | -0.898 |  -1 | 2021 |
| B   |   6 |  0.223 |   0 | 2022 |
| B   |   7 | -0.093 |   1 | 2023 |
| B   |   8 |  1.588 |   0 | 2024 |
| C   |   9 |  0.527 |   0 | 2021 |
| C   |  10 |  0.994 |   1 | 2022 |
| C   |  11 | -0.139 |   1 | 2023 |
| C   |  12 | -0.389 |   1 | 2024 |

## Conceptual overview

For a given window size `N` (which must be odd):

- **Beginning of the panel** — use the first `N` observations.
- **Middle of the panel** — use a symmetric window centered on the
  observation, including `(N - 1) / 2` lags and `(N - 1) / 2` leads.
- **End of the panel** — use the last `N` observations.

This guarantees two things:

- every observation is replaced with a smoothed value, and
- the panel dimensions remain unchanged.

## Weighting schemes

Within each window, observations can be weighted three ways.

### Equal weights

All values in the window receive equal weight. For `N = 3`:

    (1/3, 1/3, 1/3)

### Half weights

The focal observation receives 50% of the weight; the remaining 50% is
distributed evenly among its neighbors. For `N = 3` centered:

    (0.25, 0.50, 0.25)

### Decay weights

Weights decline geometrically with distance from the focal observation,
following a half-life rule (`1, 1/2, 1/4, …`) before being normalized to
sum to one.

- Centered (`N = 3`): `(0.25, 0.50, 0.25)`
- Edge (`N = 3`): `(0.5714, 0.2857, 0.1429)`

## Missing-value handling

- Missing values are dropped within each window.
- The remaining weights are re-normalized so they still sum to one.
- If *every* value in a window is missing, the result is `NA`.

For example, a window `{2, NA, 4}` with weights `{0.50, 0.25, 0.25}`
collapses to `{2, 4}` with re-normalized weights `{0.667, 0.333}`
(i.e. `0.50 / 0.75` and `0.25 / 0.75`), giving `2.667`.

## Worked example: the math

Take organization `A`, variable `x = 1, 2, 3, 4`, with `window = 3`.

**Equal weights** — each smoothed value is the simple mean of its
window:

    2021 uses {1,2,3} -> 2
    2022 uses {1,2,3} -> 2
    2023 uses {2,3,4} -> 3
    2024 uses {2,3,4} -> 3

giving `c(2, 2, 3, 3)`.

**Half weights** — edges shift the half-weight to the boundary value:

    2021 (edge):     weights (0.50, 0.25, 0.25) on {1,2,3} -> 1.75
    2022 (centered): weights (0.25, 0.50, 0.25) on {1,2,3} -> 2.00
    2023 (centered): weights (0.25, 0.50, 0.25) on {2,3,4} -> 3.00
    2024 (edge):     weights (0.25, 0.25, 0.50) on {2,3,4} -> 3.25

giving `c(1.75, 2.00, 3.00, 3.25)`.

**Decay weights** — geometric decay from the focal point:

    2021 (edge):     (0.5714, 0.2857, 0.1429) on {1,2,3} -> 1.5714
    2022 (centered): (0.25,   0.50,   0.25)   on {1,2,3} -> 2.0000
    2023 (centered): (0.25,   0.50,   0.25)   on {2,3,4} -> 3.0000
    2024 (edge):     (0.1429, 0.2857, 0.5714) on {2,3,4} -> 3.4286

giving approximately `c(1.5714, 2.0000, 3.0000, 3.4286)`.

The examples below reproduce these values in the `x` column.

## Using `panel_smooth()`

The call signature names the panel structure explicitly. Here `vars` is
a custom character vector of columns to smooth; on a real 990 panel you
would instead pass `"PZ"`, `"PC"`, or `"ALL"` to smooth the standard
financial fields (see below).

### Equal weights

``` r

df_equal <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c("x", "y", "z"),
  weights = "equal"
)

knitr::kable(df_equal, digits = 4)
```

| id  |   x |       y |      z | year |
|:----|----:|--------:|-------:|-----:|
| A   |   2 | -0.5003 | 0.3333 | 2021 |
| A   |   2 | -0.5003 | 0.3333 | 2022 |
| A   |   3 | -0.7587 | 0.6667 | 2023 |
| A   |   3 | -0.7587 | 0.6667 | 2024 |
| B   |   6 | -0.2560 | 0.0000 | 2021 |
| B   |   6 | -0.2560 | 0.0000 | 2022 |
| B   |   7 |  0.5727 | 0.3333 | 2023 |
| B   |   7 |  0.5727 | 0.3333 | 2024 |
| C   |  10 |  0.4607 | 0.6667 | 2021 |
| C   |  10 |  0.4607 | 0.6667 | 2022 |
| C   |  11 |  0.1553 | 1.0000 | 2023 |
| C   |  11 |  0.1553 | 1.0000 | 2024 |

### Half weights

``` r

df_half <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c("x", "y", "z"),
  weights = "half"
)

knitr::kable(df_half, digits = 4)
```

| id  |     x |       y |     z | year |
|:----|------:|--------:|------:|-----:|
| A   |  1.75 | -0.4252 |  0.25 | 2021 |
| A   |  2.00 | -0.5913 |  0.25 | 2022 |
| A   |  3.00 | -0.6782 |  0.75 | 2023 |
| A   |  3.25 | -0.8128 |  0.75 | 2024 |
| B   |  5.75 | -0.4165 | -0.25 | 2021 |
| B   |  6.00 | -0.1363 |  0.00 | 2022 |
| B   |  7.00 |  0.4062 |  0.50 | 2023 |
| B   |  7.25 |  0.8265 |  0.25 | 2024 |
| C   |  9.75 |  0.4772 |  0.50 | 2021 |
| C   | 10.00 |  0.5940 |  0.75 | 2022 |
| C   | 11.00 |  0.0817 |  1.00 | 2023 |
| C   | 11.25 |  0.0192 |  1.00 | 2024 |

### Decay weights

``` r

df_decay <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c("x", "y", "z"),
  weights = "decay"
)

knitr::kable(df_decay, digits = 4)
```

| id  |       x |       y |       z | year |
|:----|--------:|--------:|--------:|-----:|
| A   |  1.5714 | -0.4236 |  0.1429 | 2021 |
| A   |  2.0000 | -0.5913 |  0.2500 | 2022 |
| A   |  3.0000 | -0.6782 |  0.7500 | 2023 |
| A   |  3.4286 | -0.8054 |  0.8571 | 2024 |
| B   |  5.5714 | -0.4627 | -0.4286 | 2021 |
| B   |  6.0000 | -0.1363 |  0.0000 | 2022 |
| B   |  7.0000 |  0.4062 |  0.5000 | 2023 |
| B   |  7.4286 |  0.9127 |  0.2857 | 2024 |
| C   |  9.5714 |  0.5653 |  0.4286 | 2021 |
| C   | 10.0000 |  0.5940 |  0.7500 | 2022 |
| C   | 11.0000 |  0.0817 |  1.0000 | 2023 |
| C   | 11.4286 | -0.1200 |  1.0000 | 2024 |

## Choosing what to smooth on a 990 panel

On an assembled 990 panel, `vars` usually names a field *scope* rather
than individual columns:

| Value | Columns smoothed |
|----|----|
| `"PZ"` | PZ-scope fields (990 + 990-EZ), via [`get_pz_fields()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_pz_fields.md) |
| `"PC"` | PC-scope fields (990 only), via [`get_pc_fields()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_pc_fields.md) |
| `"ALL"` | the union of PZ and PC fields |
| character vector | a custom list of column names (as above) |

``` r

# smooth the PZ-scope financial fields with a 3-year window
panel_smoothed <- panel_smooth(panel_clean, vars = "PZ", window = 3)

# then score the cleaned, smoothed panel year by year
panel_ratios <- compute_all_panel(panel_smoothed)
```

## Trailing windows

The default window is centred, so the smoothed value for year `t`
borrows from `t + 1`. When smoothed values feed a baseline or covariate
measured before some event (a merger, a grant, a policy change), that
leaks the future into the past. `align = "right"` uses a trailing window
instead: calendar years `t - (window - 1)` through `t`, never anything
later, and a missing filing year is left as a hole rather than bridged:

``` r

gappy <- data.frame(
  EIN2     = "A",
  TAX_YEAR = c(2015, 2016, 2019, 2020, 2021),
  x        = c(10, 20, 30, 40, 50)
)
data.frame(
  year     = gappy$TAX_YEAR,
  centered = panel_smooth(gappy, vars = "x", window = 3, verbose = FALSE)$x,
  trailing = panel_smooth(gappy, vars = "x", window = 3, align = "right",
                          verbose = FALSE)$x
)
#>   year centered trailing
#> 1 2015       20       10
#> 2 2016       20       15
#> 3 2019       30       30
#> 4 2020       40       35
#> 5 2021       40       40
```

The cost is lag: a real change is spread over `window` years.
`align = "left"` is the forward-looking mirror.

## Summary

[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md):

- preserves panel dimensions and column names,
- handles the beginning and end of each series without dropping
  observations,
- supports equal, half, and decay weighting,
- offers centred, trailing (`"right"`), and forward (`"left"`) windows,
  and
- re-normalizes weights around missing values.

It is particularly useful for stabilizing noisy longitudinal financial
measures while keeping each time point interpretable.
