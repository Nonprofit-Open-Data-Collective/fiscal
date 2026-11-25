# Getting started with fiscal

`fiscal` turns IRS 990 financial data into a standard set of nonprofit
fiscal-health metrics — liquidity, leverage, margins, efficiency, and
reserves — each computed the same way every time so results are
comparable across organizations and years.

Two ideas carry the whole package:

1.  **Every metric is a `get_*()` function** that appends its result to
    your data frame. There are about fifty of them, catalogued in
    [`vignette("metrics-catalog")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/metrics-catalog.md).
2.  **Every metric returns four versions** of itself — the raw ratio
    plus winsorized, standardized, and percentile-rank variants — so
    outliers and scale never distort comparisons.

[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
runs the entire battery in one call.

## Where fiscal fits

`fiscal` is the **scoring** layer. Its companion package,
[**panel990**](https://github.com/Nonprofit-Open-Data-Collective/panel990),
is the **data** layer: it retrieves and normalizes the underlying 990
e-file panels. The usual workflow is:

> **panel990** — assemble & clean a 990 panel   →   **fiscal** — score
> every organization-year

The panel-facing functions in `fiscal`
([`get_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_panel.md),
[`deduplicate()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/deduplicate.md),
[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md),
…) are thin, convenience wrappers over `panel990`; see
[`vignette("panel-workflow")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/panel-workflow.md).

## Install

`fiscal` depends on `panel990`; installing from GitHub pulls it in
automatically through the package’s `Remotes:` field.

``` r

# install.packages("remotes")
remotes::install_github("nonprofit-open-data-collective/fiscal")
```

## Quick start

The package bundles `dat10k`, a 10,000-row sample of 990 financials
(both full 990 and 990-EZ filers), so everything below runs offline with
no download.

``` r

library(fiscal)

dim(dat10k)
#> [1] 10000   113
table(dat10k$RETURN_TYPE)
#> 
#>   990 990EZ 
#>  6051  3949
```

Score the entire battery of fiscal-health metrics and append it to your
data:

``` r

scored <- compute_all(dat10k, verbose = FALSE)

# the metric columns that were appended
new_cols <- setdiff(names(scored), names(dat10k))
length(new_cols)
#> [1] 192
head(new_cols)
#> [1] "current"   "current_w" "current_z" "current_p" "quick"     "quick_w"
```

Or compute a single metric. Each `get_*()` function defaults to the
correct 990 e-file field names but accepts your own column names:

``` r

df <- get_debt_assets_ratio(dat10k)

df[1:5, c("debt_assets", "debt_assets_w", "debt_assets_z", "debt_assets_p")]
#>   debt_assets debt_assets_w debt_assets_z debt_assets_p
#> 1   0.1016725     0.1016725  -0.074172138            72
#> 2   0.0000000     0.0000000  -0.758550952             1
#> 3   0.0000000     0.0000000  -0.758550952             1
#> 4   0.1146878     0.1146878  -0.003320409            73
#> 5         NaN           NaN           NaN            NA
```

## The four versions of every metric

[`get_debt_assets_ratio()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_debt_assets_ratio.md)
(like every metric) appends four columns:

| Column | Meaning |
|----|----|
| `debt_assets` | the raw financial ratio |
| `debt_assets_w` | winsorized (extreme tails clipped) |
| `debt_assets_z` | normalized z-score (a distribution-aware transform of the winsorized values, then standardized) |
| `debt_assets_p` | percentile rank of the raw ratio, 1–100 |

The winsorized, standardized, and percentile versions exist so that
outliers and differences in scale don’t dominate cross-organization
comparisons or any downstream index. What each transformation actually
does — and how the package chooses one automatically — is covered in
[`vignette("normalization")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/normalization.md).

``` r

summary(df[, c("debt_assets", "debt_assets_w", "debt_assets_z", "debt_assets_p")])
#>   debt_assets       debt_assets_w    debt_assets_z     debt_assets_p   
#>  Min.   :   -0.33   Min.   :0.0000   Min.   :-0.7586   Min.   :  1.00  
#>  1st Qu.:    0.00   1st Qu.:0.0000   1st Qu.:-0.7586   1st Qu.: 25.00  
#>  Median :    0.00   Median :0.0018   Median :-0.7454   Median : 50.00  
#>  Mean   :    4.77   Mean   :0.2083   Mean   :-0.2021   Mean   : 50.37  
#>  3rd Qu.:    0.14   3rd Qu.:0.1448   3rd Qu.: 0.1460   3rd Qu.: 75.00  
#>  Max.   :39469.20   Max.   :4.2772   Max.   : 2.9480   Max.   :100.00  
#>  NA's   :346        NA's   :346      NA's   :346       NA's   :346
```

Set `summarize = TRUE` on any metric to print these summaries and plot
the four density curves as you compute:

``` r

df <- get_debt_assets_ratio(dat10k, summarize = TRUE)
```

## Controlling the output

[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md)
(and
[`compute_all_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all_panel.md))
takes a `metrics` argument that selects which of the four versions to
return (individual `get_*()` functions always append all four):

| Value     | Returns                 |
|-----------|-------------------------|
| `"ratio"` | raw financial ratio     |
| `"w"`     | winsorized version      |
| `"z"`     | z-score (standardized)  |
| `"p"`     | percentile rank (1–100) |

``` r

# only the raw ratios and their percentile ranks
scored_rp <- compute_all(dat10k, metrics = c("ratio", "p"), verbose = FALSE)
```

`append_to_df` controls the shape of the result:

| Value            | Returns                                               |
|------------------|-------------------------------------------------------|
| `TRUE` (default) | your original data frame with metric columns appended |
| `FALSE`          | e-file identifier columns + metric columns only       |

``` r

metrics_only <- compute_all(dat10k, append_to_df = FALSE, verbose = FALSE)
metrics_only[1:3, 1:6]
#>             EIN2               OBJECTID   ORG_EIN
#> 1 EIN-83-2321599 OID-202403479349200600 832321599
#> 2 EIN-33-0821317 OID-202441069349301519 330821317
#> 3 EIN-30-0846685 OID-202421279349202317 300846685
#>                                ORG_NAME_L1     ORG_NAME_L2 RETURN_AMENDED_X
#> 1             FRANKIE DIAZ HOUSE OF PRAYER MINISTRIES CORP            FALSE
#> 2           SUNYATA MEDITATION ASSOCIATION                            FALSE
#> 3 CANYON HIGH SCHOOL FOOTBALL BOOSTER CLUB                            FALSE
```

The `get_*()` functions are also pipe-enabled, so metrics can be
chained:

``` r

df <- dat10k |>
  get_debt_assets_ratio() |>
  get_current_ratio() |>
  get_program_expenses_ratio()
```

## 990 vs 990-EZ

`dat10k` mixes full 990 and 990-EZ filers. Because the two forms report
different fields, some metrics are computable for both and some only for
full-990 filers — each metric’s help entry (and the catalog) states the
scope under **Calculated For**. `fiscal` handles the difference
automatically: blank financials are interpreted correctly and, where a
field has a 990-EZ equivalent, the two are coalesced. This is the
subject of
[`vignette("form-scope")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/form-scope.md).

## Where to go next

- **[`vignette("metrics-catalog")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/metrics-catalog.md)**
  — every ratio, with formula, inputs, definitional range, and
  benchmarks.
- **[`vignette("normalization")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/normalization.md)**
  — what the `_w` / `_z` / `_p` columns are and how transformations are
  chosen.
- **[`vignette("panel-workflow")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/panel-workflow.md)**
  — build, clean, and score a multi-year panel.
- **[`vignette("panel-smoothing")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/panel-smoothing.md)**
  — smoothing noisy longitudinal financials.
- **[`vignette("fiscal-health-index")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/fiscal-health-index.md)**
  — build a fiscal-health index as a measurement problem: choosing
  indicators, factor analysis, and validation.
- **[`vignette("fiscal-health-workflow")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/fiscal-health-workflow.md)**
  — the staged pipeline that turns a reviewed measurement model into
  reusable dimension and composite scores.
- **[`vignette("form-scope")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/form-scope.md)**
  — 990 vs 990-EZ field scope and sanitization.
