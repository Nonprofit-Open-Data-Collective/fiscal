# Building a 990 panel

A multi-year fiscal-health analysis needs a *clean* panel: one row per
organization per year, noise-tolerant financial fields, and a clear
picture of which organizations are actually observed across time.
`fiscal` provides a small set of functions — thin wrappers over
[**panel990**](https://github.com/Nonprofit-Open-Data-Collective/panel990)
— that take you from raw e-file downloads to a scored panel:

    get_panel()          # 1. retrieve & stack multiple years
       |
    inspect_duplicates() # 2. see duplicate filings
    deduplicate()        #    reduce to one row per org-year
       |
    panel_composition()  # 3. classify who is observed when
    panel_summary()      #    describe the panel
    panel_filter_types() #    keep the organizations you want
       |
    panel_smooth()       # 4. smooth noisy financial series
    compute_all_panel()  # 5. score every organization-year

The retrieval step needs a network connection, so it is shown but not
run here. Every later step runs live on a small synthetic panel.

## 1. Retrieve a panel

[`get_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_panel.md)
downloads multiple years of IRS 990 e-file data and stacks them into one
long-format panel, stamping each row with `TAX_YEAR` and (optionally)
attaching Business Master File metadata.

``` r

# a four-year panel with the default financial tables
panel <- get_panel(years = 2019:2022)

dim(panel)
table(panel$TAX_YEAR)

# skip the BMF join (faster; attach later with merge_bmf())
panel <- get_panel(years = 2019:2022, include_bmf = FALSE)
```

For the rest of this article we use a small hand-built panel that mimics
the messiness of real e-file data — amended returns, group returns, and
a gap:

``` r

mk <- function(ein, yr, grp = "", amd = "", stamp, rev) {
  data.frame(
    EIN2 = ein, TAX_YEAR = yr,
    RETURN_GROUP_X = grp, RETURN_PARTIAL_X = "", RETURN_AMENDED_X = amd,
    RETURN_TIME_STAMP = stamp,
    F9_08_REV_TOT_TOT = rev, F9_09_EXP_TOT_TOT = round(rev * 0.9),
    stringsAsFactors = FALSE
  )
}

panel <- do.call(rbind, list(
  mk("A", 2019, stamp = "2020-05-01", rev = 500000),
  mk("A", 2020, stamp = "2021-05-01", rev = 520000),
  mk("A", 2021, stamp = "2022-05-01", rev = 560000),
  mk("B", 2019, stamp = "2020-05-01", rev = 300000),
  mk("B", 2019, amd = "X", stamp = "2020-09-01", rev = 310000),  # amended duplicate
  mk("B", 2021, stamp = "2022-05-01", rev = 340000),             # 2020 missing
  mk("C", 2020, grp = "X", stamp = "2021-05-01", rev = 800000),  # group return
  mk("C", 2020, stamp = "2021-05-20", rev = 810000),
  mk("C", 2021, stamp = "2022-05-01", rev = 790000)
))

knitr::kable(panel[, c("EIN2", "TAX_YEAR", "RETURN_GROUP_X",
                       "RETURN_AMENDED_X", "F9_08_REV_TOT_TOT")])
```

| EIN2 | TAX_YEAR | RETURN_GROUP_X | RETURN_AMENDED_X | F9_08_REV_TOT_TOT |
|:-----|---------:|:---------------|:-----------------|------------------:|
| A    |     2019 |                |                  |            500000 |
| A    |     2020 |                |                  |            520000 |
| A    |     2021 |                |                  |            560000 |
| B    |     2019 |                |                  |            300000 |
| B    |     2019 |                | X                |            310000 |
| B    |     2021 |                |                  |            340000 |
| C    |     2020 | X              |                  |            800000 |
| C    |     2020 |                |                  |            810000 |
| C    |     2021 |                |                  |            790000 |

## 2. Remove duplicate filings

E-file data can contain several filings for the same organization-year:
amended returns, partial-year returns, and group returns.
[`inspect_duplicates()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/inspect_duplicates.md)
*diagnoses* the problem without changing the data:

``` r

dups <- inspect_duplicates(panel)

dups$summary_types
#>   filing_type rows_total rows_kept rows_dropped
#> 1      normal          2         1            1
#> 2     amended          1         1            0
#> 3       group          1         0            1
```

[`deduplicate()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/deduplicate.md)
then reduces the panel to at most one record per `EIN2` × `TAX_YEAR`
using ordered rules — drop group returns, drop partial-year returns,
then keep the most recent filing by `RETURN_TIME_STAMP`. If a rule would
drop *every* record for an organization-year, those rows are rescued and
passed to the next rule.

``` r

panel_dd <- deduplicate(panel, verbose = FALSE)

nrow(panel)      # before
#> [1] 9
nrow(panel_dd)   # after
#> [1] 7
```

## 3. Describe the panel

Who is actually observed, and when?
[`panel_composition()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_composition.md)
classifies each organization’s presence across the panel window. Ask for
the per-ID classification table with `return_classification = TRUE`:

``` r

cls <- panel_composition(panel_dd, return_classification = TRUE, print_table = FALSE)

cls[, c("EIN2", "panel_type", "panel_year_count", "panel_gap_count")]
#>   EIN2 panel_type panel_year_count panel_gap_count
#> 1    A persistent                3               0
#> 4    B persistent                2               1
#> 6    C    entrant                2               0
```

Each organization is labelled `persistent`, `entrant`, `exit`,
`transient`, or `empty`, with a `panel_spell` of `seamless` or
`segmented` (a segmented spell has a gap year). To attach these columns
to the data instead, use `append_classification = TRUE`;
[`panel_summary()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_summary.md)
then reports the overall shape of the panel:

``` r

panel_cls <- panel_composition(panel_dd, append_classification = TRUE, print_table = FALSE)

panel_summary(panel_cls, print_table = FALSE)
```

Keep only the organizations you want to analyze with
[`panel_filter_types()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_filter_types.md)
— for example, `persistent` organizations, observed in both the first
and the last year of the window (possibly with gaps in between, like
`B`):

``` r

persistent <- panel_filter_types(panel_dd, panel_types = cls, keep = "persistent")

unique(persistent$EIN2)
#> [1] "A" "B"
```

[`panel_impute()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_impute.md)
can fill short internal gaps for selected panel types before smoothing
or scoring:

``` r

panel_imp <- panel_impute(
  panel_dd,
  panel_types = cls,
  types       = "persistent",
  vars        = "F9_08_REV_TOT_TOT"
)
```

## 4. Smooth noisy series

[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md)
replaces selected numeric fields with a rolling within-organization
average, leaving the panel’s shape unchanged. On a real panel you would
pass a field *scope* (`"PZ"`, `"PC"`, or `"ALL"`); here we name the
columns directly:

``` r

panel_sm <- panel_smooth(
  panel_dd,
  vars   = c("F9_08_REV_TOT_TOT", "F9_09_EXP_TOT_TOT"),
  window = 3
)
```

[`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md)
has its own article covering window logic and the equal / half / decay
weighting schemes: see
[`vignette("panel-smoothing")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/panel-smoothing.md).

## 5. Score every organization-year

[`compute_all_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all_panel.md)
is the panel-aware sibling of
[`compute_all()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all.md):
it computes the full battery of fiscal-health metrics **within each
`TAX_YEAR`**, so that the winsorization, normalization, and percentile
ranks are all year-relative.

``` r

panel_scored <- compute_all_panel(panel_sm, metrics = c("ratio", "z"), verbose = FALSE)

new_cols <- setdiff(names(panel_scored), names(panel_sm))
length(new_cols)
#> [1] 2
head(new_cols)
#> [1] "profit_postdepr"   "profit_postdepr_z"
```

The result is a clean, smoothed, scored panel — one row per
organization-year, ready for longitudinal analysis or a composite index
([`vignette("fiscal-health-index")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/fiscal-health-index.md)
and
[`vignette("fiscal-health-workflow")`](https://nonprofit-open-data-collective.github.io/fiscal/articles/fiscal-health-workflow.md)).

## Recap

| Step | Function(s) | Purpose |
|----|----|----|
| Retrieve | [`get_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/get_panel.md) | download & stack years |
| Deduplicate | [`inspect_duplicates()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/inspect_duplicates.md), [`deduplicate()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/deduplicate.md) | one row per org-year |
| Describe | [`panel_composition()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_composition.md), [`panel_summary()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_summary.md), [`panel_filter_types()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_filter_types.md), [`panel_impute()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_impute.md) | who is observed, when; subset & fill |
| Smooth | [`panel_smooth()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/panel_smooth.md) | reduce year-to-year noise |
| Score | [`compute_all_panel()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/compute_all_panel.md) | year-relative fiscal-health metrics |
