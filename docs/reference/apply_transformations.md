# Apply winsorization, normalization, and percentile ranking to a ratio vector

`apply_transformations()` is the central post-computation step called by
every `get_*()` ratio function. It produces four versions of a ratio:

- **raw** (`raw`; the unsuffixed metric column): the unmodified computed
  ratio.

- **winsorized** (`_w`): outliers clipped to bounds determined by the
  `range` argument and the `winsorize` proportion, via
  [`winsorize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/winsorize_x.md).

- **normalized** (`_z`): a distribution-appropriate transformation of
  the winsorized values, robustly standardized. The transformation is
  selected and fitted via
  [`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md)
  (on the stable interior of non-NA, non-sentinel observations; the
  `"rank_normal"` reference distribution also keeps the winsorized
  tails), then scored on the full vector via
  [`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md).

- **percentile** (`_p`): integer percentile rank (1-100) based on the
  raw values, via
  [`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html).

## Usage

``` r
apply_transformations(
  x,
  winsorize = 0.98,
  offset = 0.001,
  range = "np",
  normalize_type = NULL
)
```

## Arguments

- x:

  Numeric vector (the computed ratio, before any transformation).

- winsorize:

  Winsorization proportion between 0 and 1 (default `0.98`, which clips
  at the 1st and 99th percentiles for `"np"` range).

- offset:

  Sentinel offset applied to fixed bounds (default `0.001`).
  Observations clipped to a fixed bound are stored just outside it (a
  lower bound minus `offset`, an upper bound plus `offset`) so they
  remain identifiable in the `_w` column.

- range:

  Character string describing the theoretical range of the ratio.
  Controls how the lower and upper winsorization bounds are determined:

  `"np"`

  : Negative to positive (unbounded both directions). Winsorizes
    symmetrically at the `(1-winsorize)/2` and `1-(1-winsorize)/2`
    percentiles. Default behaviour.

  `"zp"`

  : Zero to positive. The lower bound is fixed at `-offset` (flagging
    truncated-at-zero values) and the upper bound is the `winsorize`
    percentile of the full distribution.

  `"zo"`

  : Zero to one. Both bounds are fixed (`-offset` and `1+offset`),
    flagging values outside `[0, 1]`. No percentile-based clipping is
    applied.

  `"nz"`

  : Negative to zero. The upper bound is fixed at `+offset` and the
    lower bound is the `1-winsorize` percentile.

  `"lo;hi"`

  : Custom numeric range, e.g. `"0;10"`. The lower bound is fixed at
    `lo - offset` and the upper bound at `hi + offset`.

- normalize_type:

  Transformation type override passed to
  [`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md).
  One of `NULL` (auto-detect among `"asinh"`, `"logit"`, and
  `"rank_normal"`), `"asinh"`, `"logit"`, `"rank_normal"`, or `"hurdle"`
  (never auto-selected). Default `NULL`.

## Value

A named list with elements `raw`, `winsorized`, `z`, `pctile`.

## Details

**Winsorization**

Delegated entirely to
[`winsorize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/winsorize_x.md),
which handles all range codes, computes sentinel flags, and returns the
winsorized vector alongside diagnostic metadata.

**Normalization (`_z` column)**

[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md)
is called first to fit transformation parameters (type, scale constant,
center, spread) on the stable interior of the winsorized distribution.
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md)
then scores the full original vector using those fitted parameters. This
two-step design means the fitted model can be reused on new data if
needed. Centering and scaling use median/MAD, falling back to mean/SD
when the MAD is zero. With fewer than three non-missing values no model
is fitted and `z` is returned as `NA`.

**Percentile rank (`_p` column)**

Integer percentile rank from 1 to 100 based on the raw (pre-winsorized)
values, computed with
[`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html).

## See also

[`winsorize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/winsorize_x.md),
[`find_best_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/find_best_normalization.md),
[`apply_normalization()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_normalization.md),
[`normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/normalize_x.md),
[`plot_normalize_x()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/plot_normalize_x.md)

## Examples

``` r
library( fiscal )
data( dat10k )

# winsorize and normalize the debt-to-assets ratio
ratio <- dat10k$F9_10_LIAB_TOT_EOY / dat10k$F9_10_ASSET_TOT_EOY
out   <- apply_transformations( ratio, winsorize = 0.98, range = "zo" )
names( out )
#> [1] "raw"        "winsorized" "z"          "pctile"    
summary( out$z )
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> -0.6745 -0.6745  0.0000  0.0297  0.5067  1.4145    1867 
```
