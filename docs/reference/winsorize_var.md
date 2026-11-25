# Backward-compatible alias for apply_transformations()

`winsorize_var()` is a deprecated alias retained for backward
compatibility. New code should use
[`apply_transformations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_transformations.md)
directly.

## Usage

``` r
winsorize_var(
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

Same as
[`apply_transformations()`](https://nonprofit-open-data-collective.github.io/fiscal/reference/apply_transformations.md).
