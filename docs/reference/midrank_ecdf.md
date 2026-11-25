# Mid-rank empirical CDF of `x` against a sorted reference sample

Returns `(#{ref < x} + #{ref <= x}) / (2 n)`, so a block of tied values
(e.g. a pile-up of exact zeros or ones) is placed at the middle of its
block rather than at the top, matching `rank(ties.method = "average")`.
For values inside the reference sample this equals `(rank - 0.5) / n`.

## Usage

``` r
midrank_ecdf(x, ref_sorted)
```
