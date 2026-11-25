# Robust center and scale with a classical fallback

Uses median/MAD when `robust = TRUE`. When more than half of the values
are tied (e.g. a zero-inflated ratio) the MAD is zero, which would turn
the whole standardized column into `NA`; in that case fall back to
mean/SD.

## Usage

``` r
center_scale(x, robust = TRUE)
```
