# Resolve unrestricted net assets with a fallback for non-SFAS 117 filers

Part X lines 27-28 (net assets without / with donor restrictions) are
completed only by organizations that follow SFAS 117 (ASC 958). Other
filers report equity on lines 30-32 (capital stock, paid-in surplus,
retained earnings), so their unrestricted net assets read as zero even
when total net assets (line 33) are large. Without donor-restriction
reporting, that equity is effectively unrestricted.

This helper returns unrestricted net assets, substituting total net
assets for rows where unrestricted **and** restricted net assets are
both zero. Rows where unrestricted net assets are `NA` (e.g. 990-EZ
filers) are left `NA`.

## Usage

``` r
resolve_unrestricted_net_assets(
  dat,
  unrestricted = "F9_10_NAFB_UNRESTRICT_EOY",
  restricted = "F9_10_NAFB_RESTRICT_EOY",
  total = "F9_10_NAFB_TOT_EOY",
  verbose = TRUE
)
```

## Arguments

- dat:

  A `data.frame`.

- unrestricted:

  Column name(s) for unrestricted net assets (line 27).

- restricted:

  Column name(s) for restricted net assets (line 28), or `NULL`. When
  `NULL` or absent from `dat`, restricted net assets are treated as
  zero, so the fallback applies wherever unrestricted net assets are
  zero.

- total:

  Column name(s) for total net assets (line 33), or `NULL` to disable
  the fallback.

- verbose:

  Logical. Print the number of substituted rows. Default `TRUE`.

## Value

A numeric vector of length `nrow(dat)`.

## Examples

``` r
d <- data.frame( F9_10_NAFB_UNRESTRICT_EOY = c( 100, 0, 0 ),
                 F9_10_NAFB_RESTRICT_EOY   = c(  50, 0, 80 ),
                 F9_10_NAFB_TOT_EOY        = c( 150, 900, 80 ) )
resolve_unrestricted_net_assets( d )   # 100 900 0
#>    :: Unrestricted and restricted net assets both zero (non-SFAS 117) :: 1 case(s) use total net assets
#> [1] 100 900   0
```
