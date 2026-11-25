# Diagnose candidate fiscal-health indicators

Computes variable-level diagnostics used to distinguish indicators that
participate in a shared multivariate structure from redundant,
standalone, or noisy indicators. Correlation summaries exclude each
variable's correlation with itself.

## Usage

``` r
fiscal_diagnostics(
  data,
  variables = NULL,
  corr_cutoff = 0.9,
  kmo_low = 0.5,
  communality_low = 0.2,
  mean_abs_cor_low = 0.1,
  smc_high = 0.9,
  vif_high = 10,
  near_zero_sd = 1e-06,
  nfactors_comm = NULL,
  rotate = "oblimin",
  fm = "minres",
  use = "pairwise.complete.obs"
)
```

## Arguments

- data:

  A data frame containing candidate indicators.

- variables:

  Character vector of columns to diagnose. By default, all numeric
  columns are used.

- corr_cutoff:

  Absolute correlation used to flag redundancy.

- kmo_low:

  KMO/MSA threshold used to flag weak shared variance.

- communality_low:

  Threshold used to flag low communality.

- mean_abs_cor_low:

  Threshold used to flag weak connectedness.

- smc_high:

  Squared multiple correlation threshold.

- vif_high:

  Variance inflation factor threshold.

- near_zero_sd:

  Standard-deviation threshold for near-zero variance.

- nfactors_comm:

  Number of factors in the preliminary communality screen. The default
  is between one and three, based on the number of usable indicators.

- rotate:

  Rotation passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html).

- fm:

  Factoring method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html).

- use:

  Missing-value rule passed to
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html).

## Value

An object of class `fiscal_diagnostics`. Its `variables` component is a
variable-level data frame; other components contain the correlation
matrix, redundancy pairs, thresholds, preliminary model, and warnings.

## Details

`psych` is used, when installed, for KMO/MSA and a preliminary
common-factor communality screen. The remaining diagnostics are always
computed. If `psych` is unavailable or the factor screen fails, the
relevant fields are returned as `NA` and the reason is recorded in
`warnings`.
