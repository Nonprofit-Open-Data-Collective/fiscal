# Panel Smoothing Function Documentation

## Demo Data

```r
df <-
structure(list(id = c("A", "A", "A", "A", "B", "B", "B", "B",
"C", "C", "C", "C"), x = 1:12, y = c(-0.200434174007602, -0.863950351889634,
-0.436535322655075, -0.974785355282321, -0.898289859109831, 0.222710881214905,
-0.0929532183791868, 1.58765158319804, 0.526769130770584, 0.993626173795577,
-0.138965277403226, -0.389425585479789), z = c(0, 0, 1, 1, -1,
0, 1, 0, 0, 1, 1, 1), year = c(2021L, 2022L, 2023L, 2024L, 2021L,
2022L, 2023L, 2024L, 2021L, 2022L, 2023L, 2024L)), row.names = c(NA,
-12L), class = "data.frame")
```

```r
df |> knitr::kable()
```

| id |  x |          y |  z | year |
| :- | -: | ---------: | -: | ---: |
| A  |  1 | -0.2004342 |  0 | 2021 |
| A  |  2 | -0.8639504 |  0 | 2022 |
| A  |  3 | -0.4365353 |  1 | 2023 |
| A  |  4 | -0.9747854 |  1 | 2024 |
| B  |  5 | -0.8982899 | -1 | 2021 |
| B  |  6 |  0.2227109 |  0 | 2022 |
| B  |  7 | -0.0929532 |  1 | 2023 |
| B  |  8 |  1.5876516 |  0 | 2024 |
| C  |  9 |  0.5267691 |  0 | 2021 |
| C  | 10 |  0.9936262 |  1 | 2022 |
| C  | 11 | -0.1389653 |  1 | 2023 |
| C  | 12 | -0.3894256 |  1 | 2024 |

---

## Conceptual Overview

The `panel_smooth()` function applies rolling smoothing to panel data while preserving the original panel structure.

### Window Logic

For a given window size `N` (must be odd):

* **Beginning of panel**: use the first `N` observations
* **Middle of panel**: use symmetric window centered on the observation

  * Includes `(N-1)/2` lags and `(N-1)/2` leads
* **End of panel**: use the last `N` observations

This ensures:

* Every observation is replaced with a smoothed value
* The panel dimensions remain unchanged

---

## Weighting Schemes

### 1. Equal Weights

All values in the window receive equal weight.

For `N = 3`:

```
(1/3, 1/3, 1/3)
```

---

### 2. Half Weights

* The focal observation receives 50% of the weight
* The remaining 50% is distributed evenly among neighbors

For `N = 3`:

```
(0.25, 0.5, 0.25)
```

---

### 3. Decay Weights

Weights decline geometrically with distance from the focal observation.

Raw weights follow a half-life rule:

```
1, 1/2, 1/4, ...
```

These are then normalized to sum to 1.

* **Centered (N=3)**:

  ```
  (0.25, 0.5, 0.25)
  ```

* **Edge (N=3)**:

  ```
  (0.5714, 0.2857, 0.1429)
  ```

---

## NA Handling

* Missing values are dropped within each window
* Remaining weights are re-normalized
* If all values are missing ? result is `NA`

---

## Examples

### Equal Weights

```r
df_smooth_equal <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c( "x", "y", "z" ),
  weights = "equal"
)

 df_smooth_equal |> knitr::kable()
```

| id |     x |          y |    z | year |
| :- | ----: | ---------: | ---: | ---: |
| A  |  2.00 | -0.5003066 | 0.33 | 2021 |
| A  |  2.00 | -0.5003066 | 0.33 | 2022 |
| A  |  3.00 | -0.7584237 | 0.67 | 2023 |
| A  |  3.00 | -0.7584237 | 0.67 | 2024 |
| B  |  6.00 | -0.2561774 | 0.00 | 2021 |
| B  |  6.00 | -0.2561774 | 0.00 | 2022 |
| B  |  7.00 |  0.5724698 | 0.33 | 2023 |
| B  |  7.00 |  0.5724698 | 0.33 | 2024 |
| C  | 10.00 |  0.4604767 | 0.67 | 2021 |
| C  | 10.00 |  0.4604767 | 0.67 | 2022 |
| C  | 11.00 |  0.1550784 | 1.00 | 2023 |
| C  | 11.00 |  0.1550784 | 1.00 | 2024 |

---

### Half Weights

```r
df_smooth_half <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c( "x", "y", "z" ),
  weights = "half"
)

 df_smooth_half |> knitr::kable()
```

| id |     x |          y |     z | year |
| :- | ----: | ---------: | ----: | ---: |
| A  |  1.75 | -0.4663137 |  0.25 | 2021 |
| A  |  2.00 | -0.5912170 |  0.25 | 2022 |
| A  |  3.00 | -0.6789516 |  0.75 | 2023 |
| A  |  3.25 | -0.7715261 |  0.75 | 2024 |
| B  |  5.75 | -0.3679647 | -0.50 | 2021 |
| B  |  6.00 | -0.1367056 |  0.00 | 2022 |
| B  |  7.00 |  0.4066145 |  0.50 | 2023 |
| B  |  7.25 |  0.7950000 |  0.25 | 2024 |
| C  |  9.75 |  0.6404836 |  0.25 | 2021 |
| C  | 10.00 |  0.5937646 |  0.75 | 2022 |
| C  | 11.00 |  0.0810670 |  1.00 | 2023 |
| C  | 11.25 | -0.2244456 |  1.00 | 2024 |

---

### Decay Weights

```r
df_smooth_decay <- panel_smooth(
  df      = df,
  window  = 3,
  time    = "year",
  id      = "id",
  vars    = c( "x", "y", "z" ),
  weights = "decay"
)

 df_smooth_decay |> knitr::kable()
```

| id |      x |          y |      z | year |
| :- | -----: | ---------: | -----: | ---: |
| A  |  1.571 | -0.4441952 |  0.143 | 2021 |
| A  |  2.000 | -0.5912170 |  0.250 | 2022 |
| A  |  3.000 | -0.6789516 |  0.750 | 2023 |
| A  |  3.429 | -0.7812406 |  0.857 | 2024 |
| B  |  5.571 | -0.4631166 | -0.571 | 2021 |
| B  |  6.000 | -0.1367056 |  0.000 | 2022 |
| B  |  7.000 |  0.4066145 |  0.500 | 2023 |
| B  |  7.429 |  0.9826574 |  0.286 | 2024 |
| C  |  9.571 |  0.6331382 |  0.429 | 2021 |
| C  | 10.000 |  0.5937646 |  0.750 | 2022 |
| C  | 11.000 |  0.0810670 |  1.000 | 2023 |
| C  | 11.429 | -0.2616055 |  1.000 | 2024 |

---

## Summary

This smoothing approach:

* Preserves panel dimensions
* Handles edge cases cleanly
* Supports flexible weighting schemes
* Is robust to missing values

It is particularly useful for stabilizing noisy longitudinal measures while maintaining interpretability at each time point.
