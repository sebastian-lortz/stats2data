# Plot summary of target-vs-simulated statistics

Creates a chart comparing simulated and target summary statistics.

## Usage

``` r
# S3 method for class 'stats2data_parallel'
plot_fit(
  x,
  standardised = TRUE,
  eps = 1e-12,
  error_type = c("range", "sd"),
  order = c("input", "fit"),
  thresh = x$results[[1L]]$inputs$thresh,
  ...
)

# S3 method for class 'stats2data_mlr'
plot_fit(
  x,
  standardised = TRUE,
  eps = 1e-12,
  order = c("input", "fit"),
  thresh = x$inputs$thresh,
  ...
)

# S3 method for class 'stats2data_aov'
plot_fit(
  x,
  standardised = TRUE,
  eps = 1e-12,
  order = c("input", "fit"),
  thresh = x$inputs$thresh,
  ...
)

# S3 method for class 'stats2data_vec'
plot_fit(
  x,
  standardised = TRUE,
  eps = 1e-12,
  order = c("input", "fit"),
  thresh = x$inputs$thresh,
  ...
)

plot_fit(x, ...)
```

## Arguments

- x:

  A stats2data result object (`stats2data_aov`, `stats2data_mlr`, or
  `stats2data_vec`).

- standardised:

  Logical; if `TRUE` (default), differences are divided by target values
  (except when targets are near zero).

- eps:

  Numeric; zero-threshold. Default `1e-12`.

- error_type:

  Character; `"range"` (default) draws across-run min/max error bars,
  `"sd"` draws `mean +/- SD` across runs.

- order:

  Character; `"input"` (default) or `"fit"`.

- thresh:

  Numeric or `NULL`; convergence threshold used to shade the region
  within `+/- thresh` of the target. Defaults to `x$inputs$thresh`;
  `NULL` suppresses the shading.

- ...:

  Arguments passed to methods (typically `standardised` and `eps`).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Methods (by class)

- `plot_fit(stats2data_parallel)`: Method for parallel results
  (`stats2data_parallel`). Aggregates each simulated parameter as the
  across-run mean (rounded to the target's reported precision) and
  overlays an error bar showing the across-run min/max
  (`error_type = "range"`, default) or `mean +/- SD`
  (`error_type = "sd"`). Module-specific data assembly is dispatched
  internally based on `x$module`.

- `plot_fit(stats2data_mlr)`: Method for MLR results (`stats2data_mlr`).

- `plot_fit(stats2data_aov)`: Method for ANOVA results
  (`stats2data_aov`).

- `plot_fit(stats2data_vec)`: Method for Descriptives results
  (`stats2data_vec`).

## See also

[`plot_convergence`](https://sebastian-lortz.github.io/stats2data/reference/plot_convergence.md),
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md)

## Examples

``` r
set.seed(1)
res_vec <- optim_vec(
  N = 50, target_mean = c(x = 5), target_sd = c(x = 1), range = c(0, 10),
  integer = FALSE, max_iter = 1e4, max_starts = 1, progress_mode = "off"
)
plot_fit(res_vec)

plot_fit(res_vec, standardised = FALSE)


args_mlr <- list(
  N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
  target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
  integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
  target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
  max_iter = 2000, max_starts = 1, hill_climbs = 100,
  progress_mode = "off"
)
res_mlr <- do.call(optim_mlr, args_mlr)
plot_fit(res_mlr, order = "fit")


res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
                          progress_mode = "off")
plot_fit(res_par, error_type = "sd")

```
