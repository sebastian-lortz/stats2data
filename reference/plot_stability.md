# Plot run-to-run stability for a stats2data result

Visualises two RMSE distributions across replications: run-to-run
variability, computed against the grand mean over runs, and accuracy,
computed against the requested targets. Box-and-jitter, faceted by
metric. Requires replications, so it is defined only for
`stats2data_parallel` objects produced by `parallel_optim`.

## Usage

``` r
# S3 method for class 'stats2data_parallel'
plot_stability(x, ...)

plot_stability(x, ...)
```

## Arguments

- x:

  A `stats2data_parallel` object.

- ...:

  Arguments passed to methods.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Methods (by class)

- `plot_stability(stats2data_parallel)`: Method for parallel results
  (`stats2data_parallel`). Box-and-jitter distributions of run-to-run
  and against-target RMSE, faceted by metric.

## See also

[`get_rmse`](https://sebastian-lortz.github.io/stats2data/reference/get_rmse.md),
[`plot_fit`](https://sebastian-lortz.github.io/stats2data/reference/plot_fit.md),
[`plot.stats2data`](https://sebastian-lortz.github.io/stats2data/reference/plot.stats2data.md)

## Examples

``` r
args_mlr <- list(
  N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
  target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
  integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
  target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
  max_iter = 2000, max_starts = 1, hill_climbs = 100,
  progress_mode = "off"
)
res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
                          progress_mode = "off")
plot_stability(res_par)

```
