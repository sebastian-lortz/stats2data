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
if (FALSE) { # \dontrun{
res <- parallel_optim(FUN = optim_aov, args = list(...), runs = 20)
plot_stability(res)
} # }

if (FALSE) { # \dontrun{
res <- parallel_optim(FUN = optim_aov, args = list(...), runs = 20)
plot_stability(res)
} # }
```
