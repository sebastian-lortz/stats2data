# Plot cooling schedule for a stats2data result

Visualises the simulated-annealing temperature decay across iterations.

## Usage

``` r
# S3 method for class 'stats2data_aov'
plot_cooling(x, ...)

# S3 method for class 'stats2data_mlr'
plot_cooling(x, ...)

# S3 method for class 'stats2data_vec'
plot_cooling(x, ...)

plot_cooling(x, ...)
```

## Arguments

- x:

  A stats2data result object (`stats2data_aov`, `stats2data_mlr`, or
  `stats2data_vec`).

- ...:

  Arguments passed to methods.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Methods (by class)

- `plot_cooling(stats2data_aov)`: Method for ANOVA results
  (`stats2data_aov`).

- `plot_cooling(stats2data_mlr)`: Method for MLR results
  (`stats2data_mlr`).

- `plot_cooling(stats2data_vec)`: Method for Descriptives results
  (`stats2data_vec`).

## See also

[`plot_convergence`](https://sebastian-lortz.github.io/stats2data/reference/plot_convergence.md)

## Examples

``` r
set.seed(1)
res_aov <- optim_aov(
  S = 20, levels = 2, target_group_means = c(4, 6),
  target_f_list = list(effect = "Factor1", F_value = 10),
  integer = FALSE, range = c(0, 10), formula = "outcome ~ Factor1",
  factor_type = "between", max_iter = 200, max_starts = 1,
  progress_mode = "off"
)
plot_cooling(res_aov)

```
