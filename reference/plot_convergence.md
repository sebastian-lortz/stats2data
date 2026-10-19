# Plot error trajectory for a stats2data result

Visualises how the objective-function error evolves across iterations of
the simulated-annealing optimiser.

## Usage

``` r
# S3 method for class 'stats2data_aov'
plot_convergence(
  x,
  run = 1L,
  show_best = TRUE,
  first_iter = 0L,
  log_y = FALSE,
  thresh = x$inputs$thresh,
  ...
)

# S3 method for class 'stats2data_mlr'
plot_convergence(
  x,
  run = 1L,
  show_best = TRUE,
  first_iter = 0L,
  ratio = FALSE,
  show_mean = TRUE,
  show_median = TRUE,
  show_final = TRUE,
  log_y = FALSE,
  thresh = x$inputs$thresh,
  ...
)

# S3 method for class 'stats2data_vec'
plot_convergence(
  x,
  run = 1L,
  show_best = TRUE,
  first_iter = 0L,
  log_y = FALSE,
  thresh = x$inputs$thresh,
  ...
)

plot_convergence(x, ...)
```

## Arguments

- x:

  A stats2data result object (`stats2data_aov`, `stats2data_mlr`, or
  `stats2data_vec`).

- run:

  Integer. Index of the run (variable) to plot; default `1`.

- show_best:

  Logical. If `TRUE` (default), marks the minimum objective value.

- first_iter:

  Integer. Iterations to skip; default `0`.

- log_y:

  Logical. If `TRUE`, the y axis uses a base-10 logarithmic scale;
  non-positive objective values are dropped with a warning. Default
  `FALSE`.

- thresh:

  Numeric or `NULL`; convergence threshold, shaded as the region between
  zero and `thresh`. Defaults to the threshold of the run,
  `x$inputs$thresh`; `NULL` suppresses the shading.

- ...:

  Arguments passed to methods (see individual method pages).

- ratio:

  Logical. If `TRUE`, plot the correlation-to-regression objective value
  ratio instead of the total objective value trajectory. Default
  `FALSE`.

- show_mean:

  Logical. When `ratio = TRUE`, draw a horizontal line at the mean
  ratio. Default `TRUE`. Ignored when `ratio = FALSE`.

- show_median:

  Logical. When `ratio = TRUE`, draw a horizontal line at the median
  ratio. Default `TRUE`. Ignored when `ratio = FALSE`.

- show_final:

  Logical. When `ratio = TRUE`, draw a horizontal line at the final
  ratio. Default `TRUE`. Ignored when `ratio = FALSE`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Details

For `stats2data_mlr` objects, set `ratio = TRUE` to plot the
correlation-to-regression error ratio instead of the total error.

## Methods (by class)

- `plot_convergence(stats2data_aov)`: Method for ANOVA results
  (`stats2data_aov`).

- `plot_convergence(stats2data_mlr)`: Method for MLR results
  (`stats2data_mlr`).

- `plot_convergence(stats2data_vec)`: Method for Descriptives results
  (`stats2data_vec`).

## See also

`plot_convergence.stats2data_aov`, `plot_convergence.stats2data_mlr`,
`plot_convergence.stats2data_vec`,
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot_convergence(res)
plot_convergence(res, log_y = TRUE)
} # }

if (FALSE) { # \dontrun{
res <- optim_mlr(...)
plot_convergence(res)
plot_convergence(res, log_y = TRUE)
plot_convergence(res, ratio = TRUE)
plot_convergence(res, ratio = TRUE, show_mean = FALSE)
} # }

if (FALSE) { # \dontrun{
res <- optim_vec(...)
plot_convergence(res)
plot_convergence(res, log_y = TRUE)
} # }

if (FALSE) { # \dontrun{
res <- optim_mlr(...)
plot_convergence(res)
plot_convergence(res, ratio = TRUE)
plot_convergence(res, run = 2, first_iter = 500)
} # }
```
