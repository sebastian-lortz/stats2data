# Plot error trajectory for a stats2data result

Visualizes how the objective-function error evolves across iterations of
the simulated-annealing optimiser. Dispatches to class-specific methods
for `stats2data_aov`, `stats2data_mlr`, and `stats2data_vec`.

## Usage

``` r
# S3 method for class 'stats2data_aov'
plot_error(x, run = 1L, show_best = TRUE, first_iter = 1L, ...)

# S3 method for class 'stats2data_mlr'
plot_error(
  x,
  run = 1L,
  show_best = TRUE,
  first_iter = 1L,
  ratio = FALSE,
  show_mean = TRUE,
  show_median = TRUE,
  show_final = TRUE,
  ...
)

# S3 method for class 'stats2data_vec'
plot_error(x, run = 1L, show_best = TRUE, first_iter = 1L, ...)

plot_error(x, ...)
```

## Arguments

- x:

  A stats2data result object (`stats2data_aov`, `stats2data_mlr`, or
  `stats2data_vec`).

- run:

  Integer. Index of the run (variable) to plot; default `1`.

- show_best:

  Logical. If `TRUE` (default), marks the minimum error.

- first_iter:

  Integer. Iterations to skip; default `1`.

- ...:

  Arguments passed to methods (see individual method pages).

- ratio:

  Logical. If `TRUE`, plot the correlation-to-regression error ratio
  instead of the total error trajectory. Default `FALSE`.

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

- `plot_error(stats2data_aov)`: Method for ANOVA results
  (`stats2data_aov`).

- `plot_error(stats2data_mlr)`: Method for MLR results
  (`stats2data_mlr`).

- `plot_error(stats2data_vec)`: Method for Descriptives results
  (`stats2data_vec`).

## See also

`plot_error.stats2data_aov`, `plot_error.stats2data_mlr`,
`plot_error.stats2data_vec`,
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot_error(res)
} # }

if (FALSE) { # \dontrun{
res <- optim_mlr(...)
plot_error(res)
plot_error(res, ratio = TRUE)
plot_error(res, ratio = TRUE, show_mean = FALSE)
} # }

if (FALSE) { # \dontrun{
res <- optim_vec(...)
plot_error(res)
} # }

if (FALSE) { # \dontrun{
res <- optim_mlr(...)
plot_error(res)
plot_error(res, ratio = TRUE)
plot_error(res, run = 2, first_iter = 500)
} # }
```
