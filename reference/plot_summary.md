# Plot summary of target-vs-simulated statistics

Creates a lollipop chart comparing simulated and target summary
statistics. Dispatches to class-specific methods for `stats2data_aov`,
`stats2data_mlr`, and `stats2data_vec`.

## Usage

``` r
# S3 method for class 'stats2data_parallel'
plot_summary(
  x,
  standardised = TRUE,
  eps = 1e-12,
  error_type = c("range", "sd"),
  ...
)

# S3 method for class 'stats2data_mlr'
plot_summary(x, standardised = TRUE, eps = 1e-12, ...)

# S3 method for class 'stats2data_aov'
plot_summary(x, standardised = TRUE, eps = 1e-12, ...)

# S3 method for class 'stats2data_vec'
plot_summary(x, standardised = TRUE, eps = 1e-12, ...)

plot_summary(x, ...)
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
  `"sd"` draws `mean 1 SD` across runs.

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

- `plot_summary(stats2data_parallel)`: Method for parallel results
  (`stats2data_parallel`). Aggregates each simulated parameter as the
  across-run mean (rounded to the target's reported precision) and
  overlays an error bar showing the across-run min/max
  (`error_type = "range"`, default) or `mean 1 SD`
  (`error_type = "sd"`). Module-specific data assembly is dispatched
  internally based on `x$module`.

- `plot_summary(stats2data_mlr)`: Method for MLR results
  (`stats2data_mlr`).

- `plot_summary(stats2data_aov)`: Method for ANOVA results
  (`stats2data_aov`).

- `plot_summary(stats2data_vec)`: Method for Descriptives results
  (`stats2data_vec`).

## See also

[`plot_error`](https://sebastian-lortz.github.io/stats2data/reference/plot_error.md),
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- parallel_optim(FUN = optim_mlr, args = list(...), runs = 20)
plot_summary(res)
plot_summary(res, standardised = FALSE, error_type = "sd")
} # }

if (FALSE) { # \dontrun{
res <- optim_mlr(...)
plot_summary(res)
plot_summary(res, standardised = FALSE)
} # }

if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot_summary(res)
} # }

if (FALSE) { # \dontrun{
res <- optim_vec(...)
plot_summary(res)
} # }

if (FALSE) { # \dontrun{
res <- optim_vec(...)
plot_summary(res)
plot_summary(res, standardised = FALSE)
} # }
```
