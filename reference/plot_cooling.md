# Plot cooling schedule for a stats2data result

Visualizes the simulated-annealing temperature decay across iterations.
Dispatches to class-specific methods for `stats2data_aov`,
`stats2data_mlr`, and `stats2data_vec`.

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

[`plot_error`](https://sebastian-lortz.github.io/stats2data/reference/plot_error.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot_cooling(res)
} # }
```
