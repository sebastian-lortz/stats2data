# Plot RMSE distributions for a parallel result

Visualises between-run variability and deviation-from-target RMSE
distributions as box-and-jitter plots, faceted by metric.

## Usage

``` r
# S3 method for class 'stats2data_parallel'
plot(x, ...)
```

## Arguments

- x:

  An object of class `stats2data_parallel`.

- ...:

  Currently unused.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- parallel_optim(FUN = optim_aov, args = list(...), runs = 20)
plot(res)
} # }
```
