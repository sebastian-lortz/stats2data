# Default plots for stats2data results

Convenience [`plot`](https://rdrr.io/r/base/plot.html) methods so that
`plot(res)` works on any stats2data result object.

## Usage

``` r
# S3 method for class 'stats2data_vec'
plot(x, which = c("convergence", "cooling", "fit"), ...)

# S3 method for class 'stats2data_aov'
plot(x, which = c("convergence", "cooling", "fit"), ...)

# S3 method for class 'stats2data_mlr'
plot(x, which = c("convergence", "ratio", "cooling", "fit"), ...)

# S3 method for class 'stats2data_parallel'
plot(x, which = c("stability", "fit"), ...)
```

## Arguments

- x:

  A stats2data result object.

- which:

  Character; which panel to draw.

- ...:

  Further arguments passed to the underlying plotting function.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## Details

The explicit functions remain the primary interface and expose the full
argument set:
[`plot_convergence`](https://sebastian-lortz.github.io/stats2data/reference/plot_convergence.md),
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md),
[`plot_fit`](https://sebastian-lortz.github.io/stats2data/reference/plot_fit.md),
and
[`plot_stability`](https://sebastian-lortz.github.io/stats2data/reference/plot_stability.md).

Available panels by class:

- `stats2data_vec`:

  `"convergence"` (default), `"cooling"`, `"fit"`.

- `stats2data_aov`:

  `"convergence"` (default), `"cooling"`, `"fit"`.

- `stats2data_mlr`:

  `"convergence"` (default), `"ratio"`, `"cooling"`, `"fit"`.

- `stats2data_parallel`:

  `"stability"` (default), `"fit"`.

## See also

[`plot_convergence`](https://sebastian-lortz.github.io/stats2data/reference/plot_convergence.md),
[`plot_cooling`](https://sebastian-lortz.github.io/stats2data/reference/plot_cooling.md),
[`plot_fit`](https://sebastian-lortz.github.io/stats2data/reference/plot_fit.md),
[`plot_stability`](https://sebastian-lortz.github.io/stats2data/reference/plot_stability.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot(res)                          # convergence
plot(res, which = "cooling")
plot(res, which = "fit", standardised = FALSE)

mlr <- optim_mlr(...)
plot(mlr, which = "ratio", log_y = TRUE)

par_res <- parallel_optim(FUN = optim_aov, args = list(...), runs = 20)
plot(par_res)                      # stability
plot(par_res, which = "fit")
} # }
```
