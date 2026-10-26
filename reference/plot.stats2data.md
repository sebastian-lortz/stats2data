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

The plot is drawn as a side effect; the
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
is returned invisibly.

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
args_mlr <- list(
  N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
  target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
  integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
  target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
  max_iter = 2000, max_starts = 1, hill_climbs = 100,
  progress_mode = "off"
)
res_mlr <- do.call(optim_mlr, args_mlr)
plot(res_mlr)                                  # convergence

plot(res_mlr, which = "ratio", log_y = TRUE)

plot(res_mlr, which = "cooling")

plot(res_mlr, which = "fit", standardised = FALSE)


res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
                          progress_mode = "off")
plot(res_par)                                  # stability

plot(res_par, which = "fit")

```
