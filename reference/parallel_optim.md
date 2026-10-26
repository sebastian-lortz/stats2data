# Run an optimization function multiple times in parallel

Wrapper that executes any `optim_*` function repeatedly and collects the
results.

## Usage

``` r
parallel_optim(
  FUN,
  args,
  runs = 10L,
  cores = 1L,
  seed = NULL,
  stop_conv = FALSE,
  progress_mode = "console"
)
```

## Arguments

- FUN:

  Function. One of
  [`optim_vec`](https://sebastian-lortz.github.io/stats2data/reference/optim_vec.md),
  [`optim_mlr`](https://sebastian-lortz.github.io/stats2data/reference/optim_mlr.md),
  or
  [`optim_aov`](https://sebastian-lortz.github.io/stats2data/reference/optim_aov.md).

- args:

  List. Named arguments forwarded to `FUN`. `progress_mode` is silently
  forced to `"off"` inside workers.

- runs:

  Integer. Number of independent replications. Default `10`.

- cores:

  Integer. Number of parallel workers. Default `1` (sequential). Values
  `> 1` require the future and future.apply packages.

- seed:

  Integer or `NULL`. If non-`NULL`, a reproducible seed is generated per
  run. Default `NULL`.

- stop_conv:

  Logical. If `TRUE`, stop launching further runs as soon as one run has
  converged, i.e. its `best_objective` is below the convergence
  threshold. Default `FALSE`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Controls the outer
  progress bar only; individual runs always execute silently. Default
  `"console"`.

## Value

An object of class `stats2data_parallel`, a list with:

- results:

  List of completed runs. Each element is the object returned by `FUN`
  (class `stats2data_vec`, `stats2data_mlr`, or `stats2data_aov`).

- best:

  The single result with the lowest `best_objective`.

- module:

  Character. One of `"vec"`, `"mlr"`, or `"aov"`, inferred from the
  class of the first result.

- runs:

  Integer. Number of runs actually completed.

- runs_requested:

  Integer. Number of runs requested via `runs`.

- converged:

  Logical. `TRUE` if a converged run triggered early stopping.

- thresh:

  Numeric or `NULL`. Threshold used to judge convergence.

## Details

When `cores > 1`, a
[`multisession`](https://future.futureverse.org/reference/multisession.html)
plan is set up and torn down automatically. Any pre-existing `future`
plan is restored on exit.

With `stop_conv = TRUE` and `cores > 1`, runs are dispatched in batches
of `cores` and the convergence check happens after each batch. Up to
`cores - 1` runs may therefore complete after the first converged run;
they are kept in `results`. With `stop_conv = FALSE` all runs are
dispatched in a single call, as before.

Convergence of a single run is judged as `best_objective < thresh`. For
`stats2data_vec` results, where `best_objective` is a list of
per-variable errors, every component must be below `thresh`.

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
res <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
                      progress_mode = "off")
summary(res)
#> stats2data Parallel Summary
#> -----------------------------------------------
#> Module: mlr  | Runs: 3 
#> Best objective:  0.001472 
#> Mean error:  0.00407244 
#> SD error:    0.0045041 
#> 
#> Target RMSE (across runs):
#>    metric     mean       sd      min      max
#>  rmse_cor 0.004317 0.006338 0.000658 0.011636
#>  rmse_reg 0.003828 0.002670 0.002286 0.006911
#> 
#> Between-run RMSE:
#>    metric     mean       sd      min      max
#>  rmse_cor 0.005298 0.002294 0.003973 0.007946
#>  rmse_reg 0.003248 0.001406 0.002436 0.004872
#> 
#> Aggregated Statistics:
#>   reg:
#>               mean median       sd    min    max
#> (Intercept) 1.6489 1.6466 0.004068 1.6466 1.6536
#> X1          0.3911 0.3890 0.003675 0.3890 0.3953
#> X2          0.2555 0.2583 0.004832 0.2499 0.2583
#> 
#>   se:
#>               mean median        sd    min    max
#> (Intercept) 0.7066 0.7043 0.0039186 0.7043 0.7111
#> X1          0.1495 0.1495 0.0001142 0.1495 0.1497
#> X2          0.1261 0.1260 0.0000963 0.1260 0.1262
#> 
#>   cor:
#>     mean median       sd    min    max
#> 1 0.2160 0.2207 0.008133 0.2066 0.2207
#> 2 0.4891 0.4891 0.000000 0.4891 0.4891
#> 3 0.4150 0.4200 0.008713 0.4050 0.4200
#> 
#>   mean:
#>     mean median sd   min   max
#> X1 3.933  3.933  0 3.933 3.933
#> X2 3.833  3.833  0 3.833 3.833
#> Y  4.167  4.167  0 4.167 4.167
#> 
#>   sd:
#>     mean median sd   min   max
#> X1 1.437  1.437  0 1.437 1.437
#> X2 1.704  1.704  0 1.704 1.704
#> Y  1.341  1.341  0 1.341 1.341
#> 

# stop as soon as one run reaches the threshold
res <- parallel_optim(optim_mlr, c(args_mlr, thresh = 0.05), runs = 3,
                      seed = 2026, stop_conv = TRUE, progress_mode = "off")
res$runs
#> [1] 1
```
