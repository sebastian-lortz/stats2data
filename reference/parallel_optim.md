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

  Integer or `NULL`. If non-`NULL`, passed to
  [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html)
  as `future.seed` for reproducibility. Default `NULL`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Controls the outer
  progress bar only; individual runs always execute silently. Default
  `"console"`.

## Value

An object of class `stats2data_parallel` — a list with:

- results:

  List of length `runs`. Each element is the object returned by `FUN`
  (class `stats2data_vec`, `stats2data_mlr`, or `stats2data_aov`).

- best:

  The single result with the lowest `best_error`.

- module:

  Character. One of `"vec"`, `"mlr"`, or `"aov"`, inferred from the
  class of the first result.

- runs:

  Integer. Number of runs completed.

## Details

When `cores > 1`, a
[`multisession`](https://future.futureverse.org/reference/multisession.html)
plan is set up and torn down automatically. Any pre-existing `future`
plan is restored on exit.

## Examples

``` r
if (FALSE) { # \dontrun{
# Parallel ANOVA runs
res <- parallel_optim(
  FUN  = optim_aov,
  args = list(S = 30, levels = c(2, 2), ...),
  runs = 20, cores = 4
)
summary(res)          # aggregated summary
get_stats(res)        # pooled stats across runs
get_rmse(res)         # between-run and target RMSE
res$best              # single best result
} # }
```
