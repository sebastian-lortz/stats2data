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
  converged, i.e. its `best_error` is below the convergence threshold.
  Default `FALSE`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Controls the outer
  progress bar only; individual runs always execute silently. Default
  `"console"`.

## Value

An object of class `stats2data_parallel` — a list with:

- results:

  List of completed runs. Each element is the object returned by `FUN`
  (class `stats2data_vec`, `stats2data_mlr`, or `stats2data_aov`).

- best:

  The single result with the lowest `best_error`.

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

Convergence of a single run is judged as `best_error < thresh`. For
`stats2data_vec` results, where `best_error` is a list of per-variable
errors, every component must be below `thresh`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Parallel ANOVA runs
res <- parallel_optim(
  FUN  = optim_aov,
  args = list(S = 30, levels = c(2, 2), ...),
  runs = 20, cores = 4
)
summary(res)

# Stop as soon as one run converges
res <- parallel_optim(
  FUN  = optim_mlr,
  args = list(N = 100, thresh = 1e-3, ...),
  runs = 20, cores = 4, stop_conv = TRUE
)
} # }
```
