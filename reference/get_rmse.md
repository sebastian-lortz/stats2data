# Compute RMSE for a stats2data result

Calculates root-mean-square error metrics comparing the achieved summary
statistics of the simulated data against the target inputs.

## Usage

``` r
# S3 method for class 'stats2data_aov'
get_rmse(result, ...)

# S3 method for class 'stats2data_mlr'
get_rmse(result, ...)

# S3 method for class 'stats2data_parallel'
get_rmse(result, ...)

# S3 method for class 'stats2data_vec'
get_rmse(result, ...)

get_rmse(result, ...)
```

## Arguments

- result:

  A stats2data result object (`stats2data_vec`, `stats2data_mlr`, or
  `stats2data_aov`).

- ...:

  Additional arguments passed to methods.

## Value

A named list of RMSE values. Contents depend on the class of `result`:

- For `stats2data_vec`::

  Elements `rmse_mean` and `rmse_sd`.

- For `stats2data_mlr`::

  Elements `rmse_cor`, `rmse_reg`, and `rmse_se`.

- For `stats2data_aov`::

  Elements `rmse_F` and `rmse_mean`.

## Details

For `stats2data_parallel` objects, `get_rmse` returns a list with three
elements:

- between_rmse:

  Data frame. Per-metric summary (Mean, SD, Min, Max) of run-to-run
  RMSE, computed against the grand mean across runs.

- target_rmse:

  Data frame. Per-metric summary of RMSE from each run to the original
  targets.

- raw:

  List with numeric vectors `between` and `target` holding the per-run
  raw RMSE values, keyed by metric name.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_vec(
  N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
  range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
  max_iter = 1e4, max_starts = 1, progress_mode = "off"
)
get_rmse(res)
} # }
```
