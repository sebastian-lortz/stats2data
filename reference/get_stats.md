# Extract statistics from a \`stats2data\` result

Computes and returns key analytical outputs from a \`stats2data\` result
object, including means, standard deviations, correlations, regression
coefficients, or F-statistics depending on the module that produced the
result.

## Usage

``` r
# S3 method for class 'stats2data_aov'
get_stats(result, ...)

# S3 method for class 'stats2data_mlr'
get_stats(result, ...)

# S3 method for class 'stats2data_parallel'
get_stats(result, ...)

# S3 method for class 'stats2data_vec'
get_stats(result, ...)

get_stats(result, ...)
```

## Arguments

- result:

  A stats2data result object (`stats2data_vec`, `stats2data_mlr`, or
  `stats2data_aov`).

- ...:

  Additional arguments passed to methods.

## Value

A named list of statistics. Contents depend on the class of `result`:

- For `stats2data_vec`::

  Elements `mean` and `sd`.

- For `stats2data_mlr`::

  Elements `model`, `reg`, `se`, `cor`, `mean`, and `sd`.

- For `stats2data_aov`::

  Elements `model`, `F_value`, and `mean`.

## Details

For `stats2data_parallel` objects, `get_stats` aggregates each numeric
component returned by the per-run `get_stats` method across all runs.
The result is a named list of data frames, one per component (e.g.,
`F_value`, `mean`, `reg`, `cor`, `sd`), each containing columns `mean`,
`median`, `sd`, `min`, and `max`.

## Cell ordering for `stats2data_aov`

The vector `$mean` is returned in \*\*sorted-key\*\* cell order (Factor1
fastest, then Factor2, then Factor3, ...). This matches the order the
optimiser uses internally and the order in which `target_group_means` is
consumed by
[`optim_aov`](https://sebastian-lortz.github.io/stats2data/reference/optim_aov.md).
The returned vector is named with the cell identifier (e.g. `"1_1"`,
`"1_2"`, `"2_1"`, ...) so the cell each value belongs to is unambiguous
downstream.

## Model formula

`get_stats` reuses the formula stored in `result$inputs$formula` rather
than reconstructing one from the factor names. This guarantees the model
fitted here is the same as the one the optimiser used, eliminating drift
across afex versions or design types (purely-within, purely-between,
mixed).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_vec(
  N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
  range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
  max_iter = 1e4, max_starts = 1, progress_mode = "off"
)
get_stats(res)
} # }
```
