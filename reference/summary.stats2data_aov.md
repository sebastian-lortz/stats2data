# Summarize a stats2data ANOVA result

Computes target-vs-simulated statistics and RMSE for a `stats2data_aov`
object.

## Usage

``` r
# S3 method for class 'stats2data_aov'
summary(object, ...)
```

## Arguments

- object:

  An object of class `stats2data_aov`.

- ...:

  Additional arguments (unused).

## Value

An object of class `summary.stats2data_aov`, printed by
[`print.summary.stats2data_aov`](https://sebastian-lortz.github.io/stats2data/reference/print.summary.stats2data_aov.md).
The list always contains `f_comparison` and `means_comparison` data
frames, even if
[`get_stats()`](https://sebastian-lortz.github.io/stats2data/reference/get_stats.md)
returned `NA` values; in that case the simulated columns are `NA` and
the print method emits a warning.
