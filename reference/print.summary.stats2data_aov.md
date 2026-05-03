# Print a stats2data ANOVA summary

Prints (in order): the design header, RMSE summary, F-values comparison
table, and group-means comparison table. Each comparison block is
preceded by a horizontal rule. If a comparison data frame is missing or
empty, the method emits an explicit warning instead of silently skipping
it; this makes diagnostics easier when an upstream step has gone wrong.

## Usage

``` r
# S3 method for class 'summary.stats2data_aov'
print(x, ...)
```

## Arguments

- x:

  An object of class `summary.stats2data_aov`.

- ...:

  Additional arguments (unused).

## Value

Invisibly returns `x`.
