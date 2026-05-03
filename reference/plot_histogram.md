# Plot Histograms for Each Variable

Given a data frame or matrix of numeric variables, this function returns
a named list of `ggplot2` histogram (or bar) plots—one per column. For
each variable it displays a vertical line at the mean and an annotation
of mean and standard deviation (or just mean).

## Usage

``` r
plot_histogram(df, tol = 1e-08, SD = TRUE)
```

## Arguments

- df:

  A `data.frame` or matrix whose columns are the variables to plot.

- tol:

  Numeric; thresh for deciding whether a variable is integer-valued.
  Values within `tol` of an integer are plotted as counts (bars).

- SD:

  Logical; if `TRUE`, annotate each plot with mean and standard
  deviation; if `FALSE`, annotate with mean only.

## Value

A named list of `ggplot2` objects, one per column of `df`.
