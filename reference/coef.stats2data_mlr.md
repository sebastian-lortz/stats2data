# Extract regression coefficients from a stats2data MLR result

Fits the regression model stored in `object$inputs$reg_equation` to the
simulated data and returns the estimated coefficients.

## Usage

``` r
# S3 method for class 'stats2data_mlr'
coef(object, ...)
```

## Arguments

- object:

  An object of class `stats2data_mlr`.

- ...:

  Additional arguments (unused).

## Value

A named numeric vector of regression coefficients.
