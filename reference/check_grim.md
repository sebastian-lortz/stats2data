# Check plausibility of a reported mean with the GRIM test

Performs the GRIM (Granularity-Related Inconsistency of Means) test
(Brown & Heathers, 2017) to assess whether a reported mean is
numerically possible given the sample size and number of decimal places.

## Usage

``` r
check_grim(n, target_mean, decimals, tol = .Machine$double.eps^0.5)
```

## Arguments

- n:

  Integer. Sample size; must be a single positive whole number.

- target_mean:

  Numeric. Reported mean to be tested for plausibility.

- decimals:

  Integer. Number of decimal places in the reported mean.

- tol:

  Numeric. thresh for rounding errors; a single non-negative value.
  Default is `.Machine$double.eps^0.5`.

## Value

A list with components:

- test:

  Logical. `TRUE` if the reported mean is plausible.

- grim_mean:

  Numeric. If the test fails, the nearest plausible mean (rounded to
  `decimals`); otherwise the original `target_mean`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_grim(n = 10, target_mean = 3.7, decimals = 1)
check_grim(n = 10, target_mean = 3.74, decimals = 2)
} # }
```
