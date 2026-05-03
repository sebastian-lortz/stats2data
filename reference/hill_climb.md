# Perform hill-climbing optimization

Executes a hill-climbing algorithm to iteratively improve a candidate
data set by minimizing a supplied error function. Supports the LM
module.

## Usage

``` r
hill_climb(
  current_candidate,
  error_function,
  N,
  hill_climbs = 100,
  num_preds = NULL,
  neighborhood_size = 4,
  progressor = NULL,
  pb_interval = NULL,
  progress_mode = "console"
)
```

## Arguments

- current_candidate:

  Matrix. The predictor matrix to be optimized.

- error_function:

  Function. Objective function that takes a candidate and returns a list
  with element `$total_error`.

- N:

  Integer. Number of observations (rows) in `current_candidate`.

- hill_climbs:

  Integer. Maximum number of iterations. Default `1e2`.

- num_preds:

  Integer. Number of predictor columns.

- neighborhood_size:

  Integer. Number of candidate moves evaluated per iteration. Default
  `4`.

- progressor:

  Function or NULL. A `progressr` progressor callback. Default `NULL`.

- pb_interval:

  Integer or NULL. Interval (in iterations) between progressor calls.
  Default `NULL`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Default `"console"`.

## Value

A list with components:

- best_candidate:

  The optimized candidate matrix achieving lowest error.

- best_error:

  Numeric. The minimum objective function value found.

## Examples

``` r
if (FALSE) { # \dontrun{
hill_climb(
  current_candidate = matrix(rnorm(200), 100, 2),
  error_function = function(cand) list(total_error = sum(cand^2)),
  N = 100,
  hill_climbs = 100,
  num_preds = 2
)
} # }
```
