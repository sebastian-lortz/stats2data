# Optimize a vector or matrix to match target means and SDs

Uses the stats2data approach to simulate one or multiple vectors so that
each matches specified target means and standard deviations under given
input parameters.

## Usage

``` r
optim_vec(
  N,
  target_mean,
  target_sd,
  range,
  integer,
  thresh = 0.005,
  sprite_prec = c(2, 2),
  max_iter = 5e+05,
  init_temp = 0.001,
  cooling_rate = NULL,
  max_starts = 3,
  progress_mode = "console"
)
```

## Arguments

- N:

  Integer. Number of values in each vector.

- target_mean:

  Named numeric vector. Desired means for each variable.

- target_sd:

  Named numeric vector. Desired standard deviations for each variable.

- range:

  Numeric vector of length 2 or numeric matrix. Allowed value range for
  all variables (vector), or per-variable bounds as a two-row matrix
  matching `target_mean`.

- integer:

  Logical or logical vector. If TRUE, optimize integer values; length 1
  or same length as `target_mean`.

- thresh:

  Numeric. Convergence threshold. Default `1e-3`.

- sprite_prec:

  Integer vector of length 2. Decimal precision for mean and SD when
  using SPRITE for integer data. Default `c(2, 2)`.

- max_iter:

  Integer. Iterations per restart. Default `1e4`.

- init_temp:

  Numeric. Initial SA temperature. Default `1`.

- cooling_rate:

  Numeric in (0,1) or `NULL` (auto). Default `NULL`.

- max_starts:

  Integer. Number of restarts. Default `1`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Default `"console"`.

## Value

A `stats2data.object` list with components `best_error`, `data`,
`inputs`, `track_error`, and `error_msgs`.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- stats2data::optim_vec(
  N           = 100,
  target_mean = c(x = 10.23),
  target_sd   = c(x = 2.11),
  range       = c(0, 20),
  integer     = TRUE,
  sprite_prec = c(2, 2),
  max_iter    = 50000,
  max_starts  = 2
)
} # }
```
