# Optimize simulated data to match target correlations and regression estimates

Simulates data such that the resulting correlations and regression
coefficients match specified targets under a given regression model.
Internally calls
[`optim_vec`](https://sebastian-lortz.github.io/stats2data/reference/optim_vec.md)
to first generate marginals matching target means and standard
deviations, then optimizes predictor orderings via simulated annealing
(and hill climbing) to match correlation and regression targets.

## Usage

``` r
optim_mlr(
  N,
  target_mean,
  target_sd,
  range,
  integer,
  target_cor,
  target_reg,
  reg_equation,
  sprite_prec = c(2, 2),
  target_se = NULL,
  weight = 0.5,
  thresh = 0.005,
  max_iter = 1e+05,
  init_temp = NULL,
  cooling_rate = NULL,
  max_starts = 3,
  hill_climbs = 10000,
  progress_mode = "console"
)
```

## Arguments

- N:

  Integer. Total number of observations.

- target_mean:

  Named numeric vector. Desired means for each variable (names must
  match variables in `reg_equation`).

- target_sd:

  Named numeric vector. Desired standard deviations for each variable
  (same length and names as `target_mean`).

- range:

  Numeric vector of length 2 or numeric matrix. Allowed value range for
  all variables (vector), or per-variable bounds as a two-row matrix
  with columns matching `target_mean`.

- integer:

  Logical or logical vector. If TRUE, generate integer-valued data;
  length 1 or same length as `target_mean`.

- target_cor:

  Numeric vector. Target upper-triangular (excluding diagonal)
  correlation values for predictor and outcome variables.

- target_reg:

  Numeric vector. Target regression coefficients including intercept,
  matching terms in `reg_equation`.

- reg_equation:

  Character. Regression model formula (e.g., `"Y ~ X1 + X2 + X1:X2"`).

- sprite_prec:

  Integer vector of length 2. Decimal precision for mean and SD when
  using SPRITE for integer data. Default `c(2, 2)`.

- target_se:

  Numeric vector or NULL. Target standard errors for regression
  coefficients (same length as `target_reg`). Default `NULL`.

- weight:

  Numeric vector of length 2. Weights for correlation vs. regression
  error in the objective function. Default `c(1, 1)`.

- thresh:

  Numeric. Convergence threshold. Default `1e-6`.

- max_iter:

  Integer. Iterations per restart. Default `1e5`.

- init_temp:

  Numeric. Initial SA temperature. Default `1`.

- cooling_rate:

  Numeric in (0,1) or `NULL` (auto). Default `NULL`.

- max_starts:

  Integer. Number of restarts. Default `1`.

- hill_climbs:

  Integer or NULL. Number of hill-climbing iterations for optional local
  refinement; if NULL, skips refinement. Default `NULL`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Default `"console"`.

## Value

A `stats2data.object` list with components:

- best_error:

  Numeric. Minimum objective error achieved.

- data:

  Data frame of optimized predictor and outcome values.

- optim_vec:

  The `stats2data.object` returned by the internal `optim_vec` call
  (marginal optimization results).

- inputs:

  List of all input parameters for reproducibility.

- track_error:

  Numeric vector of best error at each iteration.

- track_error_ratio:

  Numeric vector of error ratios (cor vs. reg) per iteration.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- optim_mlr(
  N            = 100,
  target_mean  = c(X1 = 5, X2 = 3, Y = 10),
  target_sd    = c(X1 = 1, X2 = 2, Y = 3),
  range        = c(0, 20),
  integer      = FALSE,
  target_cor   = c(.23, .10, .45),
  target_reg   = c(2.1, 1.2, -0.8),
  reg_equation = "Y ~ X1 + X2",
  max_iter     = 10000,
  hill_climbs  = 50
)
} # }
```
