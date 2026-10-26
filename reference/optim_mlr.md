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
  correlation values of all variables in `target_mean`, ordered as
  [`upper.tri()`](https://rdrr.io/r/base/lower.tri.html) of the
  correlation matrix with rows and columns in the order of
  `target_mean`; `NA` for unreported correlations.

- target_reg:

  Numeric vector. Target regression coefficients including intercept,
  matching terms in `reg_equation`.

- reg_equation:

  Character. Regression model formula (e.g., `"Y ~ X1 + X2 + X1:X2"`).

- sprite_prec:

  Integer vector of length 2. Decimal precision for mean and SD when
  using SPRITE for integer data; must be at least the number of decimals
  of the means and SDs of the integer variables, because SPRITE does not
  round the targets. Default `c(2, 2)`.

- target_se:

  Numeric vector or NULL. Target standard errors for regression
  coefficients (same length as `target_reg`). Default `NULL`.

- weight:

  Numeric in \[0, 1\]. Weight of the correlation error in the objective
  function; the regression error is weighted by `1 - weight`. Default
  `0.5`.

- thresh:

  Numeric. Convergence threshold. Default `5e-3`.

- max_iter:

  Integer. Iterations per restart. Default `1e5`.

- init_temp:

  Numeric or `NULL`. Initial SA temperature; if `NULL`, it is set such
  that the median objective change of 500 random swaps is accepted with
  probability 0.5. Default `NULL`.

- cooling_rate:

  Numeric in (0,1) or `NULL` (auto). Default `NULL`.

- max_starts:

  Integer. Number of restarts. Default `3`.

- hill_climbs:

  Integer or NULL. Number of hill-climbing iterations for optional local
  refinement; if NULL or 0, skips refinement. Default `1e4`.

- progress_mode:

  Character: `"console"`, `"shiny"`, or `"off"`. Default `"console"`.

## Value

An object of class `stats2data_mlr`, a list with components:

- best_objective:

  Numeric. Minimum objective error achieved.

- data:

  Data frame of optimised predictor and outcome values.

- optim_vec:

  The `stats2data_vec` object returned by the internal `optim_vec` call
  (marginal optimization results).

- inputs:

  List of the input parameters as used internally: `weight` as
  `c(weight, 1 - weight)`, `target_cor` reordered to (predictors,
  outcome), and the set `init_temp` and `cooling_rate`.

- track_objective:

  Numeric vector of best objective value at each iteration.

- track_objective_ratio:

  Numeric vector of error ratios (cor vs. reg) per iteration.

## Details

The marginals are generated first by
[`optim_vec`](https://sebastian-lortz.github.io/stats2data/reference/optim_vec.md);
its warnings are passed on with the prefix "Marginal step" (e.g. a
continuous variable that did not converge, or an integer variable for
which SPRITE found only an approximate distribution). If no data could
be generated for a variable, `optim_mlr()` stops and names the variable
and the reason.

## Examples

``` r
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
  hill_climbs  = 50,
  max_starts   = 1,
  progress_mode = "off"
)
```
