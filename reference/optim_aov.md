# Optimize simulated data to match ANOVA F-values

Uses simulated annealing to generate raw data that reproduce target
ANOVA F-statistics and group means under a specified factorial design.

## Usage

``` r
optim_aov(
  S,
  levels,
  target_group_means,
  target_f_list,
  integer,
  range,
  formula,
  factor_type,
  subgroup_sizes = NULL,
  thresh = 0.005,
  max_iter = 10000,
  init_temp = 0.001,
  cooling_rate = NULL,
  max_starts = 3,
  progress_mode = "console"
)
```

## Arguments

- S:

  Integer. Total number of subjects.

- levels:

  Integer vector. Number of levels per factor.

- target_group_means:

  Numeric vector of length `prod(levels)`. Target cell means in
  `expand.grid` order.

- target_f_list:

  List with components `effect` (character) and `F` (numeric) of equal
  length.

- integer:

  Logical. Generate integer-valued data?

- range:

  Numeric vector of length 2. Bounds for individual observations.

- formula:

  Formula or character. ANOVA model formula.

- factor_type:

  Character vector (`"between"`/`"within"`) matching length of `levels`.

- subgroup_sizes:

  Optional numeric vector of between-group sizes (must sum to `N`).

- thresh:

  Numeric. Convergence threshold. Default `1e-2`.

- max_iter:

  Integer. Iterations per restart. Default `1e3`.

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
`inputs`, `adjusted_targets`, and `track_error`.
