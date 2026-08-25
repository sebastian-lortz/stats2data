quiet_plot <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

with_mock_session <- function(expr) {
  shiny::withReactiveDomain(shiny::MockShinySession$new(), expr)
}

fx_vec <- function(shift = 0) {
  d <- data.frame(
    V1 = c(1, 2, 3, 4, 5, 6) + shift,
    V2 = c(2, 2, 3, 3, 4, 4) + shift
  )
  new_s2d_vec(
    best_error = list(0.01, 0.02),
    data = d,
    inputs = list(
      N = 6,
      target_mean = c(V1 = 3.5, V2 = 3),
      target_sd = c(V1 = 1.87, V2 = 0.89),
      range = matrix(c(0, 10, 0, 10), nrow = 2),
      integer = c(FALSE, FALSE), thresh = 0.05, sprite_prec = c(2, 2),
      max_iter = 20, init_temp = 0.1, cooling_rate = 0.9,
      max_starts = 1, progress_mode = "off"
    ),
    track_error = list(V1 = c(1, 0.5, 0.1), V2 = c(2, 0.4, 0.2))
  )
}

fx_mlr <- function(shift = 0, se = TRUE) {
  d <- data.frame(
    X1 = c(1, 2, 3, 4, 5, 6),
    X2 = c(2, 1, 4, 3, 6, 5),
    Y  = c(3, 4, 6, 6, 9, 10) + shift
  )
  new_s2d_mlr(
    best_error = 0.02,
    data = d,
    optim_vec = fx_vec(),
    inputs = list(
      N = 6,
      target_mean = c(X1 = 3.5, X2 = 3.5, Y = 6.33),
      target_sd = c(X1 = 1.87, X2 = 1.87, Y = 2.66),
      range = matrix(c(0, 10, 0, 10, 0, 20), nrow = 2),
      integer = FALSE, sprite_prec = c(2, 2),
      target_cor = c(0.6, 0.9, 0.8),
      target_reg = c("(Intercept)" = 0.5, X1 = 1, X2 = 0.4),
      reg_equation = "Y ~ X1 + X2",
      target_se = if (se) c(0.3, 0.1, 0.1) else NULL,
      weight = c(0.5, 0.5), thresh = 0.05, max_iter = 20,
      init_temp = 0.1, cooling_rate = 0.9, max_starts = 1,
      hill_climbs = 5, progress_mode = "off"
    ),
    track_error = c(1, 0.5, 0.02),
    track_error_ratio = c(2, 1.5, 1.2)
  )
}

fx_aov <- function(shift = 0) {
  d <- data.frame(
    ID = factor(rep(1:6, each = 2)),
    Factor1 = factor(rep(1:2, times = 6)),
    outcome = c(3, 5, 4, 6, 2, 5, 3, 7, 4, 6, 5, 8) + shift
  )
  new_s2d_aov(
    best_error = 0.03,
    data = d,
    inputs = list(
      N = 6, levels = 2, target_group_means = c(3.5, 6.17),
      target_f_list = list(effect = "Factor1", F_value = 20),
      integer = FALSE, range = c(0, 10),
      formula = "~ Factor1 + Error(ID/Factor1)",
      factor_type = "within", subgroup_sizes = NULL,
      thresh = 0.05, max_iter = 20, init_temp = 0.1,
      cooling_rate = 0.9, max_starts = 1, progress_mode = "off"
    ),
    adjusted_targets = list(group_means = c(3.5, 6.17), F_values = 20),
    track_error = c(1, 0.5, 0.03)
  )
}

fx_parallel <- function(module = "vec", runs = 2, ...) {
  gen <- switch(module, vec = fx_vec, mlr = fx_mlr, aov = fx_aov)
  res <- lapply(seq_len(runs), function(i) gen(shift = (i - 1) * 0.5, ...))
  structure(
    list(results = res, best = res[[1L]], module = module,
         runs = runs, runs_requested = runs, converged = FALSE,
         thresh = NULL),
    class = "stats2data_parallel"
  )
}
