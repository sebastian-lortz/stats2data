test_that("get_design maps model terms onto design columns", {
  cand <- matrix(1:8, ncol = 2, dimnames = list(NULL, c("X1", "X2")))
  d <- get_design(cand, "Y ~ X1 + X2", stats::terms(stats::as.formula("Y ~ X1 + X2")))
  expect_equal(d$target_names, c("(Intercept)", "X1", "X2"))
  expect_equal(unname(d$positions), c(1L, 2L, 3L))
  expect_equal(d$full_names, c("(Intercept)", "X1", "X2", "X1:X2"))
})

test_that("get_design resolves reversed interaction labels", {
  cand <- matrix(1:8, ncol = 2, dimnames = list(NULL, c("X1", "X2")))
  d <- get_design(cand, "Y ~ X2:X1", stats::terms(stats::as.formula("Y ~ X2:X1")))
  expect_equal(unname(d$positions), c(1L, 4L))
})

test_that("get_design errors on missing names and unknown terms", {
  expect_error(
    get_design(matrix(1:4, ncol = 2), "Y ~ X1",
               stats::terms(stats::as.formula("Y ~ X1"))),
    "column names"
  )
  cand <- matrix(1:8, ncol = 2, dimnames = list(NULL, c("X1", "X2")))
  expect_error(
    get_design(cand, "Y ~ Z", stats::terms(stats::as.formula("Y ~ Z"))),
    "Term not in design"
  )
})

test_that("heuristic_move_cont preserves the sum and the bounds", {
  set.seed(1)
  x <- c(2, 4, 6, 8)
  up <- heuristic_move_cont(x, target_sd = 5, range = c(0, 10))
  dn <- heuristic_move_cont(x, target_sd = 0.1, range = c(0, 10))
  expect_equal(sum(up), sum(x))
  expect_equal(sum(dn), sum(x))
  expect_true(all(up >= 0 & up <= 10))
  expect_true(all(dn >= 0 & dn <= 10))
})

test_that("heuristic_move_cont returns the candidate when no move is possible", {
  expect_equal(heuristic_move_cont(rep(0, 4), 1, c(0, 10)), rep(0, 4))
  expect_equal(heuristic_move_cont(rep(10, 4), 1, c(0, 10)), rep(10, 4))
})

test_that("sprite_start_vector_cont hits the target mean", {
  set.seed(3)
  v <- sprite_start_vector_cont(tMean = 3, n = 20, range = c(1, 5), thresh = 1e-4)
  expect_length(v, 20)
  expect_lt(abs(mean(v) - 3), 1e-3)
})

test_that("sprite_start_vector_cont validates its arguments", {
  expect_error(sprite_start_vector_cont(3, 10, c(5, 5), 0.01), "range")
  expect_error(sprite_start_vector_cont(9, 10, c(1, 5), 0.01), "outside")
  expect_error(sprite_start_vector_cont(3, 1, c(1, 5), 0.01), "n must be")
})

test_that("build_aov_structure builds a between-subject design", {
  s <- build_aov_structure(N = 8, levels = 2, factor_type = "between")
  expect_equal(s$N_subjects, 8)
  expect_equal(s$n_between_groups, 2)
  expect_equal(s$n_within_levels, 1)
  expect_true(s$has_between)
  expect_false(s$has_within)
  expect_equal(unname(s$n_per_bgroup), c(4, 4))
  expect_equal(nrow(s$df), 8)
})

test_that("build_aov_structure distributes an uneven N", {
  s <- build_aov_structure(N = 7, levels = 2, factor_type = "between")
  expect_equal(unname(s$n_per_bgroup), c(4, 3))
})

test_that("build_aov_structure honours subgroup_sizes", {
  s <- build_aov_structure(8, 2, "between", subgroup_sizes = c(3, 5))
  expect_equal(unname(s$n_per_bgroup), c(3, 5))
  expect_error(build_aov_structure(8, 2, "between", subgroup_sizes = c(3)),
               "subgroup_sizes length")
})

test_that("build_aov_structure builds within and mixed designs", {
  w <- build_aov_structure(4, 2, "within")
  expect_true(w$has_within)
  expect_false(w$has_between)
  expect_equal(nrow(w$df), 8)
  expect_equal(w$within_levels, 2)

  m <- build_aov_structure(4, c(2, 2), c("between", "within"))
  expect_true(m$has_between && m$has_within)
  expect_equal(nrow(m$df), 8)
  expect_equal(m$N_subjects, 4)
})

test_that("build_aov_structure validates levels against factor_type", {
  expect_error(build_aov_structure(8, c(2, 2), "between"), "same length")
})

test_that("move_between shifts a pair of subjects within bounds", {
  set.seed(4)
  s <- build_aov_structure(8, 2, "between")
  x <- rep(5, 8)
  expect_equal(sum(move_between(x, TRUE, s, c(0, 10))), sum(x))
  expect_equal(sum(move_between(x, FALSE, s, c(0, 10))), sum(x))
})

test_that("move_between returns the candidate when there is no headroom", {
  s <- build_aov_structure(8, 2, "between")
  expect_equal(move_between(rep(0, 8), TRUE, s, c(0, 10)), rep(0, 8))
  expect_equal(move_between(rep(0, 8), FALSE, s, c(0, 10)), rep(0, 8))
  small <- list(between_group_labels = "1", subjects_in_bgroup = list(`1` = "1"))
  expect_equal(move_between(rep(5, 2), TRUE, small, c(0, 10)), rep(5, 2))
})

test_that("move_within_stratum keeps the total constant", {
  set.seed(5)
  s <- build_aov_structure(4, 2, "within")
  x <- rep(5, 8)
  out <- move_within_stratum(x, 1L, TRUE, s, c(0, 10))
  expect_equal(sum(out), sum(x))
  out <- move_within_stratum(x, 1L, FALSE, s, c(0, 10))
  expect_equal(sum(out), sum(x))
})

test_that("move_within_stratum returns early on invalid strata", {
  b <- build_aov_structure(8, 2, "between")
  expect_equal(move_within_stratum(rep(5, 8), 1L, TRUE, b, c(0, 10)), rep(5, 8))
  s <- build_aov_structure(4, 2, "within")
  expect_equal(move_within_stratum(rep(5, 8), 2L, TRUE, s, c(0, 10)), rep(5, 8))
  one <- build_aov_structure(4, 1, "within")
  expect_equal(move_within_stratum(rep(5, 4), 1L, TRUE, one, c(0, 10)), rep(5, 4))
  expect_equal(move_within_stratum(rep(0, 8), 1L, TRUE, s, c(0, 10)), rep(0, 8))
  expect_equal(move_within_stratum(rep(0, 8), 1L, FALSE, s, c(0, 10)), rep(0, 8))
})

test_that("compute_MSE_between matches the manual computation", {
  s <- build_aov_structure(4, 2, "between")
  x <- c(1, 3, 6, 8)
  sm <- x
  ss <- sum((sm[1:2] - mean(sm[1:2]))^2) + sum((sm[3:4] - mean(sm[3:4]))^2)
  expect_equal(compute_MSE_between(x, s), 1 * ss / (4 - 2))
})

test_that("compute_numerator_MS works for between and within designs", {
  old <- options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
  on.exit(options(old), add = TRUE)

  b <- build_aov_structure(8, 2, "between")
  ms_b <- compute_numerator_MS(
    x = c(1, 2, 3, 4, 6, 7, 8, 9), ID = b$df$ID,
    factor_mat = b$df[, -1, drop = FALSE], structure = b,
    formula = stats::as.formula("outcome ~ Factor1"),
    effect_names = "Factor1"
  )
  expect_length(ms_b, 1)
  expect_true(is.finite(ms_b))

  skip_if_not_installed("afex")
  w <- build_aov_structure(4, 2, "within")
  ms_w <- suppressMessages(compute_numerator_MS(
    x = c(1, 4, 2, 5, 3, 6, 4, 8), ID = w$df$ID,
    factor_mat = w$df[, -1, drop = FALSE], structure = w,
    formula = stats::as.formula("outcome ~ Factor1 + Error(ID/Factor1)"),
    effect_names = "Factor1"
  ))
  expect_length(ms_w, 1)
  expect_true(is.finite(ms_w))
})

test_that("compute_numerator_MS errors on unknown effects", {
  b <- build_aov_structure(8, 2, "between")
  expect_error(
    compute_numerator_MS(
      x = c(1, 2, 3, 4, 6, 7, 8, 9), ID = b$df$ID,
      factor_mat = b$df[, -1, drop = FALSE], structure = b,
      formula = stats::as.formula("outcome ~ Factor1"),
      effect_names = "Factor9"
    ),
    "not found"
  )
})

test_that("aov_targets derives consistent means and MS", {
  old <- options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
  on.exit(options(old), add = TRUE)
  s <- build_aov_structure(8, 2, "between")
  fm <- s$df[, -1, drop = FALSE]
  gid <- apply(fm, 1, paste0, collapse = "")
  gidx <- split(seq_along(gid), gid)
  args <- list(
    target_group_means = c(3, 5), target_F = 4,
    group_sizes = vapply(gidx, length, integer(1)),
    effect_names = "Factor1", levels = 2, factor_type = "between",
    thresh = 5e-3, ID = s$df$ID, factor_mat = fm, group_idx = gidx,
    structure = s, formula = stats::as.formula("outcome ~ Factor1")
  )

  int <- do.call(aov_targets, c(args, list(integer = TRUE)))
  expect_length(int$adjusted_means, 2)
  expect_named(int$numerator_MS, "Factor1")
  expect_true(is.finite(int$numerator_MS))
  expect_equal(int$adjusted_F, 4)
  expect_equal(unname(int$between_effect_idx), 1L)
  expect_length(int$within_effect_idx, 0)

  cont <- do.call(aov_targets, c(args, list(integer = FALSE)))
  expect_length(cont$adjusted_means, 2)
  expect_true(all(abs(cont$adjusted_means - c(3, 5)) <= 5e-3 + 1e-8))
})

test_that("aov_targets warns when no integer mean is within the threshold", {
  old <- options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
  on.exit(options(old), add = TRUE)
  s <- build_aov_structure(8, 2, "between")
  fm <- s$df[, -1, drop = FALSE]
  gid <- apply(fm, 1, paste0, collapse = "")
  gidx <- split(seq_along(gid), gid)
  expect_warning(expect_warning(
    aov_targets(
      target_group_means = c(3.1, 5.1), target_F = 4,
      group_sizes = vapply(gidx, length, integer(1)),
      effect_names = "Factor1", levels = 2, factor_type = "between",
      integer = TRUE, thresh = 1e-9, ID = s$df$ID, factor_mat = fm,
      group_idx = gidx, structure = s,
      formula = stats::as.formula("outcome ~ Factor1")
    ), "no integer mean"),
    "no integer mean"
  )
})

test_that("remap_target_cor reorders the correlation vector", {
  d <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  expect_equal(remap_target_cor(c(0.1, 0.2, 0.3), d, c("A", "B", "C")),
               c(0.1, 0.2, 0.3))
  expect_equal(remap_target_cor(c(0.1, 0.2, 0.3), d, c("B", "A", "C")),
               c(0.1, 0.3, 0.2))
})

test_that("remap_target_cor validates lengths and names", {
  d <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  expect_error(remap_target_cor(c(0.1, 0.2), d, c("A", "B", "C")),
               "does not match")
  expect_error(remap_target_cor(c(0.1, 0.2, 0.3), d, c("A", "B", "Z")),
               "not found")
})

test_that("n_decimals counts the decimals of each value", {
  expect_identical(n_decimals(c(10.53, 0.27, 3, 11.896, NA, 1e-4)),
                   c(2L, 2L, 0L, 3L, NA, 4L))
})

test_that("check_sprite_prec stops when sprite_prec has fewer decimals than the targets", {
  expect_error(
    check_sprite_prec(c(A = 10.537), c(A = 7.82), TRUE, c(0, 36), c(2, 2)),
    "set `sprite_prec = c\\(3, 2\\)`"
  )
  expect_error(
    check_sprite_prec(c(A = 10.5, B = 4.123), c(A = 7.825, B = 1), TRUE, c(0, 36), c(2, 2)),
    "mean of 'B' \\(4.123\\) and the SD of 'A' \\(7.825\\)"
  )
  # continuous variables, binary variables and fewer decimals are fine
  expect_true(check_sprite_prec(c(A = 10.537), c(A = 7.82), FALSE, c(0, 36), c(2, 2)))
  expect_true(check_sprite_prec(c(B = 0.274), c(B = 0.45), TRUE, c(0, 1), c(2, 2)))
  expect_true(check_sprite_prec(c(A = 10.5), c(A = 7.8), TRUE, c(0, 36), c(2, 2)))
  expect_true(check_sprite_prec(c(A = 10.5), c(A = 7.8), TRUE, c(0, 36), NULL))
})

test_that("optim_vec and optim_mlr check sprite_prec before running", {
  expect_error(
    optim_vec(N = 50, target_mean = c(A = 10.537), target_sd = c(A = 7.82),
              range = c(0, 36), integer = TRUE, sprite_prec = c(2, 2), progress_mode = "off"),
    "fewer decimals"
  )
  expect_error(
    optim_mlr(N = 50, target_mean = c(A = 10.537, Y = 5), target_sd = c(A = 7.82, Y = 2),
              range = c(0, 36), integer = c(TRUE, FALSE), target_cor = 0.3,
              target_reg = c(1, 0.4), reg_equation = "Y ~ A", progress_mode = "off"),
    "fewer decimals"
  )
})

test_that("optim_vec warns about variables without data, with the GRIM hint", {
  expect_warning(
    res <- optim_vec(N = 50, target_mean = c(A = 10.53), target_sd = c(A = 7.82),
                     range = c(0, 36), integer = TRUE, sprite_prec = c(2, 2),
                     progress_mode = "off"),
    "no data generated.*GRIM test.*10.52"
  )
  expect_true(all(is.na(res$data$A)))
  expect_match(res$error_msgs[[1]], "GRIM test")
})

test_that("optim_vec warns when SPRITE returns only the closest distribution", {
  expect_warning(
    optim_vec(N = 12, target_mean = c(A = 3), target_sd = c(A = 1), range = c(1, 5),
              integer = TRUE, sprite_prec = c(2, 2), progress_mode = "off"),
    "closest one found is used"
  )
})

test_that("optim_vec warns about continuous variables that did not converge", {
  set.seed(1)
  expect_warning(
    optim_vec(N = 50, target_mean = c(C = 10), target_sd = c(C = 3), range = c(0, 36),
              integer = FALSE, max_iter = 20, max_starts = 1, progress_mode = "off"),
    "Variable 'C' did not converge.*increasing max_iter"
  )
  expect_warning(
    optim_vec(N = 50, target_mean = c(C = 5), target_sd = c(C = 20), range = c(0, 10),
              integer = FALSE, max_iter = 20, max_starts = 1, progress_mode = "off"),
    "exceeds the largest SD attainable"
  )
})

test_that("optim_mlr reports the marginal step", {
  mlr_base <- list(N = 50, target_sd = c(A = 7.82, Y = 2), range = c(0, 36),
                   integer = c(TRUE, FALSE), target_cor = 0.3, target_reg = c(1, 0.4),
                   reg_equation = "Y ~ A", max_iter = 50, hill_climbs = 0,
                   progress_mode = "off")
  # no data for a marginal: stop with the variable and the reason
  expect_error(
    do.call(optim_mlr, c(mlr_base, list(target_mean = c(A = 10.53, Y = 5)))),
    "Marginal step \\(optim_vec\\): no data generated for 'A'.*GRIM test"
  )
  # a marginal that did not converge: warning with the marginal-step prefix
  set.seed(2)
  expect_warning(
    optim_mlr(N = 50, target_mean = c(A = 5, Y = 5), target_sd = c(A = 20, Y = 2),
              range = c(0, 10), integer = FALSE, target_cor = 0.3, target_reg = c(1, 0.4),
              reg_equation = "Y ~ A", sprite_prec = NULL, max_iter = 50, hill_climbs = 0,
              progress_mode = "off"),
    "Marginal step \\(optim_vec\\): Variable 'A' did not converge"
  )
})
