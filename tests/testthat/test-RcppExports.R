cand_mat <- matrix(c(1, 2, 3, 4, 5, 6, 2, 1, 4, 3, 6, 5), ncol = 2,
                   dimnames = list(NULL, c("X1", "X2")))
outcome_vec <- c(3, 4, 6, 6, 9, 10)
reg_df <- data.frame(X1 = cand_mat[, 1], X2 = cand_mat[, 2], Y = outcome_vec)
pos_main <- get_design(cand_mat, "Y ~ X1 + X2",
                       stats::terms(stats::as.formula("Y ~ X1 + X2")))$positions
pos_inter <- get_design(cand_mat, "Y ~ X1 + X2 + X1:X2",
                        stats::terms(stats::as.formula("Y ~ X1 + X2 + X1:X2")))$positions
rmse_finite <- function(sim, target) {
  keep <- is.finite(target)
  sqrt(mean((sim[keep] - target[keep])^2))
}

test_that("objective_cpp returns the absolute sample-sd deviation", {
  x <- c(1, 2, 3, 4, 5, 6)
  expect_equal(objective_cpp(x, 2), abs(stats::sd(x) - 2))
  expect_equal(objective_cpp(x, stats::sd(x)), 0)
  expect_gte(objective_cpp(x, 0), 0)
})

test_that("objective_cpp returns NA for fewer than two values", {
  expect_true(is.na(objective_cpp(1, 2)))
  expect_true(is.na(objective_cpp(numeric(0), 2)))
})

test_that("candidate_cor_cpp matches the upper triangle of cor()", {
  full <- cbind(cand_mat, outcome_vec)
  expected <- stats::cor(full)[upper.tri(stats::cor(full))]
  out <- as.numeric(candidate_cor_cpp(cand_mat, outcome_vec))
  expect_length(out, 3)
  expect_equal(out, expected)
})

test_that("candidate_cor_cpp orders the pairs column-major", {
  out <- as.numeric(candidate_cor_cpp(cand_mat, outcome_vec))
  expect_equal(out[1], stats::cor(cand_mat[, 1], cand_mat[, 2]))
  expect_equal(out[2], stats::cor(cand_mat[, 1], outcome_vec))
  expect_equal(out[3], stats::cor(cand_mat[, 2], outcome_vec))
})

test_that("candidate_reg_cpp reproduces the OLS coefficients", {
  expected <- unname(stats::coef(stats::lm(Y ~ X1 + X2, data = reg_df)))
  out <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, pos_main))
  expect_length(out, 3)
  expect_equal(out, expected)
})

test_that("candidate_reg_cpp builds the interaction column", {
  expected <- unname(stats::coef(stats::lm(Y ~ X1 * X2, data = reg_df)))
  out <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, pos_inter))
  expect_length(out, 4)
  expect_equal(out, expected)
})

test_that("candidate_reg_cpp honours the position subset", {
  expected <- unname(stats::coef(stats::lm(Y ~ X1, data = reg_df)))
  out <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, c(1L, 2L)))
  expect_equal(out, expected)
})

test_that("candidate_reg_cpp_se returns coefficients and standard errors", {
  fit <- summary(stats::lm(Y ~ X1 + X2, data = reg_df))$coefficients
  out <- candidate_reg_cpp_se(cand_mat, outcome_vec, pos_main)
  expect_equal(dim(out), c(3L, 2L))
  expect_equal(out[, 1], unname(fit[, "Estimate"]))
  expect_equal(out[, 2], unname(fit[, "Std. Error"]))
})

test_that("candidate_reg_cpp_se rejects out-of-range positions", {
  expect_error(candidate_reg_cpp_se(cand_mat, outcome_vec, c(1L, 99L)),
               "out of range")
})

test_that("error_function_cpp weights the two rmse components", {
  tc <- c(0.6, 0.9, 0.8)
  tr <- c(0.5, 1, 0.4)
  w <- c(0.3, 0.7)
  sim_cor <- as.numeric(candidate_cor_cpp(cand_mat, outcome_vec))
  sim_reg <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, pos_main))
  cor_err <- rmse_finite(sim_cor, tc)
  reg_err <- rmse_finite(sim_reg, tr)

  out <- error_function_cpp(cand_mat, outcome_vec, tc, tr, w, pos_main)
  expect_named(out, c("total_error", "error_ratio"))
  expect_equal(out$total_error, cor_err * w[1] + reg_err * w[2])
  expect_equal(out$error_ratio, cor_err / reg_err)
})

test_that("error_function_cpp skips non-finite targets", {
  tc <- c(0.6, NA, 0.8)
  tr <- c(NA, 1, 0.4)
  w <- c(0.5, 0.5)
  sim_cor <- as.numeric(candidate_cor_cpp(cand_mat, outcome_vec))
  sim_reg <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, pos_main))
  out <- error_function_cpp(cand_mat, outcome_vec, tc, tr, w, pos_main)
  expect_equal(
    out$total_error,
    rmse_finite(sim_cor, tc) * 0.5 + rmse_finite(sim_reg, tr) * 0.5
  )
})

test_that("error_function_cpp returns an infinite ratio at a perfect fit", {
  tr <- as.numeric(candidate_reg_cpp(cand_mat, outcome_vec, pos_main))
  out <- error_function_cpp(cand_mat, outcome_vec, c(0.6, 0.9, 0.8), tr,
                            c(0.5, 0.5), pos_main)
  expect_true(is.infinite(out$error_ratio))
  expect_gt(out$error_ratio, 0)
})

test_that("error_function_cpp_se is on the same scale as error_function_cpp", {
  tc <- c(0.6, 0.9, 0.8)
  trse <- cbind(c(0.5, 1, 0.4), c(0.3, 0.1, 0.1))
  w <- c(0.4, 0.6)
  sim_cor <- as.numeric(candidate_cor_cpp(cand_mat, outcome_vec))
  sim_rse <- candidate_reg_cpp_se(cand_mat, outcome_vec, pos_main)
  cor_err <- rmse_finite(sim_cor, tc)
  reg_err <- sqrt(mean((as.numeric(sim_rse) - as.numeric(trse))^2))

  out <- error_function_cpp_se(cand_mat, outcome_vec, tc, trse, w, pos_main)
  expect_named(out, c("total_error", "error_ratio"))
  expect_equal(out$total_error, cor_err * w[1] + reg_err * w[2])
  expect_equal(out$error_ratio, cor_err / reg_err)
})

test_that("error_function_cpp_se skips non-finite targets", {
  tc <- c(0.6, 0.9, 0.8)
  trse <- cbind(c(0.5, NA, 0.4), c(0.3, 0.1, NA))
  sim_rse <- as.numeric(candidate_reg_cpp_se(cand_mat, outcome_vec, pos_main))
  keep <- is.finite(as.numeric(trse))
  reg_err <- sqrt(mean((sim_rse[keep] - as.numeric(trse)[keep])^2))
  out <- error_function_cpp_se(cand_mat, outcome_vec, tc, trse, c(1, 1), pos_main)
  cor_err <- rmse_finite(as.numeric(candidate_cor_cpp(cand_mat, outcome_vec)), tc)
  expect_equal(out$total_error, cor_err + reg_err)
})

test_that("ols_from_design solves a model matrix", {
  X <- stats::model.matrix(Y ~ X1 + X2, data = reg_df)
  expected <- unname(stats::coef(stats::lm(Y ~ X1 + X2, data = reg_df)))
  expect_equal(as.numeric(ols_from_design(X, outcome_vec)), expected)
})

test_that("ols_from_design handles interaction designs", {
  X <- stats::model.matrix(Y ~ X1 * X2, data = reg_df)
  expected <- unname(stats::coef(stats::lm(Y ~ X1 * X2, data = reg_df)))
  expect_equal(as.numeric(ols_from_design(X, outcome_vec)), expected)
})
