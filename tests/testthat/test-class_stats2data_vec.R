test_that("new_s2d_vec validates its arguments", {
  expect_error(new_s2d_vec(list(1), 1:3, list(), list()), "must be a data.frame")
  expect_error(new_s2d_vec(list(1), data.frame(a = 1), 1, list()),
               "must be a list")
})

test_that("print.stats2data_vec reports the design and the errors", {
  x <- fx_vec()
  expect_output(expect_invisible(print(x)), "stats2data Descriptives result")
  expect_output(print(x), "V1")
})

test_that("print.stats2data_vec tolerates non-finite errors", {
  x <- fx_vec()
  x$best_objective <- list(Inf, "a")
  expect_output(print(x), "NA")
})

test_that("get_stats.stats2data_vec returns means and sds", {
  s <- get_stats(fx_vec())
  expect_named(s, c("mean", "sd"))
  expect_equal(unname(s$mean), c(3.5, 3))
  expect_equal(unname(round(s$sd, 3)), c(1.871, 0.894))
})

test_that("get_rmse.stats2data_vec compares against the targets", {
  r <- get_rmse(fx_vec())
  expect_named(r, c("rmse_mean", "rmse_sd"))
  expect_equal(r$rmse_mean, 0)
  expect_lt(r$rmse_sd, 0.01)
})

test_that("summary.stats2data_vec builds the comparison table", {
  s <- summary(fx_vec())
  expect_s3_class(s, "summary.stats2data_vec")
  expect_equal(s$N, 6)
  expect_equal(s$n_var, 2)
  expect_equal(nrow(s$comparison), 2)
  expect_named(s$comparison,
               c("variable", "target_mean", "sim_mean", "target_sd", "sim_sd"))
  expect_output(print(s), "stats2data Descriptives Summary")
  expect_output(print(s), "RMSE")
})
