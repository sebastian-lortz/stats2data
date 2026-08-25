test_that("new_s2d_mlr validates its arguments", {
  expect_error(new_s2d_mlr(1, 1:3, fx_vec(), list(), 1, 1), "must be a data.frame")
  expect_error(new_s2d_mlr(1, data.frame(a = 1), list(), list(), 1, 1),
               "stats2data_vec")
  expect_error(new_s2d_mlr(1, data.frame(a = 1), fx_vec(), 1, 1, 1),
               "must be a list")
})

test_that("print.stats2data_mlr reports the model", {
  expect_output(expect_invisible(print(fx_mlr())), "stats2data MLR result")
  expect_output(print(fx_mlr()), "Y ~ X1 \\+ X2")
})

test_that("get_stats.stats2data_mlr returns model quantities", {
  s <- get_stats(fx_mlr())
  expect_named(s, c("model", "reg", "se", "cor", "mean", "sd"))
  expect_s3_class(s$model, "lm")
  expect_length(s$reg, 3)
  expect_length(s$se, 3)
  expect_length(s$cor, 3)
  expect_equal(unname(s$mean[1]), 3.5)
})

test_that("get_rmse.stats2data_mlr handles present and absent target_se", {
  r <- get_rmse(fx_mlr())
  expect_named(r, c("rmse_cor", "rmse_reg", "rmse_se"))
  expect_true(all(vapply(r, is.finite, logical(1))))
  expect_true(is.na(get_rmse(fx_mlr(se = FALSE))$rmse_se))
})

test_that("get_rmse.stats2data_mlr ignores NA targets", {
  x <- fx_mlr()
  x$inputs$target_cor <- c(0.6, NA, 0.8)
  expect_true(is.finite(get_rmse(x)$rmse_cor))
})

test_that("coef.stats2data_mlr refits the stored equation", {
  x <- fx_mlr()
  expect_equal(coef(x), stats::coef(stats::lm(Y ~ X1 + X2, data = x$data)))
})

test_that("summary.stats2data_mlr builds all comparison tables", {
  s <- summary(fx_mlr())
  expect_s3_class(s, "summary.stats2data_mlr")
  expect_equal(nrow(s$descriptives), 3)
  expect_equal(nrow(s$coefficients), 3)
  expect_equal(nrow(s$correlations), 3)
  expect_equal(s$correlations$pair, c("X1-X2", "X1-Y", "X2-Y"))
  expect_true(all(c("target_se", "sim_se") %in% names(s$coefficients)))
  expect_output(print(s), "stats2data MLR Summary")
  expect_output(print(s), "Correlations")
})

test_that("summary.stats2data_mlr omits standard errors when absent", {
  s <- summary(fx_mlr(se = FALSE))
  expect_false("target_se" %in% names(s$coefficients))
  expect_output(print(s), "Coefficients")
})
