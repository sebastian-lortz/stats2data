test_that("get_stats.stats2data_parallel aggregates numeric components", {
  s <- get_stats(fx_parallel("vec"))
  expect_named(s, c("mean", "sd"))
  expect_named(s$mean, c("mean", "median", "sd", "min", "max"))
  expect_equal(nrow(s$mean), 2)
  expect_equal(s$mean$mean, c(3.75, 3.25))
})

test_that("get_rmse.stats2data_parallel summarises the vec module", {
  r <- get_rmse(fx_parallel("vec"))
  expect_named(r, c("between_rmse", "target_rmse", "raw"))
  expect_equal(r$between_rmse$metric, c("rmse_mean", "rmse_sd"))
  expect_true(all(is.finite(r$target_rmse$mean)))
  expect_named(r$raw, c("between", "target"))
})

test_that("get_rmse.stats2data_parallel summarises the mlr module", {
  r <- get_rmse(fx_parallel("mlr"))
  expect_true(all(c("rmse_cor", "rmse_reg", "rmse_se") %in% r$between_rmse$metric))
  r0 <- get_rmse(fx_parallel("mlr", se = FALSE))
  expect_false("rmse_se" %in% r0$between_rmse$metric)
})

test_that("get_rmse.stats2data_parallel summarises the aov module", {
  skip_if_not_installed("afex")
  r <- suppressMessages(get_rmse(fx_parallel("aov")))
  expect_equal(r$between_rmse$metric, "rmse_F")
  expect_length(r$raw$target$rmse_F, 2)
})

test_that(".summarise_rmse returns an empty frame when everything is NA", {
  out <- .summarise_rmse(list(a = c(NA_real_, NA_real_)))
  expect_equal(nrow(out), 0)
  expect_named(out, c("metric", "mean", "sd", "min", "max"))
})

test_that("summary.stats2data_parallel aggregates errors", {
  s <- summary(fx_parallel("vec"))
  expect_s3_class(s, "summary.stats2data_parallel")
  expect_equal(s$module, "vec")
  expect_length(s$errors, 2)
  expect_output(print(s), "stats2data Parallel Summary")
  expect_output(print(s), "Between-run RMSE")
})

test_that("plot.stats2data_parallel returns a ggplot", {
  p <- quiet_plot(plot(fx_parallel("vec")))
  expect_s3_class(p, "ggplot")
  expect_true(all(c("Metric", "Type", "RMSE") %in% names(p$data)))
})

test_that("plot.stats2data_parallel drops all-NA metrics", {
  p <- quiet_plot(plot(fx_parallel("mlr", se = FALSE)))
  expect_s3_class(p, "ggplot")
  expect_false("SE" %in% levels(p$data$Metric))
  skip_if_not_installed("afex")
  expect_s3_class(suppressMessages(quiet_plot(plot(fx_parallel("aov")))), "ggplot")
})

test_that("plot_fit.stats2data_parallel builds the lollipop chart", {
  p <- quiet_plot(plot_fit(fx_parallel("vec")))
  expect_s3_class(p, "ggplot")
  expect_true(all(c("Lower", "Upper", "Centered") %in% names(p$data)))
  expect_s3_class(quiet_plot(plot_fit(fx_parallel("vec"), error_type = "sd")),
                  "ggplot")
  expect_s3_class(quiet_plot(plot_fit(fx_parallel("vec"), standardised = FALSE)),
                  "ggplot")
})

test_that("plot_fit.stats2data_parallel supports mlr and aov", {
  expect_s3_class(quiet_plot(plot_fit(fx_parallel("mlr"))), "ggplot")
  expect_s3_class(quiet_plot(plot_fit(fx_parallel("mlr", se = FALSE))), "ggplot")
  skip_if_not_installed("afex")
  expect_s3_class(suppressMessages(quiet_plot(plot_fit(fx_parallel("aov")))),
                  "ggplot")
})

test_that("plot_fit.stats2data_parallel validates its arguments", {
  x <- fx_parallel("vec")
  expect_error(plot_fit(x, standardised = "yes"), "single logical")
  expect_error(plot_fit(x, eps = "a"), "single numeric")
  expect_error(plot_fit(x, error_type = "iqr"), "arg")
  y <- x
  y$module <- "zzz"
  expect_error(plot_fit(y), "Unrecognised module")
})

test_that(".parallel_sim_block coerces non-finite summaries to NA", {
  b <- suppressWarnings(
    .parallel_sim_block(matrix(c(NA_real_, 1, NA_real_, 3), nrow = 2), "range")
  )
  expect_true(is.na(b$sim[1]))
  expect_true(is.na(b$lo[1]))
  expect_equal(b$sim[2], 2)
})

test_that(".centered_bounds orders the bounds for negative targets", {
  b <- .centered_bounds(1, 3, -2, TRUE, 1e-12)
  expect_true(all(b$Lower <= b$Upper))
})
