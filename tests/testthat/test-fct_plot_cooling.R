test_that(".plot_cooling_engine builds a decay curve", {
  p <- .plot_cooling_engine(max_iter = 10, init_temp = 1, cooling_rate = 0.5)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 10)
  expect_equal(p$data$Temperature[1], 0.5)
})

test_that(".plot_cooling_engine validates the schedule", {
  expect_error(.plot_cooling_engine(0, 1, 0.5), "positive integer")
  expect_error(.plot_cooling_engine(10, 0, 0.5), "positive number")
  expect_error(.plot_cooling_engine(10, 1, 1), "in \\(0, 1\\)")
  expect_error(.plot_cooling_engine(10, 1, 0), "in \\(0, 1\\)")
})

test_that(".validate_cooling_inputs reports missing parameters", {
  expect_null(.validate_cooling_inputs(
    list(max_iter = 1, init_temp = 1, cooling_rate = 0.5)
  ))
  expect_error(.validate_cooling_inputs(list()),
               "max_iter, init_temp, cooling_rate")
})

test_that("plot_cooling methods dispatch for every module", {
  expect_match(quiet_plot(plot_cooling(fx_vec()))$labels$title, "Descriptives")
  expect_match(quiet_plot(plot_cooling(fx_mlr()))$labels$title, "MLR")
  expect_match(quiet_plot(plot_cooling(fx_aov()))$labels$title, "ANOVA")
})

test_that("plot_cooling errors when the schedule is incomplete", {
  x <- fx_vec()
  x$inputs$init_temp <- NULL
  expect_error(plot_cooling(x), "init_temp")
})
