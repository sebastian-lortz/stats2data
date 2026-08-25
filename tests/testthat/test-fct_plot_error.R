test_that(".plot_error_engine builds a trajectory plot", {
  p <- .plot_error_engine(c(3, 2, 1), first_iter = 1L)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2)
  expect_equal(p$data$Iteration, c(2, 3))
  expect_s3_class(.plot_error_engine(c(3, 2, 1), show_best = FALSE), "ggplot")
})

test_that(".plot_error_engine validates the error vector", {
  expect_error(.plot_error_engine(numeric(0)), "empty")
  expect_error(.plot_error_engine(c(1, 2), first_iter = -1), "non-negative integer")
  expect_error(.plot_error_engine(c(1, 2), first_iter = 1.5), "non-negative integer")
  expect_error(.plot_error_engine(c(1, 2), first_iter = 5), "must be smaller")
})

test_that(".plot_error_ratio_engine builds a ratio plot", {
  p <- .plot_error_ratio_engine(c(3, 2, 1), first_iter = 0L)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
  expect_s3_class(
    .plot_error_ratio_engine(c(3, 2, 1), show_mean = FALSE, show_median = FALSE,
                             show_final = FALSE),
    "ggplot"
  )
})

test_that(".plot_error_ratio_engine validates the ratio vector", {
  expect_error(.plot_error_ratio_engine(numeric(0)), "empty")
  expect_error(.plot_error_ratio_engine(c(1, 2), first_iter = -1),
               "non-negative integer")
  expect_error(.plot_error_ratio_engine(c(1, 2), first_iter = 2), "must be smaller")
})

test_that(".extract_error_vec selects the requested run", {
  expect_equal(.extract_error_vec(list(a = 1:3, b = 4:6), run = 2), 4:6)
  expect_equal(.extract_error_vec(1:3), 1:3)
  expect_error(.extract_error_vec(list(1:3), run = 0), "positive integer")
  expect_error(.extract_error_vec(list(1:3), run = 2), "out of bounds")
  expect_error(.extract_error_vec(list(1:3, 4:6), run = 3), "are only 2 runs")
  expect_error(.extract_error_vec(1:3, run = 2), "Only 1 run")
})

test_that("plot_error.stats2data_vec labels the variable", {
  p <- quiet_plot(plot_error(fx_vec()))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "V1")
  x <- fx_vec()
  names(x$track_error) <- NULL
  expect_match(quiet_plot(plot_error(x, run = 2))$labels$title, "V2")
})

test_that("plot_error.stats2data_vec requires track_error", {
  x <- fx_vec()
  x$track_error <- NULL
  expect_error(plot_error(x), "No .track_error. element")
})

test_that("plot_error.stats2data_aov plots the trajectory", {
  expect_s3_class(quiet_plot(plot_error(fx_aov())), "ggplot")
  x <- fx_aov()
  x$track_error <- NULL
  expect_error(plot_error(x), "No .track_error. element")
})

test_that("plot_error.stats2data_mlr plots errors and ratios", {
  expect_s3_class(quiet_plot(plot_error(fx_mlr())), "ggplot")
  p <- quiet_plot(plot_error(fx_mlr(), ratio = TRUE))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Error Ratio")
})

test_that("plot_error.stats2data_mlr validates its flags", {
  x <- fx_mlr()
  expect_error(plot_error(x, ratio = "yes"), "single logical")
  expect_error(plot_error(x, ratio = TRUE, show_mean = "yes"), "single logical")
  expect_error(plot_error(x, ratio = TRUE, show_median = "yes"), "single logical")
  expect_error(plot_error(x, ratio = TRUE, show_final = "yes"), "single logical")
  x$track_error_ratio <- NULL
  expect_error(plot_error(x, ratio = TRUE), "No .track_error_ratio. element")
  y <- fx_mlr()
  y$track_error <- NULL
  expect_error(plot_error(y), "No .track_error. element")
})
