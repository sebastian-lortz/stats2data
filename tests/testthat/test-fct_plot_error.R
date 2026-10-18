test_that(".step_points keeps only the change points and the last index", {
  expect_equal(.step_points(c(5, 5, 5, 3, 3, 1)), c(1L, 4L, 6L))
  expect_equal(.step_points(c(2, 2, 2)), c(1L, 3L))
  expect_equal(.step_points(c(3, 2, 1)), c(1L, 2L, 3L))
  expect_equal(.step_points(5), 1L)
  expect_equal(.step_points(c(1, NA, 1)), c(1L, 2L, 3L))
})

test_that(".plot_convergence_engine plots one point per distinct error level", {
  err <- c(9, rep(5, 50), rep(2, 49))
  p <- .plot_convergence_engine(err, first_iter = 0L)
  expect_equal(nrow(p$data), 4)
  expect_equal(p$data$Iteration, c(1, 2, 52, 100))
  expect_equal(p$data$Error, c(9, 5, 2, 2))
})

test_that(".plot_convergence_engine supports a log y axis", {
  p <- .plot_convergence_engine(c(3, 2, 1), first_iter = 0L, log_y = TRUE)
  expect_true(any(vapply(p$scales$scales,
                         function(s) identical(s$trans$name, "log-10"),
                         logical(1))))
  expect_warning(.plot_convergence_engine(c(3, 2, 0), first_iter = 0L, log_y = TRUE),
                 "could not be plotted")
  expect_error(
    suppressWarnings(.plot_convergence_engine(c(0, 0), first_iter = 0L, log_y = TRUE)),
    "No plottable error values"
  )
  expect_error(.plot_convergence_engine(c(1, 2), log_y = "yes"), "single logical")
})

test_that(".plot_convergence_ratio_engine deduplicates constant stretches", {
  p <- .plot_convergence_ratio_engine(c(4, rep(7, 8), 3), first_iter = 0L)
  expect_equal(p$data$Iteration, c(1, 2, 10))
  expect_equal(p$data$ErrorRatio, c(4, 7, 3))
})

test_that(".plot_convergence_engine builds a trajectory plot", {
  p <- .plot_convergence_engine(c(3, 2, 1), first_iter = 1L)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 2)
  expect_equal(p$data$Iteration, c(2, 3))
  expect_s3_class(.plot_convergence_engine(c(3, 2, 1), show_best = FALSE), "ggplot")
})

test_that(".plot_convergence_engine validates the error vector", {
  expect_error(.plot_convergence_engine(numeric(0)), "empty")
  expect_error(.plot_convergence_engine(c(1, 2), first_iter = -1), "non-negative integer")
  expect_error(.plot_convergence_engine(c(1, 2), first_iter = 1.5), "non-negative integer")
  expect_error(.plot_convergence_engine(c(1, 2), first_iter = 5), "must be smaller")
})

test_that(".plot_convergence_ratio_engine builds a ratio plot", {
  p <- .plot_convergence_ratio_engine(c(3, 2, 1), first_iter = 0L)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
  expect_s3_class(
    .plot_convergence_ratio_engine(c(3, 2, 1), show_mean = FALSE, show_median = FALSE,
                             show_final = FALSE),
    "ggplot"
  )
})

test_that(".plot_convergence_ratio_engine validates the ratio vector", {
  expect_error(.plot_convergence_ratio_engine(numeric(0)), "empty")
  expect_error(.plot_convergence_ratio_engine(c(1, 2), first_iter = -1),
               "non-negative integer")
  expect_error(.plot_convergence_ratio_engine(c(1, 2), first_iter = 2), "must be smaller")
})

test_that(".extract_error_vec selects the requested run", {
  expect_equal(.extract_error_vec(list(a = 1:3, b = 4:6), run = 2), 4:6)
  expect_equal(.extract_error_vec(1:3), 1:3)
  expect_error(.extract_error_vec(list(1:3), run = 0), "positive integer")
  expect_error(.extract_error_vec(list(1:3), run = 2), "out of bounds")
  expect_error(.extract_error_vec(list(1:3, 4:6), run = 3), "are only 2 runs")
  expect_error(.extract_error_vec(1:3, run = 2), "Only 1 run")
})

test_that("plot_convergence.stats2data_vec labels the variable", {
  p <- quiet_plot(plot_convergence(fx_vec()))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "V1")
  x <- fx_vec()
  names(x$track_objective) <- NULL
  expect_match(quiet_plot(plot_convergence(x, run = 2))$labels$title, "V2")
})

test_that("plot_convergence.stats2data_vec requires track_objective", {
  x <- fx_vec()
  x$track_objective <- NULL
  expect_error(plot_convergence(x), "No .track_objective. element")
})

test_that("plot_convergence.stats2data_aov plots the trajectory", {
  expect_s3_class(quiet_plot(plot_convergence(fx_aov())), "ggplot")
  x <- fx_aov()
  x$track_objective <- NULL
  expect_error(plot_convergence(x), "No .track_objective. element")
})

test_that("plot_convergence.stats2data_mlr plots errors and ratios", {
  expect_s3_class(quiet_plot(plot_convergence(fx_mlr())), "ggplot")
  p <- quiet_plot(plot_convergence(fx_mlr(), ratio = TRUE))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Error Ratio")
})

test_that("plot_convergence.stats2data_mlr validates its flags", {
  x <- fx_mlr()
  expect_error(plot_convergence(x, ratio = "yes"), "single logical")
  expect_error(plot_convergence(x, ratio = TRUE, show_mean = "yes"), "single logical")
  expect_error(plot_convergence(x, ratio = TRUE, show_median = "yes"), "single logical")
  expect_error(plot_convergence(x, ratio = TRUE, show_final = "yes"), "single logical")
  x$track_objective_ratio <- NULL
  expect_error(plot_convergence(x, ratio = TRUE), "No .track_objective_ratio. element")
  y <- fx_mlr()
  y$track_objective <- NULL
  expect_error(plot_convergence(y), "No .track_objective. element")
})
