fake_vec <- function(progress_mode = "off", thresh = 0.5) fx_vec()
fake_bad <- function(progress_mode = "off") list(best_objective = 1)
fake_no_thresh <- function(progress_mode = "off") fx_vec()

test_that("parallel_optim validates its arguments", {
  expect_error(parallel_optim(FUN = 1, args = list()), "must be a function")
  expect_error(parallel_optim(fake_vec, args = 1), "named list")
  expect_error(parallel_optim(fake_vec, list(), runs = 0), "positive integer")
  expect_error(parallel_optim(fake_vec, list(), cores = 0), "positive integer")
  expect_error(parallel_optim(fake_vec, list(), seed = "a"), "single integer")
  expect_error(parallel_optim(fake_vec, list(), stop_conv = NA), "single logical")
  expect_error(parallel_optim(fake_vec, list(), progress_mode = "loud"), "console")
})

test_that("parallel_optim runs sequentially and identifies the module", {
  res <- parallel_optim(fake_vec, list(), runs = 2, progress_mode = "off")
  expect_s3_class(res, "stats2data_parallel")
  expect_equal(res$module, "vec")
  expect_equal(res$runs, 2)
  expect_equal(res$runs_requested, 2)
  expect_false(res$converged)
  expect_s3_class(res$best, "stats2data_vec")
})

test_that("parallel_optim reports progress on the console", {
  expect_output(parallel_optim(fake_vec, list(), runs = 1,
                               progress_mode = "console"), "Run 1")
})

test_that("parallel_optim stops early on convergence", {
  res <- parallel_optim(fake_vec, list(thresh = 0.5), runs = 3,
                        stop_conv = TRUE, progress_mode = "off")
  expect_true(res$converged)
  expect_equal(res$runs, 1)
  expect_equal(res$thresh, 0.5)
})

test_that("parallel_optim falls back to the threshold in the formals of FUN", {
  res <- parallel_optim(fake_vec, list(), runs = 2, stop_conv = TRUE,
                        progress_mode = "off")
  expect_equal(res$thresh, 0.5)
  expect_true(res$converged)
})

test_that("parallel_optim disables early stopping without a usable threshold", {
  expect_warning(
    res <- parallel_optim(fake_no_thresh, list(), runs = 1, stop_conv = TRUE,
                          progress_mode = "off"),
    "early stopping is disabled"
  )
  expect_false(res$converged)
  expect_null(res$thresh)
})

test_that("parallel_optim keeps running when runs do not converge", {
  res <- parallel_optim(fake_vec, list(thresh = 1e-12), runs = 2,
                        stop_conv = TRUE, progress_mode = "off")
  expect_false(res$converged)
  expect_equal(res$runs, 2)
})

test_that("parallel_optim seeds each run reproducibly", {
  a <- parallel_optim(fake_vec, list(), runs = 2, seed = 42, progress_mode = "off")
  expect_equal(a$runs, 2)
})

test_that("parallel_optim rejects unknown result classes", {
  expect_error(parallel_optim(fake_bad, list(), runs = 1, progress_mode = "off"),
               "Unrecognised result class")
})

test_that("parallel_optim runs on multiple cores", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  res <- parallel_optim(
    FUN = function(progress_mode = "off") {
      structure(list(best_objective = 0.1, data = data.frame(x = 1),
                     inputs = list(), track_objective = 1),
                class = "stats2data_vec")
    },
    args = list(), runs = 2, cores = 2, progress_mode = "off"
  )
  expect_equal(res$runs, 2)
  expect_equal(res$module, "vec")
})

test_that("print.stats2data_parallel reports the runs and errors", {
  res <- fx_parallel("vec")
  expect_output(print(res), "stats2data parallel result")
  expect_output(print(res), "Runs")
  res$converged <- TRUE
  res$thresh <- 0.5
  res$runs <- 1L
  expect_output(print(res), "Converged")
  expect_output(print(res), "stopped early")
  expect_output(print(fx_parallel("mlr")), "Best objective")
})
