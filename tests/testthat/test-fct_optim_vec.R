vec_args <- list(
  N = 10, target_mean = c(x = 3), target_sd = c(x = 1), range = c(1, 5),
  integer = TRUE, thresh = 0.05, sprite_prec = c(2, 2), max_iter = 10,
  init_temp = 0.1, cooling_rate = 0.9, max_starts = 1, progress_mode = "off"
)

run_vec <- function(...) do.call(optim_vec, utils::modifyList(vec_args, list(...)))

test_that("optim_vec validates its arguments", {
  expect_error(run_vec(N = 0), "single positive integer")
  expect_error(run_vec(N = 10.5), "single positive integer")
  expect_error(run_vec(target_sd = c(x = 1, y = 2)), "same positive length")
  expect_error(run_vec(target_mean = c(3)), "non-empty names")
  expect_error(run_vec(range = c(1, 2, 3)), "numeric vector of length 2")
  expect_error(run_vec(thresh = -1), "single non-negative number")
  expect_error(run_vec(integer = 1), "logical vector")
  expect_error(run_vec(max_iter = 0), "single positive number")
  expect_error(run_vec(init_temp = 0), "positive number or NULL")
  expect_error(run_vec(cooling_rate = 1.5), "must be in")
  expect_error(run_vec(max_starts = 0), "single positive integer")
  expect_error(run_vec(sprite_prec = c(2, 2, 2)), "integer vector of length 2")
  expect_error(run_vec(progress_mode = "loud"), "console")
  expect_error(
    optim_vec(N = 10, target_mean = c(x = 3), target_sd = c(x = 1),
              range = c(1, 5), integer = TRUE, sprite_prec = NULL,
              progress_mode = "off"),
    "must be specified when"
  )
})

test_that("optim_vec returns a stats2data_vec object for integer data", {
  set.seed(11)
  res <- run_vec()
  expect_s3_class(res, "stats2data_vec")
  expect_equal(dim(res$data), c(10L, 1L))
  expect_named(res$data, "x")
  expect_length(res$best_objective, 1)
  expect_equal(res$inputs$N, 10)
})

test_that("optim_vec handles the binary range shortcut", {
  res <- run_vec(target_mean = c(x = 0.4), target_sd = c(x = 0.5), range = c(0, 1))
  expect_equal(sum(res$data$x), 4)
  expect_equal(res$best_objective[[1]], 0)
})

test_that("optim_vec optimises continuous data and per-variable settings", {
  set.seed(12)
  res <- run_vec(
    target_mean = c(x = 3, y = 3), target_sd = c(x = 1, y = 1),
    range = matrix(c(1, 5, 1, 5), nrow = 2), integer = c(TRUE, FALSE),
    max_iter = 20, cooling_rate = NULL, init_temp = NULL
  )
  expect_equal(dim(res$data), c(10L, 2L))
  expect_true(all(res$data$y >= 1 & res$data$y <= 5))
  expect_true(length(res$track_objective[[2]]) > 0)
  expect_equal(res$inputs$cooling_rate, (20 - 10) / 20)
})

test_that("optim_vec supports the console progress handler", {
  res <- NULL
  utils::capture.output(
    res <- run_vec(target_mean = c(x = 0.4), target_sd = c(x = 0.5),
                   range = c(0, 1), progress_mode = "console")
  )
  expect_s3_class(res, "stats2data_vec")
})

test_that("optim_vec_single handles degenerate ranges", {
  res <- optim_vec_single(N = 5, t_mean = 2, t_sd = 0, rng = c(2, 2),
                          is_int = FALSE, sprite_prec = c(2, 2), thresh = 0.05,
                          max_iter = 5, init_temp = 0.1, cooling_rate = 0.9,
                          max_starts = 1)
  expect_equal(res$data, rep(2, 5))
  expect_equal(res$best_objective, 0)
})

test_that("optim_vec_single validates the range", {
  base <- list(N = 5, t_sd = 1, sprite_prec = c(2, 2), thresh = 0.05,
               max_iter = 5, init_temp = 0.1, cooling_rate = 0.9,
               max_starts = 1, is_int = FALSE)
  expect_error(do.call(optim_vec_single, c(base, list(t_mean = 3, rng = c(5, 1)))),
               "range\\[1\\] <= range\\[2\\]")
  expect_error(do.call(optim_vec_single, c(base, list(t_mean = 3, rng = c(2, 2)))),
               "Zero-width range")
  expect_error(do.call(optim_vec_single, c(base, list(t_mean = 9, rng = c(1, 5)))),
               "must lie within")
})

test_that("optim_vec_single reports SPRITE failures instead of erroring", {
  res <- optim_vec_single(N = 5, t_mean = 3, t_sd = 99, rng = c(1, 5),
                          is_int = TRUE, sprite_prec = c(2, 2), thresh = 0.05,
                          max_iter = 5, init_temp = 0.1, cooling_rate = 0.9,
                          max_starts = 1)
  expect_true(is.infinite(res$best_objective))
  expect_true(!is.null(res$error_msg))
})
