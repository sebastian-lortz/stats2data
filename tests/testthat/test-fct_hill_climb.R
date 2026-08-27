err_fn <- function(cand) list(total_error = sum(abs(cand[, 1] - seq_len(nrow(cand)))))

test_that("hill_climb validates its arguments", {
  cand <- matrix(c(3, 1, 2, 4), ncol = 1)
  expect_error(hill_climb(error_function = err_fn, N = 4, num_preds = 1),
               "must be provided")
  expect_error(hill_climb(cand, error_function = 1, N = 4, num_preds = 1),
               "must be a function")
  expect_error(hill_climb(cand, err_fn, N = 0, num_preds = 1),
               "single positive integer")
  expect_error(hill_climb(cand, err_fn, N = 4, hill_climbs = -1, num_preds = 1),
               "NULL or a single positive integer")
  expect_error(hill_climb(cand, err_fn, N = 4, num_preds = 0), "num_preds")
  expect_error(hill_climb(cand, err_fn, N = 4, num_preds = 1,
                          neighborhood_size = 0), "neighborhood_size")
  expect_error(hill_climb(cand, err_fn, N = 4, num_preds = 1,
                          progress_mode = "loud"), "console")
})

test_that("hill_climb does not increase the error", {
  set.seed(41)
  cand <- matrix(c(3, 1, 2, 4), ncol = 1)
  res <- hill_climb(cand, err_fn, N = 4, hill_climbs = 20, num_preds = 1,
                    progress_mode = "off")
  expect_lte(res$best_error, err_fn(cand)$total_error)
  expect_equal(dim(res$best_candidate), dim(cand))
})

test_that("hill_climb stops at a perfect solution and reports to the console", {
  set.seed(42)
  cand <- matrix(c(1, 2, 3, 4), ncol = 1)
  expect_output(
    res <- hill_climb(cand, err_fn, N = 4, hill_climbs = 10, num_preds = 1,
                      progress_mode = "console"),
    "Hill climbing best error"
  )
  expect_equal(res$best_error, 0)
})

test_that("hill_climb calls the progressor at the requested interval", {
  calls <- 0L
  p <- function() calls <<- calls + 1L
  flat <- function(cand) list(total_error = 1)
  set.seed(43)
  hill_climb(matrix(c(3, 1, 2, 4), ncol = 1), flat, N = 4, hill_climbs = 4,
             num_preds = 1, progressor = p, pb_interval = 2,
             progress_mode = "off")
  expect_equal(calls, 2L)
})
