mlr_args <- list(
  N = 12, target_mean = c(X1 = 3, X2 = 3, Y = 3),
  target_sd = c(X1 = 1, X2 = 1, Y = 1), range = c(1, 5), integer = TRUE,
  target_cor = c(0.1, 0.2, 0.3), target_reg = c(1, 0.2, 0.3),
  reg_equation = "Y ~ X1 + X2", sprite_prec = c(2, 2), weight = 0.5,
  thresh = 1e-6, max_iter = 20, init_temp = 1, cooling_rate = 0.9,
  max_starts = 1, hill_climbs = 3, progress_mode = "off"
)

run_mlr <- function(...) do.call(optim_mlr, utils::modifyList(mlr_args, list(...)))

test_that("optim_mlr validates its arguments", {
  expect_error(run_mlr(N = 0), "single positive integer")
  expect_error(run_mlr(target_sd = c(X1 = 1)), "same positive length")
  expect_error(run_mlr(target_mean = c(3, 3, 3)), "non-empty names")
  expect_error(run_mlr(range = c(1, 2, 3)), "numeric vector of length 2")
  expect_error(run_mlr(integer = 1), "logical vector")
  expect_error(run_mlr(sprite_prec = c(2, 2, 2)), "integer vector of length 2")
  expect_error(run_mlr(reg_equation = 1), "single character string")
  expect_error(run_mlr(reg_equation = "Y ~ X1 + Z"), "are not in")
  expect_error(run_mlr(target_cor = c(2, 0.2, 0.3)), "between -1 and 1")
  expect_error(run_mlr(target_cor = c(0.1, 0.2)), "must be a numeric vector of length 3")
  expect_error(run_mlr(target_reg = c(NA, NA, NA)), "at least one non-NA")
  expect_error(run_mlr(target_reg = c(1, 0.2)), "must be a numeric vector of length 3")
  expect_error(run_mlr(target_se = c(0.1, 0.1)), "same length as")
  expect_error(run_mlr(weight = c(0.5, 0.5)), "length 1")
  expect_error(run_mlr(weight = 2), "between 0 and 1")
  expect_error(run_mlr(thresh = -1), "single non-negative number")
  expect_error(run_mlr(max_iter = 0), "single positive number")
  expect_error(run_mlr(init_temp = 0), "positive number or NULL")
  expect_error(run_mlr(cooling_rate = 1.5), "must be in")
  expect_error(run_mlr(max_starts = 0), "single positive integer")
  expect_error(run_mlr(hill_climbs = -1), "non-negative integer")
  expect_error(run_mlr(progress_mode = "loud"), "console")
  expect_error(
    optim_mlr(N = 12, target_mean = c(X1 = 3, X2 = 3, Y = 3),
              target_sd = c(X1 = 1, X2 = 1, Y = 1), range = c(1, 5),
              integer = TRUE, target_cor = c(0.1, 0.2, 0.3),
              target_reg = c(1, 0.2, 0.3), reg_equation = "Y ~ X1 + X2",
              sprite_prec = NULL, progress_mode = "off"),
    "must be specified when"
  )
})

test_that("optim_mlr returns a stats2data_mlr object", {
  set.seed(21)
  res <- run_mlr()
  expect_s3_class(res, "stats2data_mlr")
  expect_equal(dim(res$data), c(12L, 3L))
  expect_named(res$data, c("X1", "X2", "Y"))
  expect_s3_class(res$optim_vec, "stats2data_vec")
  expect_gt(length(res$track_error), 0)
  expect_lte(length(res$track_error), 20)
  expect_equal(length(res$track_error_ratio), length(res$track_error))
  expect_equal(res$inputs$weight, c(0.5, 0.5))
})

test_that("optim_mlr uses the standard-error objective when target_se is given", {
  set.seed(22)
  res <- run_mlr(target_se = c(0.3, 0.1, 0.1))
  expect_s3_class(res, "stats2data_mlr")
  expect_equal(res$inputs$target_se, c(0.3, 0.1, 0.1))
})

test_that("optim_mlr supports interactions and calibrated temperature", {
  set.seed(23)
  res <- run_mlr(reg_equation = "Y ~ X1 + X2 + X1:X2",
                 target_reg = c(1, 0.2, 0.3, 0.1), init_temp = NULL,
                 cooling_rate = NULL, hill_climbs = 0)
  expect_s3_class(res, "stats2data_mlr")
  expect_named(res$inputs$target_reg,
               c("(Intercept)", "X1", "X2", "X1:X2"))
  expect_equal(res$inputs$cooling_rate, 0.5)
})
