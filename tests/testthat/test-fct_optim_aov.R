aov_args <- function() list(
  S = 8, levels = 2, target_group_means = c(3, 5),
  target_f_list = list(effect = "Factor1", F_value = 4), integer = FALSE,
  range = c(0, 10), formula = "outcome ~ Factor1", factor_type = "between",
  thresh = 5e-3, max_iter = 5, init_temp = 1e-3, cooling_rate = 0.9,
  max_starts = 1, progress_mode = "off"
)

run_aov <- function(...) do.call(optim_aov, utils::modifyList(aov_args(), list(...)))

test_that("optim_aov validates its arguments", {
  expect_error(run_aov(range = c(10, 0)), "range\\[1\\] < range\\[2\\]")
  expect_error(run_aov(S = 0), "single positive integer")
  expect_error(run_aov(levels = 1), "all values >= 2")
  expect_error(run_aov(integer = c(TRUE, FALSE)), "single logical value")
  expect_error(run_aov(target_group_means = c(3)), "length prod")
  expect_error(run_aov(target_group_means = c(3, 99)), "must lie within")
  expect_error({
    bad <- aov_args()
    bad$target_f_list <- list(effect = "Factor1")
    do.call(optim_aov, bad)
  }, "numeric vector")
  expect_error(run_aov(target_f_list = list(effect = c("a", "b"), F_value = 4)),
               "must match length")
  expect_error({
    bad <- aov_args()
    bad["init_temp"] <- list(NULL)
    do.call(optim_aov, bad)
  }, "single positive finite number")
  expect_error(run_aov(max_starts = 1.5), "single positive integer")
  expect_error(run_aov(max_starts = Inf), "single positive integer")
  expect_error(run_aov(formula = 1), "formula or single character")
  expect_error(run_aov(factor_type = "sideways"), "between")
  expect_error(run_aov(subgroup_sizes = c(4, 4, 4)), "must have length")
  expect_error(run_aov(subgroup_sizes = c(4, 5)), "must equal sum")
  expect_error(run_aov(thresh = -1), "single non-negative number")
  expect_error(run_aov(max_iter = 0), "single positive number")
  expect_error(run_aov(init_temp = 0), "single positive finite number")
  expect_error(run_aov(cooling_rate = 1.5), "must be in")
  expect_error(run_aov(max_starts = 0), "single positive integer")
  expect_error(run_aov(progress_mode = "loud"), "console")
})

test_that("optim_aov optimises a continuous between-subject design", {
  skip_if_not_installed("car")
  set.seed(31)
  res <- run_aov()
  expect_s3_class(res, "stats2data_aov")
  expect_equal(nrow(res$data), 8)
  expect_named(res$data, c("ID", "Factor1", "outcome"))
  expect_true(all(res$data$outcome >= 0 & res$data$outcome <= 10))
  expect_length(res$adjusted_targets$group_means, 2)
  expect_gt(length(res$track_objective), 0)
})

test_that("optim_aov calibrates the temperature for integer designs", {
  skip_if_not_installed("car")
  set.seed(32)
  res <- run_aov(integer = TRUE, init_temp = NULL, cooling_rate = NULL,
                 max_iter = 100, subgroup_sizes = c(4, 4))
  expect_s3_class(res, "stats2data_aov")
  expect_true(is.finite(res$inputs$init_temp))
  expect_gt(res$inputs$init_temp, 0)
  expect_equal(res$inputs$cooling_rate, (100 - 10) / 100)
})

test_that("optim_aov optimises a within-subject design", {
  skip_if_not_installed("afex")
  set.seed(33)
  res <- suppressMessages(run_aov(
    S = 6, integer = TRUE, factor_type = "within",
    formula = "outcome ~ Factor1 + Error(ID/Factor1)", max_iter = 3
  ))
  expect_s3_class(res, "stats2data_aov")
  expect_equal(nrow(res$data), 12)
  expect_equal(res$inputs$factor_type, "within")
})

test_that("optim_aov applies the default temperature schedule for integer designs", {
  skip_if_not_installed("car")
  set.seed(32)
  args <- utils::modifyList(
    aov_args(),
    list(integer = TRUE, max_iter = 100, subgroup_sizes = c(4, 4))
  )
  args$init_temp <- NULL
  args["cooling_rate"] <- list(NULL)
  res <- do.call(optim_aov, args)
  expect_s3_class(res, "stats2data_aov")
  expect_equal(res$inputs$init_temp, 1e-3)
  expect_equal(res$inputs$cooling_rate, (100 - 10) / 100)
})

test_that("optim_aov keeps the input cell order for factors with 10+ levels", {
  skip_if_not_installed("car")
  skip_if_not_installed("afex")
  set.seed(34)
  means <- seq(1.5, 6, by = 0.5)
  res <- suppressMessages(run_aov(S = 20, levels = 10, target_group_means = means,
                                  max_iter = 5))
  s <- suppressMessages(get_stats(res))
  expect_named(s$mean, as.character(1:10))
  expect_equal(unname(s$mean), means, tolerance = 0.01)
})
