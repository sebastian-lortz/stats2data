test_that("new_s2d_aov validates its arguments", {
  at <- list(group_means = 1, F_values = 1)
  expect_error(new_s2d_aov(1, 1:3, list(), at, 1), "must be a data.frame")
  expect_error(new_s2d_aov(1, data.frame(a = 1), 1, at, 1), "must be a list")
  expect_error(new_s2d_aov(1, data.frame(a = 1), list(), list(a = 1), 1),
               "group_means")
})

test_that("print.stats2data_aov reports the design", {
  expect_output(expect_invisible(print(fx_aov())), "stats2data ANOVA result")
  expect_output(print(fx_aov()), "Factor1")
})

test_that(".aov_formula_from_inputs rebuilds a two-sided formula", {
  expect_equal(
    deparse(.aov_formula_from_inputs(list(formula = "~ Factor1 + Error(ID/Factor1)"))),
    "outcome ~ Factor1 + Error(ID/Factor1)"
  )
  expect_equal(
    deparse(.aov_formula_from_inputs(list(formula = stats::as.formula("y ~ Factor1")))),
    "outcome ~ Factor1 + Error(ID)"
  )
  expect_error(.aov_formula_from_inputs(list()), "missing")
  expect_error(.aov_formula_from_inputs(list(formula = 1)), "not a formula")
})

test_that("get_stats.stats2data_aov returns F values and cell means", {
  skip_if_not_installed("afex")
  s <- suppressMessages(get_stats(fx_aov()))
  expect_named(s, c("model", "F_value", "mean"))
  expect_length(s$F_value, 1)
  expect_true(is.finite(s$F_value))
  expect_named(s$mean, c("1", "2"))
  expect_equal(unname(s$mean), c(3.5, 6.1667), tolerance = 1e-3)
})

test_that("get_stats.stats2data_aov returns NA for unknown effects", {
  skip_if_not_installed("afex")
  x <- fx_aov()
  x$inputs$target_f_list$effect <- "Factor9"
  expect_true(is.na(suppressMessages(get_stats(x))$F_value))
})

test_that("get_rmse.stats2data_aov compares F values and means", {
  skip_if_not_installed("afex")
  r <- suppressMessages(get_rmse(fx_aov()))
  expect_named(r, c("rmse_F", "rmse_mean"))
  expect_true(all(vapply(r, is.finite, logical(1))))
})

test_that("summary.stats2data_aov builds the comparison tables", {
  skip_if_not_installed("afex")
  s <- suppressMessages(summary(fx_aov()))
  expect_s3_class(s, "summary.stats2data_aov")
  expect_equal(nrow(s$f_comparison), 1)
  expect_equal(nrow(s$means_comparison), 2)
  expect_equal(s$means_comparison$cell, c("F1=1", "F1=2"))
  expect_output(print(s), "stats2data ANOVA Summary")
  expect_output(print(s), "Group Means")
})

test_that("print.summary.stats2data_aov warns about empty comparisons", {
  skip_if_not_installed("afex")
  s <- suppressMessages(summary(fx_aov()))
  s$f_comparison <- NULL
  s$means_comparison <- s$means_comparison[0, ]
  expect_warning(
    expect_warning(utils::capture.output(print(s)), "f_comparison"),
    "means_comparison"
  )
})
