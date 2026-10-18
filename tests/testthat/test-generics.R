test_that("the generics dispatch on stats2data classes", {
  expect_type(get_stats(fx_vec()), "list")
  expect_type(get_rmse(fx_vec()), "list")
  expect_true(is.function(plot_convergence))
  expect_true(is.function(plot_cooling))
  expect_true(is.function(plot_fit))
  expect_true(is.function(plot_stability))
})

test_that("the generics fail on unsupported classes", {
  x <- structure(list(), class = "not_stats2data")
  expect_error(get_stats(x),        "applied to an object of class")
  expect_error(get_rmse(x),         "applied to an object of class")
  expect_error(plot_convergence(x), "applied to an object of class")
  expect_error(plot_cooling(x),     "applied to an object of class")
  expect_error(plot_fit(x),         "applied to an object of class")
  expect_error(plot_stability(x),   "applied to an object of class")
})

test_that("plot() dispatches to the requested panel", {
  res <- fx_vec()
  expect_s3_class(plot(res), "ggplot")
  expect_error(plot(res, which = "nope"), "should be one of")
})
