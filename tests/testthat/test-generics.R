test_that("the generics dispatch on stats2data classes", {
  expect_s3_class(get_stats(fx_vec()), "list")
  expect_s3_class(get_rmse(fx_vec()), "list")
  expect_true(is.function(plot_error))
  expect_true(is.function(plot_cooling))
  expect_true(is.function(plot_summary))
})

test_that("the generics fail on unsupported classes", {
  x <- structure(list(), class = "not_stats2data")
  expect_error(get_stats(x), "applied to an object of class")
  expect_error(get_rmse(x), "applied to an object of class")
  expect_error(plot_error(x), "applied to an object of class")
  expect_error(plot_cooling(x), "applied to an object of class")
  expect_error(plot_summary(x), "applied to an object of class")
})

test_that("theme_stats2data returns a ggplot2 theme", {
  th <- theme_stats2data()
  expect_s3_class(th, "theme")
  expect_s3_class(theme_stats2data(base_size = 8), "theme")
})
