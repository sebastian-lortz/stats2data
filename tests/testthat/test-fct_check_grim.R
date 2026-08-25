test_that("check_grim validates its arguments", {
  expect_error(check_grim(0, 3.7, 1), "single positive integer")
  expect_error(check_grim(c(1, 2), 3.7, 1), "single positive integer")
  expect_error(check_grim(10.5, 3.7, 1), "single positive integer")
  expect_error(check_grim(10, c(1, 2), 1), "single numeric value")
  expect_error(check_grim(10, "a", 1), "single numeric value")
  expect_error(check_grim(10, 3.7, -1), "non-negative integer")
  expect_error(check_grim(10, 3.7, 1.5), "non-negative integer")
  expect_error(check_grim(10, 3.7, 1, tol = -1), "non-negative numeric")
})

test_that("check_grim accepts plausible means", {
  res <- check_grim(n = 10, target_mean = 3.7, decimals = 1)
  expect_true(res$test)
  expect_equal(res$grim_mean, 3.7)
})

test_that("check_grim rejects implausible means and returns the nearest", {
  res <- check_grim(n = 10, target_mean = 3.74, decimals = 2)
  expect_false(res$test)
  expect_equal(res$grim_mean, 3.7)
})
