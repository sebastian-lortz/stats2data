test_that("plot_histogram returns one plot per column", {
  df <- data.frame(a = c(1, 2, 2, 3, 3, 3), b = c(0.1, 0.4, 0.6, 0.9, 1.2, 1.7))
  p <- plot_histogram(df)
  expect_named(p, c("a", "b"))
  expect_s3_class(p$a, "ggplot")
  expect_s3_class(p$b, "ggplot")
  expect_s3_class(plot_histogram(df, SD = FALSE)$a, "ggplot")
})

test_that("plot_partial_regression returns one plot per term", {
  d <- data.frame(
    y  = c(2, 4, 5, 4, 6, 8, 7, 9),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(2, 1, 4, 3, 6, 5, 8, 7)
  )
  p <- plot_partial_regression(stats::lm(y ~ x1 + x2, data = d))
  expect_named(p, c("x1", "x2"))
  expect_s3_class(p$x1, "ggplot")
})

test_that("plot_partial_regression handles interaction terms", {
  d <- data.frame(
    y  = c(2, 4, 5, 4, 6, 8, 7, 9),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(2, 1, 4, 3, 6, 5, 8, 7)
  )
  p <- plot_partial_regression(stats::lm(y ~ x1 * x2, data = d))
  expect_named(p, c("x1", "x2", "x1:x2"))
})

test_that("long_to_wide reshapes and validates", {
  long <- data.frame(
    ID = rep(1:3, each = 2), group = rep(c("a", "b", "a"), each = 2),
    time = rep(1:2, 3), score = 1:6
  )
  w <- long_to_wide(long)
  expect_s3_class(w, "data.frame")
  expect_equal(nrow(w), 3)
  expect_true(all(c("score_1", "score_2") %in% names(w)))

  expect_error(long_to_wide(1:3), "data.frame")
  expect_error(long_to_wide(data.frame(a = 1, b = 2)), "at least 1 measure")
  expect_error(long_to_wide(data.frame(a = 1, b = 2, c = 3)), "not found")
})

test_that("long_to_wide works without between-subject columns", {
  long <- data.frame(ID = rep(1:3, each = 2), time = rep(1:2, 3), score = 1:6)
  expect_equal(nrow(long_to_wide(long)), 3)
})

test_that("wide_to_long reshapes and validates", {
  wide <- data.frame(ID = 1:3, V1_1 = 1:3, V1_2 = 4:6)
  l <- wide_to_long(wide)
  expect_equal(nrow(l), 6)
  expect_true(all(c("ID", "time", "V1") %in% names(l)))
  expect_type(l$time, "integer")

  expect_equal(nrow(wide_to_long(as.matrix(data.frame(V1_1 = 1:3, V1_2 = 4:6, V1_3 = 7:9)))), 9)
  expect_error(wide_to_long(1:3), "data.frame or matrix")
  expect_error(wide_to_long(data.frame(a = 1, b = 2)), "wide format")
  expect_error(wide_to_long(data.frame(ID = 1:2, a = 1:2, b = 3:4)),
               "at least two repeated-measure")
})
