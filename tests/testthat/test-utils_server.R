test_that("name_with_info returns html", {
  x <- with_mock_session(name_with_info("Sample Size", "the N"))
  expect_s3_class(x, "html")
  expect_match(as.character(x), "Sample Size")
})

test_that("check_vec_inputs accepts valid input and rejects invalid input", {
  ok <- list(
    N = 20, target_mean = c(1, 2), range = rbind(c(0, 0), c(10, 10)),
    thresh = 1e-3, max_iter = 100, init_temp = 1, cooling_rate = 0.9,
    max_starts = 1
  )
  chk <- function(...) {
    with_mock_session(do.call(check_vec_inputs, utils::modifyList(ok, list(...))))
  }
  chk_null <- function(nm) {
    a <- c(ok[setdiff(names(ok), nm)], stats::setNames(list(NULL), nm))
    with_mock_session(do.call(check_vec_inputs, a))
  }
  expect_true(chk())
  expect_false(chk(N = 5))
  expect_false(chk(N = 20.5))
  expect_false(chk(N = "a"))
  expect_false(chk(target_mean = c(1, 99)))
  expect_false(chk(thresh = -1))
  expect_false(chk(max_iter = 0))
  expect_false(chk(init_temp = 0))
  expect_false(chk(cooling_rate = 2))
  expect_false(chk(max_starts = 0))
  expect_true(chk_null("cooling_rate"))
})

test_that("check_aov_inputs accepts valid input and rejects invalid input", {
  ok <- list(
    N = 20, levels = c(2, 2), target_group_means = c(3, 5, 4, 6),
    range = c(0, 10), thresh = 1e-3, max_iter = 100, init_temp = 1,
    cooling_rate = 0.9, max_starts = 1
  )
  chk <- function(...) {
    with_mock_session(do.call(check_aov_inputs, utils::modifyList(ok, list(...))))
  }
  expect_true(chk())
  expect_false(chk(N = 5))
  expect_false(chk(levels = c(2.5, 2)))
  expect_false(chk(levels = "a"))
  expect_false(chk(target_group_means = c(3, 5, 4, 99)))
  expect_false(chk(thresh = -1))
  expect_false(chk(max_iter = 0))
  expect_false(chk(init_temp = 0))
  expect_false(chk(cooling_rate = 1))
  expect_false(chk(max_starts = 0))
})

test_that("check_lm_inputs accepts valid input and rejects invalid input", {
  ok <- list(
    thresh = 1e-3, max_iter = 100, init_temp = 1, cooling_rate = 0.9,
    hill_climbs = 10, max_starts = 1
  )
  chk <- function(...) {
    with_mock_session(do.call(check_lm_inputs, utils::modifyList(ok, list(...))))
  }
  chk_null <- function(nm) {
    a <- c(ok[setdiff(names(ok), nm)], stats::setNames(list(NULL), nm))
    with_mock_session(do.call(check_lm_inputs, a))
  }
  expect_true(chk())
  expect_true(chk_null("init_temp"))
  expect_true(chk_null("hill_climbs"))
  expect_false(chk(thresh = -1))
  expect_false(chk(max_iter = 0))
  expect_false(chk(init_temp = 0))
  expect_false(chk(cooling_rate = 0))
  expect_false(chk(hill_climbs = -1L))
  expect_false(chk(max_starts = 0))
})
