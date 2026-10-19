test_that("name_with_info returns html", {
  x <- with_mock_session(name_with_info("Sample Size", "the N"))
  expect_s3_class(x, "html")
  expect_match(as.character(x), "Sample Size")
  expect_true(length(htmltools::findDependencies(x)) > 0)
})

test_that("arg_label writes the label out and names the argument in the tooltip", {
  x <- as.character(with_mock_session(arg_label("Max Iterations", "Iterations per start.", "max_iter")))
  expect_match(x, "Max Iterations", fixed = TRUE)
  expect_match(x, "(argument: max_iter)", fixed = TRUE)
  expect_match(x, "s2d-label", fixed = TRUE)
  expect_false(grepl("<code>", x, fixed = TRUE))
  expect_false(grepl("argument:", as.character(with_mock_session(arg_label("Run", "A run."))),
                     fixed = TRUE))
})

test_that("s2d_default_input starts empty and shows the default", {
  x <- as.character(s2d_default_input("init_temp", "Initial Temperature", "Default: 0.001"))
  expect_match(x, 'placeholder="Default: 0.001"', fixed = TRUE)
  expect_false(grepl('value="[0-9]', x))  # empty in the browser (value NA)
})

test_that("s2d_cooling_default follows max_iter", {
  expect_identical(s2d_cooling_default(1e5), "0.9999")
  expect_identical(s2d_cooling_default(5e3), "0.998")
  expect_match(s2d_cooling_default(NA), "Max Iterations")
  expect_match(s2d_cooling_default(10), "Max Iterations")
})

test_that("s2d_filled names the empty inputs", {
  expect_true(with_mock_session(s2d_filled(list("Sample Size" = 10, "Max Iterations" = 5))))
  expect_false(with_mock_session(s2d_filled(list("Sample Size" = NA, "Max Iterations" = NULL))))
})

test_that("s2d_targets_table writes out the headers but keeps the names", {
  df <- data.frame(variable = "X1", target_mean = 1, target_sd = 1, range_min = 0,
                   range_max = 2, integer = TRUE)
  tb <- s2d_targets_table(df)
  expect_identical(tb$x$colHeaders, c("Variable", "Mean", "SD", "Min", "Max", "Integer"))
  expect_identical(unlist(tb$x$rColnames), names(df))
  expect_identical(tb$x$columns[[6]]$type, "checkbox")
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
  expect_true(chk_null("init_temp"))
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
  expect_false(chk(levels = c(NA, 2)))
  expect_false(chk(levels = "a"))
  expect_false(chk(range = c(10, 0)))
  expect_false(chk(target_group_means = c(3, 5, 4, 99)))
  expect_false(chk(thresh = -1))
  expect_false(chk(max_iter = 0))
  expect_false(chk(init_temp = 0))
  expect_false(chk(cooling_rate = 1))
  expect_false(chk(max_starts = 0))
})

test_that("check_mlr_inputs accepts valid input and rejects invalid input", {
  ok <- list(
    thresh = 1e-3, max_iter = 100, init_temp = 1, cooling_rate = 0.9,
    hill_climbs = 10, max_starts = 1
  )
  chk <- function(...) {
    with_mock_session(do.call(check_mlr_inputs, utils::modifyList(ok, list(...))))
  }
  chk_null <- function(nm) {
    a <- c(ok[setdiff(names(ok), nm)], stats::setNames(list(NULL), nm))
    with_mock_session(do.call(check_mlr_inputs, a))
  }
  expect_true(chk())
  expect_true(chk_null("init_temp"))
  expect_true(chk_null("hill_climbs"))
  expect_false(chk(thresh = -1))
  expect_false(chk(max_iter = 0))
  expect_false(chk(init_temp = 0))
  expect_false(chk(cooling_rate = 0))
  expect_false(chk(hill_climbs = -1L))
  expect_false(chk(hill_climbs = 1.5))
  expect_false(chk(max_starts = 0))
})

test_that("na_null maps empty inputs to NULL", {
  expect_null(na_null(NULL))
  expect_null(na_null(NA))
  expect_null(na_null(c(NA, NA)))
  expect_identical(na_null(c(1, NA)), c(1, NA))
  expect_identical(na_null(0.5), 0.5)
})

test_that("s2d_effects lists main effects and interactions", {
  expect_identical(s2d_effects(c("A", "B")), c("A", "B", "A:B"))
  expect_identical(s2d_effects(c("A", "B", "C"), min_order = 2L),
                   c("A:B", "A:C", "B:C", "A:B:C"))
  expect_identical(s2d_effects("A", min_order = 2L), character(0))
})

test_that("s2d_level_grid orders cells with the first factor slowest", {
  g <- s2d_level_grid(c("A", "B"), c(2, 3))
  expect_identical(names(g), c("A", "B"))
  expect_identical(paste0(g$A, g$B), c("11", "12", "13", "21", "22", "23"))
})

test_that("s2d_deparse writes R code that evaluates to the same value", {
  vals <- list(
    NULL, 0.5, c(V1 = 10.53, V2 = -0.64), c(TRUE, FALSE), c(0.1, NA, 0.3),
    rbind(c(0, -24), c(36, 24)), "outcome ~ Factor1 + Error(ID/Factor2)",
    list(effect = c("Factor1", "Factor1:Factor2"), F_value = c(8.2, 3.4)),
    c("(Intercept)" = 0.115, `V1:V2` = 0)
  )
  for (v in vals) {
    expect_identical(eval(parse(text = s2d_deparse(v))), v)
  }
})

test_that("s2d_call_code writes a runnable call with the seed", {
  args <- list(N = 10, target_mean = c(x = 1), range = rbind(0, 5), init_temp = NULL)
  single <- s2d_call_code("optim_vec", args, runs = 1L, seed = 42L)
  expect_match(single, "set.seed(42L)", fixed = TRUE)
  expect_match(single, "res <- optim_vec(", fixed = TRUE)
  expect_match(single, "init_temp = NULL", fixed = TRUE)
  expect_silent(parse(text = single))

  multi <- s2d_call_code("optim_vec", args, runs = 3L, seed = 42L,
                         notes = "# Factors: Factor1 = A")
  expect_match(multi, "parallel_optim(", fixed = TRUE)
  expect_match(multi, "runs = 3L", fixed = TRUE)
  expect_match(multi, "seed = 42L", fixed = TRUE)
  expect_match(multi, "# Factors: Factor1 = A", fixed = TRUE)
  expect_silent(parse(text = multi))

  # the call passes exactly the arguments the app used
  cl <- parse(text = single)[[3]][[3]]
  expect_identical(names(as.list(cl))[-1], names(args))
})
