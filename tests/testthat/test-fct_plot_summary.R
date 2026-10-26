test_that(".compute_centered standardises unless the target is near zero", {
  expect_equal(.compute_centered(c(2, 4), c(1, 2), TRUE, 1e-12), c(1, 1))
  expect_equal(.compute_centered(c(2, 4), c(1, 2), FALSE, 1e-12), c(1, 2))
  expect_equal(.compute_centered(2, 0, TRUE, 1e-12), 2)
})

test_that(".tag_sim_type flags near-zero targets", {
  df <- .tag_sim_type(data.frame(Target = c(1, 0)), TRUE, 1e-12)
  expect_equal(df$SimulatedType, c("Simulated", "Unstandardised Diff"))
  expect_equal(.tag_sim_type(data.frame(Target = 0), FALSE, 1e-12)$SimulatedType,
               "Simulated")
})

test_that(".plot_fit_engine warns about unstandardised entries", {
  df <- data.frame(
    Measure = "Mean", Variable = factor("V1"), Simulated = 1, Target = 0,
    Centered = 1, SimulatedType = "Unstandardised Diff"
  )
  expect_warning(.plot_fit_engine(df, TRUE, "Variable"), "practically 0")
})

test_that(".plot_fit_engine labels the axes", {
  df <- data.frame(
    Measure = "Mean", Variable = factor("V1"), Simulated = 1, Target = 1,
    Centered = 0, SimulatedType = "Simulated"
  )
  p <- .plot_fit_engine(df, TRUE, "Variable")
  expect_match(p$labels$x, "^Std\\. Difference")
  expect_equal(p$labels$y, "Variable")
  expect_match(.plot_fit_engine(df, FALSE, "Variable")$labels$x,
               "^Difference")
})

test_that("plot_fit.stats2data_vec builds the comparison frame", {
  p <- quiet_plot(plot_fit(fx_vec()))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 4)
  expect_equal(levels(p$data$Variable), c("V1", "V2"))
  expect_s3_class(quiet_plot(plot_fit(fx_vec(), standardised = FALSE)), "ggplot")
})

test_that("plot_fit.stats2data_mlr covers regression, correlation and se", {
  p <- quiet_plot(plot_fit(fx_mlr()))
  expect_equal(sort(unique(p$data$Measure)),
               c("Correlation", "Regression Coefficient", "Standard Error"))
  p0 <- quiet_plot(plot_fit(fx_mlr(se = FALSE)))
  expect_false("Standard Error" %in% p0$data$Measure)
})

test_that("plot_fit.stats2data_mlr names unnamed correlations", {
  p <- quiet_plot(plot_fit(fx_mlr()))
  expect_true(all(c("Cor1", "Cor2", "Cor3") %in% as.character(p$data$Variable)))
})

test_that("plot_fit.stats2data_aov uses the effect names", {
  skip_if_not_installed("afex")
  p <- suppressMessages(quiet_plot(plot_fit(fx_aov())))
  expect_equal(as.character(p$data$Variable), "Factor1")
})

test_that("plot_fit methods validate their arguments", {
  expect_error(plot_fit(fx_vec(), standardised = "yes"), "single logical")
  expect_error(plot_fit(fx_vec(), eps = "a"), "single numeric")
  expect_error(plot_fit(fx_mlr(), standardised = "yes"), "single logical")
  expect_error(plot_fit(fx_mlr(), eps = "a"), "single numeric")
  expect_error(plot_fit(fx_aov(), standardised = "yes"), "single logical")
  expect_error(plot_fit(fx_aov(), eps = "a"), "single numeric")
})

test_that(".plot_fit_engine orders parameters by fit when requested", {
  df <- data.frame(
    Measure = "Mean",
    Variable = factor(c("V1", "V2", "V3", "V4"), levels = c("V1", "V2", "V3", "V4")),
    Simulated = c(1.1, 0.5, 1.3, NA), Target = 1,
    Centered = c(0.1, -0.5, 0.3, NA), SimulatedType = "Simulated"
  )
  expect_equal(levels(.plot_fit_engine(df, TRUE, NULL)$data$Variable),
               c("V1", "V2", "V3", "V4"))
  # worst absolute fit first; all-NA parameters last
  expect_equal(levels(.plot_fit_engine(df, TRUE, NULL, order = "fit")$data$Variable),
               c("V2", "V3", "V1", "V4"))
  expect_error(.plot_fit_engine(df, TRUE, NULL, order = "worst"), "should be one of")
})
