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

test_that(".plot_summary_engine warns about unstandardised entries", {
  df <- data.frame(
    Measure = "Mean", Variable = factor("V1"), Simulated = 1, Target = 0,
    Centered = 1, SimulatedType = "Unstandardised Diff"
  )
  expect_warning(.plot_summary_engine(df, TRUE, "Variable"), "practically 0")
})

test_that(".plot_summary_engine labels the axes", {
  df <- data.frame(
    Measure = "Mean", Variable = factor("V1"), Simulated = 1, Target = 1,
    Centered = 0, SimulatedType = "Simulated"
  )
  expect_match(.plot_summary_engine(df, TRUE, "Variable", "MLR")$labels$title,
               "Standardised")
  expect_match(.plot_summary_engine(df, FALSE, "Variable")$labels$title,
               "Unstandardised")
})

test_that("plot_summary.stats2data_vec builds the comparison frame", {
  p <- quiet_plot(plot_summary(fx_vec()))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 4)
  expect_equal(levels(p$data$Variable), c("V1", "V2"))
  expect_s3_class(quiet_plot(plot_summary(fx_vec(), standardised = FALSE)), "ggplot")
})

test_that("plot_summary.stats2data_mlr covers regression, correlation and se", {
  p <- quiet_plot(plot_summary(fx_mlr()))
  expect_equal(sort(unique(p$data$Measure)),
               c("Correlation", "Regression Coefficient", "Standard Error"))
  p0 <- quiet_plot(plot_summary(fx_mlr(se = FALSE)))
  expect_false("Standard Error" %in% p0$data$Measure)
})

test_that("plot_summary.stats2data_mlr names unnamed correlations", {
  p <- quiet_plot(plot_summary(fx_mlr()))
  expect_true(all(c("Cor1", "Cor2", "Cor3") %in% as.character(p$data$Variable)))
})

test_that("plot_summary.stats2data_aov uses the effect names", {
  skip_if_not_installed("afex")
  p <- suppressMessages(quiet_plot(plot_summary(fx_aov())))
  expect_equal(as.character(p$data$Variable), "Factor1")
})

test_that("plot_summary methods validate their arguments", {
  expect_error(plot_summary(fx_vec(), standardised = "yes"), "single logical")
  expect_error(plot_summary(fx_vec(), eps = "a"), "single numeric")
  expect_error(plot_summary(fx_mlr(), standardised = "yes"), "single logical")
  expect_error(plot_summary(fx_mlr(), eps = "a"), "single numeric")
  expect_error(plot_summary(fx_aov(), standardised = "yes"), "single logical")
  expect_error(plot_summary(fx_aov(), eps = "a"), "single numeric")
})
