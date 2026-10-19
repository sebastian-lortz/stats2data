results_server <- function(x, code = "res <- 1") {
  list(result = shiny::reactive(x), call_code = shiny::reactive(code), file_stub = "test")
}

test_that("mod_results shows plots, text and data for single results", {
  for (x in list(fx_vec(), fx_mlr(), fx_aov())) {
    shiny::testServer(mod_results_server, args = results_server(x), {
      expect_type(output$best_objective, "character")

      session$setInputs(plot_fit = 1, standardised = FALSE, order = "fit", show_thresh = FALSE)
      expect_true(!is.null(output$plot$src))

      session$setInputs(plot_convergence = 1, first_iter = 0, log_y = TRUE, show_best = FALSE)
      expect_true(!is.null(output$plot$src))

      session$setInputs(plot_cooling = 1)
      expect_true(!is.null(output$plot$src))

      session$setInputs(get_rmse = 1)
      expect_match(output$text, "rmse")
      session$setInputs(summary = 1)
      expect_match(output$text, "stats2data")
      session$setInputs(display_data = 1)
      expect_match(output$data, "<table")
    })
  }
})

test_that("first_iter beyond the trajectory is capped instead of failing", {
  shiny::testServer(mod_results_server, args = results_server(fx_vec()), {
    session$setInputs(plot_convergence = 1, run = "2", first_iter = 1e6)
    expect_true(!is.null(output$plot$src))
  })
})

test_that("mod_results draws the MLR objective ratio", {
  shiny::testServer(mod_results_server, args = results_server(fx_mlr()), {
    session$setInputs(plot_convergence = 1, ratio = TRUE, show_mean = FALSE,
                      show_median = TRUE, show_final = FALSE, first_iter = 0)
    expect_true(!is.null(output$plot$src))
  })
})

test_that("mod_results handles parallel results and single runs of them", {
  shiny::testServer(mod_results_server, args = results_server(fx_parallel("aov", 3)), {
    expect_match(output$best_objective, "Run")

    session$setInputs(plot_stability = 1)
    expect_true(!is.null(output$plot$src))

    session$setInputs(plot_fit = 1, error_type = "sd")
    expect_true(!is.null(output$plot$src))

    session$setInputs(plot_convergence = 1)
    expect_error(output$plot, "single run")
    session$setInputs(run_index = "2")
    expect_true(!is.null(output$plot$src))
  })
})

test_that("downloads write the result, the data and the call", {
  code <- "set.seed(1L)\nres <- optim_vec(N = 6)"
  shiny::testServer(mod_results_server, args = results_server(fx_parallel("vec", 2), code), {
    expect_identical(readRDS(output$dl_object)$runs, 2)
    d <- utils::read.csv(output$dl_data)
    expect_identical(names(d), c("run", "V1", "V2"))
    expect_identical(nrow(d), 12L)
    expect_identical(readLines(output$dl_call), strsplit(code, "\n")[[1]])
  })
})
