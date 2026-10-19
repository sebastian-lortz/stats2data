#' results UI Function
#'
#' @description A shiny Module shared by the optim_vec, optim_aov and
#'   optim_mlr modules.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs toggleState toggle
#' @noRd
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("run_index_ui")),
    h5(arg_label("Best Objective",
                 "Lowest objective value reached by the run; 'converged' when it is below the convergence threshold.",
                 "best_objective")),
    tableOutput(ns("best_objective")),
    div(
      actionButton(ns("plot_fit"), name_with_info(
        "Plot Fit", "Simulated minus target statistics (plot_fit())."), class = "btn-sm"),
      actionButton(ns("plot_convergence"), name_with_info(
        "Plot Convergence", "Objective value across iterations (plot_convergence())."), class = "btn-sm"),
      actionButton(ns("plot_cooling"), name_with_info(
        "Plot Cooling", "Simulated-annealing temperature schedule (plot_cooling())."), class = "btn-sm"),
      actionButton(ns("plot_stability"), name_with_info(
        "Plot Stability", "Run-to-run and to-target RMSE across runs; needs more than one run (plot_stability())."), class = "btn-sm"),
      actionButton(ns("get_rmse"), name_with_info(
        "RMSE", "Unweighted RMSE between simulated and target statistics (get_rmse())."), class = "btn-sm"),
      actionButton(ns("summary"), name_with_info(
        "Summary", "Target versus simulated statistics (summary())."), class = "btn-sm"),
      actionButton(ns("display_data"), name_with_info(
        "Show Data", "First rows of the simulated data."), class = "btn-sm"),
      actionButton(ns("download"), name_with_info(
        "Download", "Result object, data, or the R code of the function call."), class = "btn-sm")
    ),
    uiOutput(ns("controls")),
    uiOutput(ns("main_output"))
  )
}


#' results Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param result Reactive returning the result object, or \code{NULL}.
#' @param call_code Reactive returning the R code that reproduces the result.
#' @param file_stub Character; prefix of the download file names.
#'
#' @noRd
mod_results_server <- function(id, result, call_code, file_stub) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    actions <- c("plot_fit", "plot_convergence", "plot_cooling", "plot_stability",
                 "get_rmse", "summary", "display_data")
    is_parallel <- function(x) inherits(x, "stats2data_parallel")
    iv <- function(id, default) isolate(input[[id]]) %||% default

    observe({
      x <- result()
      for (b in c(actions, "download")) shinyjs::toggleState(b, condition = !is.null(x))
      shinyjs::toggle("plot_stability", condition = is_parallel(x))
    })

    output$run_index_ui <- renderUI({
      x <- result()
      if (!is_parallel(x)) return(NULL)
      selectInput(
        ns("run_index"),
        arg_label("Run", "Show all runs together or a single run of parallel_optim()."),
        choices  = c("All runs" = "all",
                     stats::setNames(seq_len(x$runs), paste("Run", seq_len(x$runs)))),
        selected = "all", width = "180px"
      )
    })

    selected <- reactive({
      x <- result()
      req(x)
      if (!is_parallel(x)) return(x)
       i <- input$run_index
      if (is.null(i) || identical(i, "all")) x else x$results[[as.integer(i)]]
    })
    single <- function() {
      x <- selected()
      validate(need(!is_parallel(x),
                    "Select a single run above to show this output."))
      x
    }

    bo_row <- function(r) {
      b   <- unlist(r$best_objective)
      out <- ifelse(is.finite(b) & b < r$inputs$thresh, "converged", format(signif(b, 4)))
       nm  <- if (length(b) > 1L) colnames(r$data) else "Best Objective"
      as.data.frame(stats::setNames(as.list(out), nm), check.names = FALSE)
    }
    output$best_objective <- renderTable({
      x <- selected()
      if (is_parallel(x)) {
         cbind(Run = seq_along(x$results), do.call(rbind, lapply(x$results, bo_row)))
      } else {
        bo_row(x)
      }
    }, rownames = FALSE)

    last <- reactiveVal(NULL)
    lapply(actions, function(a) observeEvent(input[[a]], last(a)))

    output$controls <- renderUI({
      act <- last()
      req(act, result())
       x <- selected()
      thresh_box <- checkboxInput(ns("show_thresh"), arg_label(
        "Show Threshold", "Shade the region within the run's convergence threshold (unticked: thresh = NULL).",
        "thresh"),
        iv("show_thresh", TRUE))

      if (act == "plot_convergence" && !is_parallel(x)) {
        vec <- inherits(x, "stats2data_vec")
        mlr <- inherits(x, "stats2data_mlr")
        fluidRow(
          if (vec) column(3, selectInput(
            ns("run"), arg_label("Variable", "Variable whose objective-value trajectory is plotted.", "run"),
             choices = stats::setNames(seq_along(colnames(x$data)), colnames(x$data)),
            selected = iv("run", 1))),
          column(3, numericInput(
            ns("first_iter"), arg_label("Skip Iterations", "Number of initial iterations to skip.", "first_iter"),
            value = iv("first_iter", 0), min = 0, step = 100)),
          column(3,
                 checkboxInput(ns("log_y"), arg_label(
                   "Log Scale", "Base-10 logarithmic y axis.", "log_y"), iv("log_y", FALSE)),
                  if (mlr) checkboxInput(ns("ratio"), arg_label(
                   "Objective Ratio", "Plot the correlation-to-regression objective ratio instead of the objective value.",
                   "ratio"),
                   iv("ratio", FALSE))),
          column(3,
                 conditionalPanel(
                   "!input.ratio", ns = ns,
                   checkboxInput(ns("show_best"), arg_label(
                      "Mark Best", "Mark the iteration with the lowest objective value.", "show_best"),
                     iv("show_best", TRUE)),
                   thresh_box),
                 if (mlr) conditionalPanel(
                   "input.ratio", ns = ns,
                   checkboxInput(ns("show_mean"), arg_label(
                     "Show Mean", "Line at the mean ratio.", "show_mean"), iv("show_mean", TRUE)),
                   checkboxInput(ns("show_median"), arg_label(
                       "Show Median", "Line at the median ratio.", "show_median"), iv("show_median", TRUE)),
                   checkboxInput(ns("show_final"), arg_label(
                     "Show Final", "Line at the final ratio.", "show_final"), iv("show_final", TRUE))))
        )
      } else if (act == "plot_fit") {
        fluidRow(
          column(3, checkboxInput(ns("standardised"), arg_label(
            "Standardised", "Divide the differences by the target values.", "standardised"),
            iv("standardised", TRUE))),
           column(3, radioButtons(ns("order"), arg_label(
            "Order", "Parameter order: as entered, or worst fit first.", "order"),
            choices = c("As Entered" = "input", "By Fit" = "fit"),
            selected = iv("order", "input"), inline = TRUE)),
          column(3, thresh_box),
          if (is_parallel(x)) column(3, radioButtons(ns("error_type"), arg_label(
            "Error Bars", "Across-run error bars: min/max range or mean +/- SD.", "error_type"),
            choices = c("Range" = "range", "SD" = "sd"),
            selected = iv("error_type", "range"), inline = TRUE))
        )
      }
    })

    output$main_output <- renderUI({
      act <- last()
       req(act, result())
      switch(act,
             get_rmse     = ,
             summary      = verbatimTextOutput(ns("text")),
             display_data = tableOutput(ns("data")),
             plotOutput(ns("plot"), width = "650px", height = "450px"))
    })

    output$plot <- renderPlot({
      act <- last()
      req(act %in% c("plot_fit", "plot_convergence", "plot_cooling", "plot_stability"))
      show_thresh <- isTRUE(input$show_thresh %||% TRUE)

      switch(act,
             plot_fit = {
               x <- selected()
               thr <- if (is_parallel(x)) x$results[[1L]]$inputs$thresh else x$inputs$thresh
               plot_fit(x,
                        standardised = input$standardised %||% TRUE,
                        order        = input$order %||% "input",
                        thresh       = if (show_thresh) thr else NULL,
                        error_type   = input$error_type %||% "range")
             },
             plot_convergence = {
               x     <- single()
               ratio <- inherits(x, "stats2data_mlr") && isTRUE(input$ratio)
                run   <- 1L
               if (inherits(x, "stats2data_vec")) {
                 run <- as.integer(input$run %||% 1L)
                 if (is.na(run) || run > length(x$track_objective)) run <- 1L
               }
                tr <- if (ratio) x$track_objective_ratio else x$track_objective
               if (is.list(tr)) tr <- tr[[run]]
               fi <- suppressWarnings(as.integer(input$first_iter %||% 0L))
               if (length(fi) != 1L || is.na(fi)) fi <- 0L
               fi <- min(max(0L, fi), length(tr) - 1L)
                validate(need(!isTRUE(input$log_y) || any(is.finite(tr) & tr > 0),
                             "Log Scale needs positive values; this trajectory only contains 0 (or non-finite) values."))
               if (ratio) {
                 plot_convergence(x, ratio = TRUE, first_iter = fi,
                                  log_y       = isTRUE(input$log_y),
                                  show_mean   = input$show_mean   %||% TRUE,
                                   show_median = input$show_median %||% TRUE,
                                  show_final  = input$show_final  %||% TRUE)
                } else {
                 plot_convergence(x, run = run, first_iter = fi,
                                  log_y     = isTRUE(input$log_y),
                                  show_best = input$show_best %||% TRUE,
                                  thresh    = if (show_thresh) x$inputs$thresh else NULL)
               }
             },
             plot_cooling = plot_cooling(single()),
             plot_stability = {
               x <- result()
                validate(need(is_parallel(x), "Plot Stability needs more than one run."))
               plot_stability(x)
             }
      )
    })

    output$text <- renderPrint({
      x <- selected()
      if (identical(last(), "get_rmse")) get_rmse(x) else summary(x)
    })

    output$data <- renderTable({
      d <- as.data.frame(single()$data)
      utils::head(d, 15)
    }, rownames = TRUE)

    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download",
        p("The result object is the full ", tags$code(class(result())[1]),
          " object (read it with ", tags$code("readRDS()"), "). The data file holds the ",
          "simulated data of all runs (with a ", tags$code("run"), " column when there is more than one run). ",
          "The function call is R code that reproduces this result."),
        downloadButton(ns("dl_object"), "Result object (.rds)"),
         downloadButton(ns("dl_data"),   "Simulated data (.csv)"),
        downloadButton(ns("dl_call"),   "Function call (.txt)"),
        tags$hr(),
        tags$pre(class = "s2d-call", call_code()),
        size = "l", easyClose = TRUE, footer = modalButton("Close")
      ))
    })

    output$dl_object <- downloadHandler(
      filename = function() paste0(file_stub, "_result.rds"),
      content  = function(file) saveRDS(result(), file)
    )
    output$dl_data <- downloadHandler(
      filename = function() paste0(file_stub, "_data.csv"),
       content  = function(file) {
        x <- result()
        d <- if (is_parallel(x)) {
          do.call(rbind, lapply(seq_along(x$results), function(i) {
            cbind(run = i, as.data.frame(x$results[[i]]$data))
          }))
        } else {
           as.data.frame(x$data)
        }
         utils::write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_call <- downloadHandler(
      filename = function() paste0(file_stub, "_call.txt"),
      content  = function(file) writeLines(call_code(), file)
    )
  })
}
