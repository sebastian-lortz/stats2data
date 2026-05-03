#' optim_vec UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs disable enable
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table
#' @noRd
NULL

mod_optim_vec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
    .rhandsontable .ht_master .wtHolder,
    .rhandsontable .ht_master .handsontable {
      overflow: visible !important;
    }
    .rhandsontable .ht_master .htDropdownMenu,
    .rhandsontable .ht_master .htContextMenu {
      z-index: 9999 !important;
    }
    .rhandsontable .ht_master .htDropdownMenu .dropdown-menu {
      max-height: none !important;
      white-space: normal !important;
    }
    .rhandsontable .ht_master .htDropdownMenu table {
      width: auto !important;
    }
    .selectize-dropdown,
    .selectize-dropdown-content {
      overflow: visible !important;
      z-index: 9999 !important;
      max-height: none !important;
      white-space: normal !important;
    }
  "))
    ),
    div(style = "white-space: nowrap;",
        div(
          style = "display:inline-block; vertical-align:top; margin-right:10px;",
          wellPanel(
            h4("Reported Summary Statistics"),
            numericInput(ns("N"),  name_with_info(
              "Sample Size",
              "The length of the target vectors.")
              , 864, min = 5, step = 1),
            p(name_with_info(
              "Descriptive Statistics",
              "The variables' name, mean, SD, minimum, maximum, and if integer/continuous")),
            div(style = "width:100%; overflow-x:auto;",
                rHandsontableOutput(ns("param_table"), width = "100%")
            ),
            fluidRow(
              column(6, actionButton(ns("add_row"),    "Add row",    class = "btn-sm btn-block")),
              column(6, actionButton(ns("remove_row"), "Remove row", class = "btn-sm btn-block"))
            )
          )
        ),
        div(
          style = "display:inline-block; vertical-align:top; margin-right:20px;",
          wellPanel(
            h4("Algorithm Hyperparameters"),
            numericInput(
              ns("thresh"),
              name_with_info(
                "thresh",
                "The threshold for the objective function value below which the optimization will stop."),
              value = 1e-2,
              min   = 0,
              step  = 1e-3,
              width = "100%"
            ),
            numericInput(ns("max_iter"), name_with_info(
              "Max Iterations",
              "The maximum number of iterations the algorithm will run each time it restarts and for each variable."), 1e5,   min = 1,    step = 1000),
            numericInput(ns("init_temp"), name_with_info(
              "Initial Temperature",
              "The starting temperature for the simulated annealing, which sets the initial likelihood of accepting worse solutions in the first start."),
              1e-3,  min = 0, max = 1,   step = 0.001),
            numericInput(ns("cooling_rate"), name_with_info(
              "Cooling Rate",
              "The factor by which the temperature is multiplied after each iteration, governing how quickly the algorithm reduces its acceptance of worse solutions."),
              (1e5-10)/1e5, min = 0, max = 1, step = 0.0001),
            numericInput(ns("max_starts"), name_with_info(
              "Max Starts",
              "The maximum number of times the optimization algorithm will restart from the current best solution using reduced initial temperatures."),     3,     min = 1,    step = 1)
          ),
        ),
        div(style = "display:inline-block; vertical-align:top; margin-left:20px;",
            h4("Optimization Output"),
            fluidRow(
              column(
                width = 12,
                div(style = "display:inline-block;",
                    actionButton(ns("run"),       "Run Optimization", class = "btn-primary")
                )
              )
            ),
            div(
              id    = ns("processing_msg"),
              style = "display:none; margin:10px; font-weight:bold; color:#337ab7;",
              "Processing, please wait ..."
            ),
            textOutput(ns("status_text")),

            h5(name_with_info(
              "Objective Function Value",
              "The minimum value of the objective function attained by the optimization.")),
            tableOutput(ns("best_errors")),
            fluidRow( style = "margin-bottom: 10px;",
                      column(12,
                             actionButton(ns("plot_summary"),    name_with_info("Plot Summary","Plot summary differences"), class = "btn-sm"),
                             actionButton(ns("plot_error"),      name_with_info("Plot Errors","Show objective value trajectory"), class = "btn-sm"),
                             actionButton(ns("plot_cooling"),    name_with_info("Plot Cooling","Show temperature schedule"), class = "btn-sm"),
                             actionButton(ns("get_rmse"),        name_with_info("Get RMSE","Compute RMSE"), class = "btn-sm"),
                             actionButton(ns("display_data"),    name_with_info("Display Data","Show head of simulated data"), class = "btn-sm"),
                             actionButton(ns("download"),        name_with_info("Download","Download data or full object"), class = "btn-sm")
                      )
            ),

            div(style = "overflow:visible; margin-top:20px;", uiOutput(ns("main_output")))
        )
    )
  )
}

mod_optim_vec_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      params = data.frame(
        Variable = paste0("V", 1:5),
        Mean     = c(10.53, 0.27, 2.49, -0.64, 0.03),
        SD       = c( 7.82,  0.45,11.68, 1.85,  2.05),
        Min      = c( 0,     0,  -24,  -3,    -3),
        Max      = c(36,     1,   24,   3,     3),
        Integer  = rep(TRUE, 5),
        stringsAsFactors = FALSE
      ),
      result = NULL,
      status = "ready",
      last   = NULL,
      dirty  = TRUE
    )

    observeEvent({
      list(
        input$N,
        input$param_table,
        input$add_row,
        input$remove_row,
        input$thresh,
        input$max_iter,
        input$init_temp,
        input$cooling_rate,
        input$max_starts)
    }, {
      rv$dirty <- TRUE
      rv$last <- NULL
      for (btn in c(
        "plot_error","get_rmse",
        "plot_summary","plot_cooling",
        "display_data","download"
      )) {
        shinyjs::disable(btn)
      }
    })

    output$param_table <- renderRHandsontable({
      tbl <- rhandsontable(rv$params, rowHeaders = NULL)
      hot_col(tbl, "Integer", type = "checkbox")
    })
    observeEvent(input$param_table, {
      rv$params <- hot_to_r(input$param_table)
    })

    observeEvent(input$add_row, {
      n <- nrow(rv$params) + 1
      rv$params <- rbind(rv$params, data.frame(
        Variable = paste0("V", n),
        Mean     = 4,
        SD       = 1,
        Min      = 0,
        Max      = 9,
        Integer  = TRUE,
        stringsAsFactors = FALSE
      ))
    })
    observeEvent(input$remove_row, {
      if (nrow(rv$params) > 1)
        rv$params <- rv$params[-nrow(rv$params), ]
    })

    observeEvent(input$max_iter, {
      updateNumericInput(session, "cooling_rate",
                         value = max(0, min(1, (input$max_iter - 10)/input$max_iter))
      )
    })

    shinyjs::disable("run")
    lapply(c("plot_error","get_rmse","plot_summary","plot_cooling","display_data","download"),
           shinyjs::disable
    )

    observe({
      df <- rv$params
      ok <- all(
        nzchar(df$Variable),
        !is.na(df$Mean), !is.na(df$SD),
        !is.na(df$Min),  !is.na(df$Max)
      )
      if (ok) shinyjs::enable("run") else shinyjs::disable("run")
    })

    observeEvent(input$run, {
      shinyjs::show("processing_msg")
      on.exit(shinyjs::hide("processing_msg"), add = TRUE)
      rv$status <- "running"
      df  <- rv$params
      N            <- input$N
      thresh    <- input$thresh
      max_iter     <- input$max_iter
      init_temp    <- input$init_temp
      cooling_rate <- input$cooling_rate
      max_starts   <- input$max_starts
      target_mean  <- stats::setNames(df$Mean, df$Variable)
      range        <- rbind(df$Min, df$Max)
      input.check <- check_vec_inputs(
        N = N,
        target_mean = target_mean,
        range = range,
        thresh = thresh,
        max_iter = max_iter,
        init_temp = init_temp,
        cooling_rate = cooling_rate,
        max_starts = max_starts
      )
      if (!input.check) {return()}
      for (tbl in c("param_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"none","opacity":0.5});',
                  ns(tbl))
        )
      }
      shinyjs::disable("run")
      lapply(c("N", "param_table", "add_row", "remove_row",
               "thresh", "max_iter", "init_temp", "cooling_rate",
               "max_starts",
               "plot_error","get_rmse","plot_summary","plot_cooling",
               "display_data","download"),
             shinyjs::disable
      )
      tryCatch({
        withProgress(message = "Running optimization...", value = 0, {
          rv$result <- optim_vec(
            N             = N,
            target_mean   = target_mean,
            target_sd     = stats::setNames(df$SD, df$Variable),
            range         = range,
            integer       = df$Integer,
            thresh     = thresh,
            sprite_prec   = c(2, 2),
            max_iter      = max_iter,
            init_temp     = init_temp,
            cooling_rate  = cooling_rate,
            max_starts    = max_starts,
            progress_mode = "shiny"
          )
        })
      }, error = function(e) {
        showNotification(
          paste("Optimization failed:", conditionMessage(e)),
          type = "error", duration = 10
        )
        return()
      })
      if (is.null(rv$result)) {
        shinyjs::enable("run")
        lapply(c("N", "param_table", "add_row", "remove_row",
                 "thresh", "max_iter", "init_temp", "cooling_rate",
                 "max_starts"),
               shinyjs::enable)
        for (tbl in c("param_table")) {
          shinyjs::runjs(
            sprintf('$("#%s .ht_master").css({"pointer-events":"auto","opacity":1});',
                    ns(tbl)))
        }
        return()
      }
      # Show per-variable warnings for SPRITE/feasibility failures
      if (!is.null(rv$result$error_msgs)) {
        for (v in seq_along(rv$result$error_msgs)) {
          msg <- rv$result$error_msgs[[v]]
          if (!is.null(msg)) {
            var_name <- rv$params$Variable[v]
            showNotification(
              paste0("Variable '", var_name, "': ", msg,
                     " Consider adjusting the SD, range, or switching to continuous."),
              type = "warning", duration = 15
            )
          }
        }
      }
      rv$status <- "done"
      rv$dirty <- FALSE
      shinyjs::enable("run")
      lapply(c("N", "param_table", "add_row", "remove_row",
               "thresh", "max_iter", "init_temp", "cooling_rate",
               "max_starts",
               "plot_error","get_rmse","plot_summary","plot_cooling",
               "display_data","download"),
             shinyjs::enable
      )
      for (tbl in c("param_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"auto","opacity":1});',
                  ns(tbl))
        )
      }
    })

    output$status_text <- renderText({
      if (rv$dirty) return(NULL)
      if (rv$status == "running") "Optimization is running..." else ""
    })

    output$best_errors <- renderTable({
      if (rv$dirty) return(NULL)
      vars <- rv$params$Variable
      if (rv$status != "done") {
        df <- as.data.frame(as.list(rep("hold", length(vars))), stringsAsFactors = FALSE)
        colnames(df) <- vars
        return(df)
      }
      bes  <- unlist(rv$result$best_error)
      disp <- ifelse(bes < input$thresh, "converged", format(bes))
      df <- as.data.frame(as.list(disp), stringsAsFactors = FALSE)
      colnames(df) <- rv$params$Variable[seq_along(disp)]
      df
    }, rownames = FALSE)

    observeEvent(input$plot_error, rv$last <- reactive("traj"))
    observeEvent(input$get_rmse,       rv$last <- reactive("rmse"))
    observeEvent(input$plot_summary,    rv$last <- reactive("summary"))
    observeEvent(input$plot_cooling,    rv$last <- reactive("cooling"))
    observeEvent(input$display_data,    rv$last <- reactive("data"))
    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download",
        downloadButton(ns("dl_object"), "Full object"),
        downloadButton(ns("dl_data"),   "Data CSV"),
        easyClose = TRUE
      ))
    })

    observeEvent(rv$params$Variable, {
      if (!is.null(input$run_select)) {
        vars <- rv$params$Variable
        sel  <- input$run_select
        if (is.null(sel) || !(sel %in% vars)) {
          sel <- vars[1]
        }
        updateSelectizeInput(
          session,
          "run_select",
          choices  = vars,
          selected = sel,
          options  = list(dropdownParent = "body")
        )
      }
    })

    output$main_output <- renderUI({
      if (rv$dirty) return(NULL)
      req(rv$last)
      vars <- rv$params$Variable
      old     <- isolate(input$run_select)
      run_id  <- if (!is.null(old) && old %in% vars) old else vars[1]
      idx     <- match(run_id, vars)

      all_runs <- seq_along(rv$result$track_error)
      idx      <- match(run_id, vars, nomatch = 1)
      if (!(idx %in% all_runs)) idx <- all_runs[1]

      n_iters <- length(rv$result$track_error[[idx]])
      max_it  <- if (n_iters >= 1) n_iters else 1

      switch(
        rv$last(),
        traj = tagList(
          plotOutput(ns("error_plot"), width = "600px", height = "400px"),
          fluidRow(
            column(
              width = 3,
              selectizeInput(
                ns("run_select"),
                name_with_info("Variable", "Select which variable's (i.e. run's) error trajectory to plot."),
                choices = vars,
                selected = run_id,
                options = list(dropdownParent = 'body'),
                width = "100px"
              ),
            ),
            column(
              width = 3,
              numericInput(
                ns("iter_select"),
                name_with_info(
                  "Start Iteration",
                  "Plot errors beginning at this iteration."
                ),
                value = isolate(input$iter_select %||% 1),
                min   = 1,
                max   = max_it,
                step  = 100,
                width = "100px"
              )
            )
          )
        ),
        rmse    = verbatimTextOutput(ns("rmse_out")),
        summary = tagList(
          plotOutput(ns("summary_plot"), width = "600px", height = "400px"),
          fluidRow(
            column(
              width = 6,
              checkboxInput(
                ns("std"),
                name_with_info(
                  "Standardized",
                  "Select whether the differences between simulated and target values are standardized."
                ),
                TRUE,
                width = "100%"
              ),
            ))),
        cooling = plotOutput(ns("cooling_plot"), width = "600px", height = "400px"),
        data    = tableOutput(ns("data_preview"))
      )
    })

    output$error_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      req(input$run_select)
      plot_error(
        rv$result,
        run        = which(rv$params$Variable == input$run_select),
        first_iter = as.integer(input$iter_select)
      )
    })
    output$rmse_out     <- renderPrint({
      if (rv$dirty) return(NULL)
      get_rmse(rv$result) })
    output$summary_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_summary(rv$result,
                   standardised = input$std) })
    output$cooling_plot <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_cooling(rv$result) })
    output$data_preview <- renderTable({
      if (rv$dirty) return(NULL)
      utils::head(as.data.frame(rv$result$data), min(nrow(rv$result$data),15))
    }, rownames = TRUE)
    output$dl_object <- downloadHandler(
      filename = "stats2data_object.rds",
      content  = function(file) {
        req(!rv$dirty)
        saveRDS(rv$result, file)
      }
    )
    output$dl_data <- downloadHandler(
      filename = "optimized_data.csv",
      content  = function(file) {
        req(!rv$dirty)
        utils::write.csv(as.data.frame(rv$result$data), file, row.names = FALSE)
      }
    )
  })
}
