#' optim_lm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @import shiny
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table hot_cell
#' @noRd
mod_optim_lm_ui <- function(id) {
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
  "))
    ),

    div(style = "white-space: nowrap;",
        div(
          style = "display:inline-block; vertical-align:top; margin-right:10px;",
          wellPanel(
            h4("Reported Summary Statistics"),
            tags$hr(),
            p(tags$b(name_with_info(
              "Descriptives",
              "The data simulated by the descriptives module, matching means and standard deviations."
            ))),
            actionButton(ns("match_descr"),
                         "Start Descriptives Module",
                         class = "btn-sm btn-info"),
            tags$div(
              style = "
                margin-top: 6px;
                font-size: 0.8em;
                color: #444;
                line-height: 1.1;
              ",
              tags$style(HTML("
                .shrink-p p { margin: 2px 0; }
              ")),
              div(class = "shrink-p", uiOutput(ns("var_names")))
            ),
            tags$hr(),
            p(tags$b(name_with_info(
              "Correlations",
              "The correlation matrix containing the upper triangle of the reported bivariate correlations. If a value is not available, the cell is supposed to be blank."
            ))),
            div(
              style = "width:100%; overflow-x: auto;",
              rHandsontableOutput(ns("corr_table"), width = "100%")
            ),
            tags$hr(),
            p(tags$b(name_with_info(
              "Regression Model",
              "The model specifications determining the formula of the reported regression model."
            ))),
            tags$div(
              style = "margin-top: 6px; font-style: italic; color: #555;",
              textOutput(ns("lm_formula"))
            ),
            selectInput(
              ns("lm_outcome"),
              label = tags$span(style = "font-size:90%;", "Dependent Variable:"),
              choices = NULL
            ),
            fluidRow(
              column(
                width = 6,
                checkboxGroupInput(
                  ns("lm_predictors"),
                  label = tags$span(style = "font-size:90%;", "Predictors:"),
                  choices = NULL
                )
              ),
              column(
                width = 6,
                checkboxGroupInput(
                  ns("lm_interactions"),
                  label = tags$span(style = "font-size:90%;", "Interactions:"),
                  choices = NULL
                )
              )
            ),
            p(tags$b(name_with_info(
              "Regression Table",
              "The reported unstandardized regression coefficients (estimates) and their standard errors (SE). If a value is not available, the cell is supposed to be blank."
            ))),
            div(
              style = "margin-top:10px; overflow-x:auto; width:100%;",
              rHandsontableOutput(ns("coef_table"), width = "100%")
            ))
        ),

        div(
          style = "display:inline-block; vertical-align:top; margin-right:20px;",
          wellPanel(
            h4("Algorithm Hyperparameters"),
            numericInput(
              ns("tolerance"),
              name_with_info(
                "Tolerance",
                "The threshold for the weighted objective function value below which the optimization will stop."),
              value = 1e-3,
              min   = 0,
              step  = 1e-4,
              width = "100%"
            ),
            numericInput(ns("max_iter"), name_with_info(
              "Max Iterations",
              "The maximum number of iterations the algorithm will run each time it restarts."), 1e5,   min = 1,    step = 1000),
            numericInput(ns("init_temp"), name_with_info(
              "Initial Temperature",
              "The starting temperature for the simulated annealing. Leave at default for automatic calibration."),
              NA, min = 0, max = 100,    step = 0.01),
            numericInput(ns("cooling_rate"), name_with_info(
              "Cooling Rate",
              "The factor by which the temperature is multiplied after each iteration."),
              (1e5-10)/1e5, min = 0, max = 1, step = 0.0001),
            numericInput(ns("hill_climbs"), name_with_info(
              "Hill Climbs",
              "The number of hill climbing iterations for further refinement."), 1e4,   min = 0,    step = 1000),
            numericInput(ns("max_starts"), name_with_info(
              "Max Starts",
              "The maximum number of times the optimization algorithm will restart."),
              3,     min = 1,    step = 1),
            numericInput(ns("n_datasets"),
                         name_with_info(
                           "Number of Datasets",
                           "How many independent sequential optimization runs to perform."),
                         value = 1, min = 1, step = 1),
            tags$hr(),
            h5(name_with_info(
              "Weights of Objective Function",
              "The weights multiplied with each term in the objective function.")),
            div(style = "width:100%; margin-bottom:10px; overflow-x:auto;",
                rHandsontableOutput(ns("weight_table"), width = "100%")
            )
          )
        ),

        div(style = "display:inline-block; vertical-align:top; margin-left:20px; width: calc(100% - auto);",
            h4("Optimization Output"),
            div(style = "margin-bottom:10px;",
                actionButton(ns("run"), name_with_info(
                  "Run Optimization",
                  "Executes nds3: Data-Simulation via iterative stochastic combinatorial optimization using reported summary estimates."), class = "btn-primary")
            ),
            div(
              id    = ns("processing_msg"),
              style = "display:none; margin:10px; font-weight:bold; color:#337ab7;",
              "Processing, please wait ..."
            ),
            textOutput(ns("status_text")),
            selectInput(ns("dataset_selector"), name_with_info("Select Dataset", "Choose the data set to inspect or download."),
                        choices = NULL, selected = 1,   width    = "100px"),
            h5(name_with_info("Objective Function Value","The minimum weighted value of the objective function attained by the optimization.")),
            tableOutput(ns("best_error")),
            fluidRow( style = "margin-bottom: 10px;",
                      column(12,
                             actionButton(ns("plot_summary"),    name_with_info("Plot Summary","Plot summary differences"), class = "btn-sm"),
                             actionButton(ns("plot_error"),      name_with_info("Plot Errors","Show objective value trajectory"), class = "btn-sm"),
                             actionButton(ns("plot_error_ratio"),name_with_info("Plot Error Ratio","Show trajectory of objective ratio correlations/regression"), class = "btn-sm"),
                             actionButton(ns("plot_cooling"),    name_with_info("Plot Cooling","Show temperature schedule"), class = "btn-sm"),
                             actionButton(ns("get_rmse"),        name_with_info("Get RMSE","Compute unweighted RMSE"), class = "btn-sm")
                      )
            ),
            fluidRow(
              column(12,
                     actionButton(ns("display_data"),    name_with_info("Display Data","Show head of simulated data"), class = "btn-sm"),
                     actionButton(ns("download"),        name_with_info("Download","Download data or full object"), class = "btn-sm")
              )
            ),
            div(style = "overflow:auto; margin-top:10px;", uiOutput(ns("main_output")))
        )
    )
  )
}


#' optim_lm Server Functions
#'
#' @noRd
mod_optim_lm_server <- function(id, root_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      dirty       = TRUE,
      result      = NULL,
      status      = "ready"
    )

    observeEvent({
      list(
        input$corr_table,
        input$lm_outcome,
        input$lm_predictors,
        input$lm_interactions,
        input$tolerance,
        input$max_iter,
        input$init_temp,
        input$cooling_rate,
        input$hill_climbs,
        input$max_starts,
        input$n_datasets,
        input$weight_table)
    }, {
      rv$dirty <- TRUE
      for (btn in c(
        "plot_error", "plot_error_ratio", "get_rmse",
        "plot_summary", "plot_cooling",
        "display_data", "download", "dataset_selector"
      )) {
        shinyjs::disable(btn)
      }
    })

    simulated_data <- reactive({
      root_session$userData$lm_data()
    })

    output$var_names <- renderUI({
      df <- simulated_data()
      if (is.data.frame(df) && ncol(df) > 0) {
        tagList(
          tags$p("Data available!"),
          tags$p("Variables: ", paste(colnames(df), collapse = ", "))
        )
      } else {
        tagList(
          tags$p("No simulated data found."),
          tags$p("Run the Descriptives module with LM-format data."),
        )
      }
    })

    observe({
      df <- simulated_data()
      if (!is.data.frame(df) || ncol(df) == 0) {
        return(NULL)
      }
      updateSelectInput(
        session,
        "lm_outcome",
        choices  = colnames(df),
        selected = colnames(df)[length(colnames(df))]
      )
      preds <- setdiff(colnames(df), colnames(df)[1])
      updateCheckboxGroupInput(
        session,
        "lm_predictors",
        choices  = preds,
        selected = preds
      )
      updateCheckboxGroupInput(
        session,
        "lm_interactions",
        choices  = character(0),
        selected = character(0)
      )
    })

    observeEvent(input$lm_outcome, {
      df <- simulated_data()
      if (!is.data.frame(df)) return(NULL)
      out_var  <- input$lm_outcome
      all_vars <- colnames(df)
      new_preds <- setdiff(all_vars, out_var)
      updateCheckboxGroupInput(
        session,
        "lm_predictors",
        choices  = new_preds,
        selected = new_preds
      )
      updateCheckboxGroupInput(
        session,
        "lm_interactions",
        choices  = character(0),
        selected = character(0)
      )
    })

    observeEvent(input$lm_predictors, {
      sel_preds <- input$lm_predictors
      if (length(sel_preds) < 2) {
        updateCheckboxGroupInput(
          session,
          "lm_interactions",
          choices  = character(0),
          selected = character(0)
        )
        return()
      }
      inters <- utils::combn(
        sel_preds,
        2,
        FUN = function(x) paste(x, collapse = ":"),
        simplify = TRUE
      )
      if (length(sel_preds) == 4) {
        default <- intersect(c("V1:V3", "V2:V3"), inters)
      } else {
        default <- NULL
      }
      updateCheckboxGroupInput(
        session,
        "lm_interactions",
        choices  = inters,
        selected = default
      )
    }, ignoreNULL = FALSE)

    lm_formula_reactive <- reactive({
      out_var <- input$lm_outcome
      preds   <- input$lm_predictors
      inters  <- input$lm_interactions
      if (is.null(out_var) || length(preds) == 0) {
        return(NULL)
      }
      rhs_terms <- preds
      if (!is.null(inters) && length(inters) > 0) {
        rhs_terms <- c(rhs_terms, inters)
      }
      rhs <- paste(rhs_terms, collapse = " + ")
      stats::as.formula(paste(out_var, "~", rhs))
    })

    output$lm_formula <- renderText({
      frm <- lm_formula_reactive()
      if (is.null(frm)) {
        "Please select at least one predictor."
      } else {
        paste(deparse(frm), collapse = "")
      }
    })

    corr_df_reactive <- reactive({
      df <- simulated_data()
      if (!is.data.frame(df)) return(NULL)
      vars <- colnames(df)
      mat <- matrix(NA_real_, nrow = length(vars), ncol = length(vars))
      rownames(mat) <- vars
      colnames(mat) <- vars
      if (length(vars) == 5) {
        mat[upper.tri(mat)] <-  c(0.011, -0.177, 0.091, 0.035, 0.114, 0.246, -0.110, 0.119, 0.357, 0.263)
      }
      as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$corr_table <- renderRHandsontable({
      df_corr <- corr_df_reactive()
      if (is.null(df_corr)) return(NULL)
      tbl <- rhandsontable(df_corr, rowHeaders = colnames(df_corr), width = "100%", height = "200px") %>%
        hot_table(overflow = "visible") %>%
        hot_col(col = colnames(df_corr), format = "0.000")
      n <- ncol(df_corr)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          if (i >= j) {
            tbl <- hot_cell(
              tbl,
              row      = i,
              col      = j,
              readOnly = TRUE
            )
          }
        }
      }
      tbl
    })

    coef_df_reactive <- reactive({
      frm <- lm_formula_reactive()
      if (is.null(frm)) return(NULL)
      term_labels <- attr(stats::terms(frm), "term.labels")
      all_terms   <- c("(Intercept)", term_labels)
      mat <- matrix(
        NA_real_,
        nrow = 2,
        ncol = length(all_terms),
        dimnames = list(c("Est.", "SE"), all_terms)
      )
      if (length(all_terms) == 7) {
        mat[1,] <- c(0.115, -0.016, 0.292, 0.052, 0.203, 0.000, 0.009)
        mat[2,] <- c(0.123, 0.008, 0.151, 0.009, 0.036, 0.001, 0.012)
      }
      as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE)
    })

    output$coef_table <- renderRHandsontable({
      df <- coef_df_reactive()
      if (is.null(df)) return(NULL)
      rhandsontable(df, rowHeaders = c("Est.", "SE"), width = "100%", height = "200px") %>%
        hot_table(overflow = "visible") %>%
        hot_col(col = colnames(df), format = "0.000")
    })

    observeEvent(input$match_descr, {
      updateNavbarPage(
        root_session,
        inputId  = "main",
        selected = "Descriptives"
      )
    })

    observeEvent(input$max_iter, {
      updateNumericInput(session, "cooling_rate",
                         value = max(0, min(1, (input$max_iter - 10)/input$max_iter))
      )
    })

    weight_df <- reactiveVal(data.frame(
      Correlation = 1,
      Regression   = 1,
      stringsAsFactors = FALSE
    ))

    output$weight_table <- renderRHandsontable({
      tbl <- rhandsontable(weight_df(), rowHeaders = NULL, width = "100%") %>%
        hot_table(stretchH = "all") %>%
        hot_col("Correlation",  title = "Correlation",  format = "0.000") %>%
        hot_col("Regression",    title = "Regression",    format = "0.000")
      tbl
    })

    for (btn in c(
      "run", "plot_error", "plot_error_ratio", "get_rmse",
      "plot_summary", "plot_cooling",
      "display_data", "download", "dataset_selector"
    )) {
      shinyjs::disable(btn)
    }

    observe({
      preds_ok <- length(input$lm_predictors) > 0
      tbl <- hot_to_r(input$coef_table)
      coef_present <- !is.null(tbl) &&
        any(!is.na(as.numeric(tbl["Est.", ])))
      if (preds_ok && coef_present) {
        shinyjs::enable("run")
      } else {
        shinyjs::disable("run")
      }
    })

    observeEvent(input$run, {
      shinyjs::show("processing_msg")
      on.exit(shinyjs::hide("processing_msg"), add = TRUE)

      sim_data <- simulated_data()
      req(is.data.frame(sim_data), ncol(sim_data) > 0)

      # Extract inputs from UI
      df_corr <- hot_to_r(input$corr_table)
      mat_corr <- as.matrix(df_corr)
      target_cor <- mat_corr[upper.tri(mat_corr, diag = FALSE)]

      coef_df    <- hot_to_r(input$coef_table)
      estimates  <- as.numeric(coef_df["Est.", ])
      target_reg <- estimates
      names(target_reg) <- colnames(coef_df)

      ses       <- as.numeric(coef_df["SE", ])
      target_se <- ses
      names(target_se) <- colnames(coef_df)
      # If all SE are NA, set to NULL
      if (all(is.na(target_se))) target_se <- NULL

      reg_equation <- paste(deparse(lm_formula_reactive()), collapse = "")

      # Derive N, target_mean, target_sd, range, integer from sim_data
      N           <- nrow(sim_data)
      vars        <- colnames(sim_data)
      target_mean <- vapply(sim_data, mean, numeric(1))
      target_sd   <- vapply(sim_data, stats::sd, numeric(1))
      # Infer integer status
      is_int      <- vapply(sim_data, function(x) all(abs(x - round(x)) < 1e-8), logical(1))
      # Infer range from data (use data min/max)
      range_mat   <- rbind(
        vapply(sim_data, min, numeric(1)),
        vapply(sim_data, max, numeric(1))
      )

      max_iter     <- input$max_iter
      init_temp    <- input$init_temp
      if (is.na(init_temp)) init_temp <- NULL
      cooling_rate <- input$cooling_rate
      tolerance    <- input$tolerance
      max_starts   <- input$max_starts
      hill_climbs  <- input$hill_climbs
      n_datasets   <- input$n_datasets

      wdf    <- hot_to_r(input$weight_table)
      weight <- c(wdf$Correlation, wdf$Regression)

      # Input checks
      input.check <- check_lm_inputs(
        tolerance    = tolerance,
        max_iter     = max_iter,
        init_temp    = if (is.null(init_temp)) 1 else init_temp,
        cooling_rate = cooling_rate,
        hill_climbs  = hill_climbs,
        max_starts   = max_starts
      )
      if (!input.check) {return()}

      # Disable all inputs
      for (tbl in c("corr_table","coef_table","weight_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"none","opacity":0.5});',
                  ns(tbl))
        )
      }
      for (btn in c(
        "match_descr", "corr_table", "lm_outcome", "lm_predictors", "lm_interactions",
        "coef_table_ht", "tolerance", "max_iter", "init_temp", "cooling_rate",
        "hill_climbs", "max_starts", "n_datasets",
        "weight_table", "run", "plot_error",
        "plot_error_ratio", "get_rmse",
        "plot_summary", "plot_cooling",
        "display_data", "download", "dataset_selector"
      )) {
        shinyjs::disable(btn)
      }
      rv$status <- "running"

      withProgress(message = "Running optimization...", value = 0, {
        if (n_datasets > 1) {
          results_list <- vector("list", n_datasets)
          for (ds in seq_len(n_datasets)) {
            incProgress(1 / n_datasets,
                        detail = sprintf("Dataset %d / %d", ds, n_datasets))
            results_list[[ds]] <- optim_mlr(
              N            = N,
              target_mean  = target_mean,
              target_sd    = target_sd,
              range        = range_mat,
              integer      = is_int,
              target_cor   = target_cor,
              target_reg   = target_reg,
              reg_equation = reg_equation,
              sprite_prec  = c(2, 2),
              target_se    = target_se,
              weight       = weight,
              tolerance    = tolerance,
              max_iter     = max_iter,
              init_temp    = init_temp,
              cooling_rate = cooling_rate,
              max_starts   = max_starts,
              hill_climbs  = hill_climbs,
              progress_mode = "off"
            )
          }
          rv$result <- results_list
        } else {
          rv$result <- optim_mlr(
            N            = N,
            target_mean  = target_mean,
            target_sd    = target_sd,
            range        = range_mat,
            integer      = is_int,
            target_cor   = target_cor,
            target_reg   = target_reg,
            reg_equation = reg_equation,
            sprite_prec  = c(2, 2),
            target_se    = target_se,
            weight       = weight,
            tolerance    = tolerance,
            max_iter     = max_iter,
            init_temp    = init_temp,
            cooling_rate = cooling_rate,
            max_starts   = max_starts,
            hill_climbs  = hill_climbs,
            progress_mode = "shiny"
          )
        }
      })

      is_multi <- n_datasets > 1
      rv$status <- "done"
      rv$dirty  <- FALSE
      shinyjs::enable("run")
      for (btn in c(
        "match_descr", "corr_table", "lm_outcome", "lm_predictors", "lm_interactions",
        "coef_table_ht", "tolerance", "max_iter", "init_temp", "cooling_rate",
        "hill_climbs", "max_starts", "n_datasets",
        "weight_table", "run", "plot_error",
        "plot_error_ratio", "get_rmse",
        "plot_summary", "plot_cooling",
        "display_data", "download"
      )) {
        shinyjs::enable(btn)
      }
      if (is_multi) {
        updateSelectInput(session, "dataset_selector",
                          choices = seq_len(n_datasets), selected = 1)
        shinyjs::enable("dataset_selector")
      } else {
        updateSelectInput(session, "dataset_selector", choices = 1, selected = 1)
        shinyjs::disable("dataset_selector")
      }
      for (tbl in c("corr_table","coef_table","weight_table")) {
        shinyjs::runjs(
          sprintf('$("#%s .ht_master").css({"pointer-events":"auto","opacity":1});',
                  ns(tbl))
        )
      }
    })

    selected_dataset <- reactive({
      if (rv$dirty) return(NULL)
      if (is.list(rv$result) && !inherits(rv$result, "stats2data_mlr") &&
          input$n_datasets > 1) {
        rv$result[[as.integer(input$dataset_selector)]]
      } else rv$result
    })

    output$status_text <- renderText({
      if (rv$dirty) return(NULL)
      if (rv$status == "running") "Optimization is running..." else ""
    })

    output$best_error <- renderTable({
      if (rv$dirty) return(NULL)
      ds <- selected_dataset()
      req(ds)
      bes <- ds$best_error
      is_conv <- (bes == 0) | (bes <= input$tolerance)
      disp    <- ifelse(is_conv, "converged", format(bes))
      data.frame(
        Objective = disp,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, rownames = FALSE,
    colnames = FALSE)

    last_action <- reactiveVal(NULL)
    observeEvent(input$plot_error,         last_action("plot_error"))
    observeEvent(input$plot_error_ratio,   last_action("plot_error_ratio"))
    observeEvent(input$get_rmse,           last_action("get_rmse"))
    observeEvent(input$plot_summary,       last_action("plot_summary"))
    observeEvent(input$plot_cooling,       last_action("plot_cooling"))
    observeEvent(input$display_data,       last_action("display_data"))

    observeEvent(input$download, {
      showModal(modalDialog(
        title = "Download",
        downloadButton(ns("dl_object"), "Full nds3.object"),
        downloadButton(ns("dl_data"),   "Data as CSV"),
        easyClose = TRUE
      ))
    })

    output$plot_error <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_error(selected_dataset(),
                 first_iter = as.integer(input$iter_select)
      ) })
    output$plot_error_ratio <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_error(selected_dataset(), ratio = TRUE)
    })
    output$get_rmse <- renderPrint({
      if (rv$dirty) return(NULL)
      get_rmse(selected_dataset())
    })
    output$plot_summary <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_summary(selected_dataset(),
                   standardised = input$std) })
    output$plot_cooling <- renderPlot({
      if (rv$dirty) return(NULL)
      plot_cooling(selected_dataset())
    })
    output$display_data <- renderTable({
      if (rv$dirty) return(NULL)
      ds <- selected_dataset()
      if (!is.list(ds) || is.null(ds$data)) {
        return(data.frame(Message = "No data available."))
      }
      utils::head(as.data.frame(ds$data), min(nrow(ds$data),15))
    })

    output$main_output <- renderUI({
      if (rv$dirty) return(NULL)
      req(last_action())
      switch(last_action(),
             "plot_error"        = tagList(
               plotOutput(ns("plot_error"), width = "600px", height = "400px"),
               fluidRow(
                 column(
                   width = 6,
                   numericInput(
                     ns("iter_select"),
                     name_with_info(
                       "Start Iteration",
                       "Plot errors beginning at this iteration."
                     ),
                     value = isolate(input$iter_select %||% 1),
                     min   = 1,
                     max   = length(selected_dataset()$track_error),
                     step  = 100,
                     width = "100px"
                   )
                 )
               )
             ),
             "plot_error_ratio"  = plotOutput(ns("plot_error_ratio"),   width = "600px", height = "400px"),
             "get_rmse"          = verbatimTextOutput(ns("get_rmse")),
             "plot_summary"      = tagList(
               plotOutput(ns("plot_summary"), width = "600px", height = "400px"),
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
             "plot_cooling"      = plotOutput(ns("plot_cooling"),   width = "600px", height = "400px"),
             "display_data"      = tableOutput(ns("display_data"))
      )
    })

    output$dl_object <- downloadHandler(
      filename = "nds3_object.rds",
      content = function(file) {
        req(!rv$dirty)
        ds <- selected_dataset()
        saveRDS(ds, file)
      }
    )
    output$dl_data <- downloadHandler(
      filename = "optimized_data.csv",
      content  = function(file) {
        req(!rv$dirty)
        ds <- selected_dataset()
        utils::write.csv(as.data.frame(ds$data), file, row.names = TRUE)
      }
    )
  })
}
