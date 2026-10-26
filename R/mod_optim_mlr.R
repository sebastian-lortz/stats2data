#' optim_mlr UI Function
#'
#' @description A shiny Module for \code{optim_mlr()}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table hot_cell
#' @noRd
mod_optim_mlr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    s2d_css(),
    div(class = "s2d-module",
        div(id = ns("inputs"), class = "s2d-inputs",
            div(class = "s2d-col",
                wellPanel(
                  h4("Targets"),
                  numericInput(ns("N"), arg_label(
                    "Sample Size", "Total number of observations (rows).", "N"),
                    value = 150, min = 10, step = 1),
                  tags$label(arg_label(
                    "Variables",
                    paste("One row per variable: its name, reported mean and SD, lower (Min)",
                          "and upper (Max) bound, and whether it is integer-valued."),
                    "target_mean, target_sd, range, integer")),
                  rHandsontableOutput(ns("targets")),
                  fluidRow(
                    column(6, actionButton(ns("add_variable"),    "Add Variable",    class = "btn-sm btn-block")),
                    column(6, actionButton(ns("remove_variable"), "Remove Variable", class = "btn-sm btn-block"))
                  ),
                  br(),
                  s2d_sprite_prec_input(ns),
                  tags$hr(),
                  tags$label(arg_label(
                    "Target Correlations",
                    "Upper triangle of the reported correlation matrix of all variables; leave unreported cells empty.",
                    "target_cor")),
                  rHandsontableOutput(ns("target_cor")),
                  tags$hr(),
                  tags$label(arg_label(
                    "Regression Model", "Outcome, predictors and interactions of the reported regression model.",
                    "reg_equation")),
                  tags$div(style = "font-style: italic; color: #555; margin-bottom: 6px;",
                           textOutput(ns("reg_equation"))),
                  selectInput(ns("reg_outcome"), "Outcome", choices = NULL),
                  fluidRow(
                    column(6, checkboxGroupInput(ns("reg_predictors"),   "Predictors",   choices = NULL)),
                    column(6, checkboxGroupInput(ns("reg_interactions"), "Interactions", choices = NULL))
                  ),
                  tags$label(arg_label(
                    "Target Coefficients",
                    "Reported unstandardised coefficients (Estimate) and their standard errors (SE); leave unreported cells empty. If all SEs are empty, the SEs are not used.",
                    "target_reg, target_se")),
                  rHandsontableOutput(ns("target_reg"))
                )
            ),
            div(class = "s2d-col s2d-settings",
                wellPanel(
                  h4("Settings"),
                  numericInput(ns("weight"), arg_label(
                    "Correlation Weight",
                    "Mixing weight of the objective: weight x correlation error + (1 - weight) x regression error.",
                    "weight"),
                    value = 0.5, min = 0, max = 1, step = 0.05),
                  s2d_settings_inputs(ns, thresh = 5e-3, max_iter = 1e5, max_starts = 3,
                                      init_temp_default = "auto-calibrated"),
                  numericInput(ns("hill_climbs"), arg_label(
                    "Hill-Climbing Iterations",
                    "Number of hill-climbing iterations after the annealing; 0 or empty skips the refinement.",
                    "hill_climbs"),
                    value = 1e4, min = 0, step = 1000),
                  s2d_run_inputs(ns)
                )
            )
        ),
        div(class = "s2d-col s2d-results",
            h4("Results"),
            actionButton(ns("run"), name_with_info(
              "Run Optimization",
              "Simulate data that reproduce the target correlations and regression estimates (optim_mlr())."),
              class = "btn-primary"),
            br(), br(),
            mod_results_ui(ns("results"))
        )
    )
  )
}


#' optim_mlr Server Functions
#'
#' @noRd
mod_optim_mlr_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    targets <- reactiveVal(data.frame(
      variable    = c("X1", "X2", "Y"),
      target_mean = c(11.896, 18.178, 3.045),
      target_sd   = c(1.743, 1.471,  1.985),
      range_min   = c(7,    14,     -3),
      range_max   = c(17,    23,    9),
      integer     = c(FALSE, FALSE, FALSE),
      stringsAsFactors = FALSE
    ))

    redraw <- reactiveVal(0)
    output$targets <- renderRHandsontable({
      redraw()
      s2d_targets_table(isolate(targets()))
    })
    observeEvent(input$targets, targets(hot_to_r(input$targets)))

    # Reported Decimals: shown for integer variables, never below the table decimals
    sprite_prec <- s2d_sprite_prec_server(input, output, session, targets)
    observeEvent(input$add_variable, {
      tg <- targets()
      targets(rbind(tg, data.frame(
        variable = setdiff(paste0("X", seq_len(nrow(tg) + 1)), tg$variable)[1],
        target_mean = 4, target_sd = 1,
        range_min = 0, range_max = 9, integer = TRUE, stringsAsFactors = FALSE)))
      redraw(redraw() + 1)
    })
    observeEvent(input$remove_variable, {
      tg <- targets()
      if (nrow(tg) > 2) targets(tg[-nrow(tg), , drop = FALSE])
      redraw(redraw() + 1)
    })

    vars <- reactiveVal(NULL)
    observe(vars(targets()$variable))

    cor_store <- reactiveVal({
      v <- c("X1", "X2", "Y")
      m <- matrix(NA_real_, 3, 3, dimnames = list(v, v))
      m[upper.tri(m)] <- c(0.024, 0.231, 0.256)  # X1-X2, X1-Y, X2-Y
      m
    })

    cor_for <- function(v, store) {
      m    <- matrix(NA_real_, length(v), length(v), dimnames = list(v, v))
      keep <- intersect(v, rownames(store))
      m[keep, keep] <- store[keep, keep]
      m[lower.tri(m, diag = TRUE)] <- NA_real_
      m
    }
    output$target_cor <- renderRHandsontable({
      v  <- vars()
      req(length(v) >= 2)
      m  <- cor_for(v, isolate(cor_store()))
      df <- as.data.frame(m, check.names = FALSE)
      tb <- hot_col(hot_table(rhandsontable(df, rowHeaders = rownames(m)), overflow = "visible"),
                    col = colnames(df), format = "0.000")
      for (i in seq_len(nrow(m))) for (j in seq_len(i)) tb <- hot_cell(tb, row = i, col = j, readOnly = TRUE)
      tb
    })
    observeEvent(input$target_cor, {
      m <- as.matrix(hot_to_r(input$target_cor))
      if (!is.null(rownames(m)) && identical(rownames(m), colnames(m))) {
        m[lower.tri(m, diag = TRUE)] <- NA_real_
        cor_store(m)
      }
    })

    observeEvent(vars(), {
      v   <- vars()
      cur <- isolate(input$reg_outcome)
      updateSelectInput(session, "reg_outcome", choices = v,
                        selected = if (!is.null(cur) && cur %in% v) cur else v[length(v)])
    })
    observeEvent(list(input$reg_outcome, vars()), {
      preds <- setdiff(vars(), input$reg_outcome)
      cur   <- isolate(input$reg_predictors)
      sel   <- if (length(cur)) intersect(cur, preds) else preds
      if (!length(sel)) sel <- preds
      updateCheckboxGroupInput(session, "reg_predictors", choices = preds, selected = sel)
    })
    default_inter <- "X1:X2"
    observeEvent(input$reg_predictors, {
      p     <- input$reg_predictors
      inter <- if (length(p) >= 2) utils::combn(p, 2, FUN = paste, collapse = ":") else character(0)
      sel   <- isolate(input$reg_interactions)
      if (length(default_inter) && all(default_inter %in% inter)) {
        sel <- default_inter
        default_inter <<- character(0)
      }
      updateCheckboxGroupInput(session, "reg_interactions", choices = inter,
                               selected = intersect(sel, inter))
    }, ignoreNULL = FALSE)

    reg_terms <- reactive({
      req(input$reg_outcome, length(input$reg_predictors) > 0)
      c(input$reg_predictors, input$reg_interactions)
    })
    reg_equation <- reactive(paste(input$reg_outcome, "~", paste(reg_terms(), collapse = " + ")))
    output$reg_equation <- renderText({
      if (length(input$reg_predictors) == 0) "Select at least one predictor." else reg_equation()
    })

    coef_store <- reactiveVal(list(
      "(Intercept)" = c(-12.402, 12.262),
      "X1"          = c(0.784, 1.024),
      "X2"          = c(0.682, 0.673),
      "X1:X2"       = c(-0.029, 0.056)
    ))
    coef_for <- function(terms, store) {
      m <- matrix(NA_real_, 2, length(terms), dimnames = list(c("target_reg", "target_se"), terms))
      for (t in intersect(terms, names(store))) m[, t] <- store[[t]]
      m
    }
    output$target_reg <- renderRHandsontable({
      m  <- coef_for(c("(Intercept)", reg_terms()), isolate(coef_store()))
      df <- as.data.frame(m, check.names = FALSE)
      hot_col(hot_table(rhandsontable(df, rowHeaders = c("Estimate", "SE"), rowHeaderWidth = 85),
                        overflow = "visible"),
              col = colnames(df), format = "0.000")
    })
    observeEvent(input$target_reg, {
      df    <- hot_to_r(input$target_reg)
      store <- coef_store()
      for (t in colnames(df)) {
        v <- as.numeric(df[, t])
        if (any(!is.na(v))) store[[t]] <- v else store[[t]] <- NULL
      }
      coef_store(store)
    })

    result    <- reactiveVal(NULL)
    call_code <- reactiveVal(NULL)
    observeEvent(list(input$N, targets(), input$sprite_prec_mean, input$sprite_prec_sd,
                      cor_store(), input$reg_outcome, input$reg_predictors,
                      input$reg_interactions, coef_store(), input$weight, input$thresh,
                      input$max_iter, input$init_temp, input$cooling_rate, input$max_starts,
                      input$hill_climbs, input$runs, input$seed),
                 result(NULL), ignoreInit = TRUE)

    observeEvent(input$max_iter, s2d_cooling_placeholder(session, input$max_iter))

    observeEvent(input$run, {
      if (!s2d_filled(list("Sample Size" = input$N, "Correlation Weight" = input$weight,
                           "Convergence Threshold" = input$thresh,
                           "Max Iterations" = input$max_iter, "Max Restarts" = input$max_starts))) return()
      tg <- targets()
      if (any(!nzchar(tg$variable)) || anyDuplicated(tg$variable) ||
          anyNA(tg[, c("target_mean", "target_sd", "range_min", "range_max")])) {
        s2d_notify("Each variable needs a unique name, Mean, SD, Min and Max.")
        return()
      }
      if (!s2d_sprite_prec_ok(sprite_prec(), tg)) return()
      if (length(input$reg_predictors) == 0) {
        s2d_notify("Select at least one predictor for the Regression Model.")
        return()
      }
      cf <- coef_for(c("(Intercept)", reg_terms()), coef_store())
      target_reg <- cf["target_reg", ]
      target_se  <- cf["target_se", ]
      if (all(is.na(target_reg))) {
        s2d_notify("Enter at least one target coefficient (Estimate).")
        return()
      }
      w <- input$weight
      if (!is.numeric(w) || is.na(w) || w < 0 || w > 1) {
        s2d_notify("Correlation Weight must be a number between 0 and 1.")
        return()
      }
      m <- cor_for(tg$variable, cor_store())
      args <- list(
        N            = input$N,
        target_mean  = stats::setNames(tg$target_mean, tg$variable),
        target_sd    = stats::setNames(tg$target_sd,   tg$variable),
        range        = rbind(tg$range_min, tg$range_max),
        integer      = as.logical(tg$integer),
        target_cor   = m[upper.tri(m)],
        target_reg   = target_reg,
        reg_equation = reg_equation(),
        sprite_prec  = sprite_prec(),
        target_se    = na_null(target_se),
        weight       = w,
        thresh       = input$thresh,
        max_iter     = input$max_iter,
        init_temp    = na_null(input$init_temp),
        cooling_rate = na_null(input$cooling_rate),
        max_starts   = input$max_starts,
        hill_climbs  = na_null(input$hill_climbs)
      )
      ok <- check_mlr_inputs(
        thresh = args$thresh, max_iter = args$max_iter, init_temp = args$init_temp,
        cooling_rate = args$cooling_rate, hill_climbs = args$hill_climbs,
        max_starts = args$max_starts
      )
      if (!ok) return()

      s2d_execute("optim_mlr", args, runs = input$runs, seed = input$seed,
                  result = result, call_code = call_code)
    })

    mod_results_server("results", result = result, call_code = call_code,
                       file_stub = "optim_mlr")
  })
}
