#' optim_vec UI Function
#'
#' @description A shiny Module for \code{optim_vec()}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table
#' @noRd
mod_optim_vec_ui <- function(id) {
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
                    "Sample Size", "Number of observations per variable.", "N"),
                    value = 864, min = 10, step = 1),
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
                  s2d_sprite_prec_input(ns)
                )
            ),
            div(class = "s2d-col s2d-settings",
                wellPanel(
                  h4("Settings"),
                  s2d_settings_inputs(ns, thresh = 1e-2, max_iter = 1e5, max_starts = 3,
                                       init_temp_default = "0.001"),
                  s2d_run_inputs(ns)
                )
            )
        ),
        div(class = "s2d-col s2d-results",
            h4("Results"),
            actionButton(ns("run"), name_with_info(
              "Run Optimization",
               "Simulate one vector per variable that matches its target mean and SD (optim_vec())."),
              class = "btn-primary"),
            br(), br(),
            mod_results_ui(ns("results"))
        )
    )
  )
}


#' optim_vec Server Functions
#'
#' @noRd
mod_optim_vec_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    targets <- reactiveVal(data.frame(
      variable    = paste0("V", 1:5),
      target_mean = c(10.53, 0.27, 2.49, -0.64, 0.03),
       target_sd   = c( 7.82, 0.45, 11.68, 1.85, 2.05),
      range_min   = c( 0,    0,   -24,   -3,   -3),
      range_max   = c(36,    1,    24,    3,    3),
      integer     = rep(TRUE, 5),
      stringsAsFactors = FALSE
    ))
    redraw <- reactiveVal(0)
     output$targets <- renderRHandsontable({
      redraw()
      s2d_targets_table(isolate(targets()))
    })
    observeEvent(input$targets, targets(hot_to_r(input$targets)))

    observeEvent(input$add_variable, {
      tg <- targets()
      targets(rbind(tg, data.frame(
        variable = paste0("V", nrow(tg) + 1), target_mean = 4, target_sd = 1,
         range_min = 0, range_max = 9, integer = TRUE, stringsAsFactors = FALSE)))
      redraw(redraw() + 1)
    })
    observeEvent(input$remove_variable, {
      tg <- targets()
      if (nrow(tg) > 1) targets(tg[-nrow(tg), , drop = FALSE])
      redraw(redraw() + 1)
    })

    result    <- reactiveVal(NULL)
     call_code <- reactiveVal(NULL)
    observeEvent(list(input$N, targets(), input$sprite_prec_mean, input$sprite_prec_sd,
                      input$thresh, input$max_iter, input$init_temp, input$cooling_rate,
                      input$max_starts, input$runs, input$seed),
                 result(NULL), ignoreInit = TRUE)

    observeEvent(input$max_iter, s2d_cooling_placeholder(session, input$max_iter))

     observeEvent(input$run, {
      if (!s2d_filled(list("Sample Size" = input$N, "Convergence Threshold" = input$thresh,
                           "Max Iterations" = input$max_iter, "Max Restarts" = input$max_starts,
                           "Reported Decimals (Mean)" = input$sprite_prec_mean,
                           "Reported Decimals (SD)" = input$sprite_prec_sd))) return()
      tg <- targets()
      if (nrow(tg) < 1 || any(!nzchar(tg$variable)) || anyDuplicated(tg$variable) ||
          anyNA(tg[, c("target_mean", "target_sd", "range_min", "range_max")])) {
        s2d_notify("Each variable needs a unique name, Mean, SD, Min and Max.")
        return()
      }
      args <- list(
        N            = input$N,
        target_mean  = stats::setNames(tg$target_mean, tg$variable),
        target_sd    = stats::setNames(tg$target_sd,   tg$variable),
        range        = rbind(tg$range_min, tg$range_max),
        integer      = as.logical(tg$integer),
         thresh       = input$thresh,
        sprite_prec  = c(input$sprite_prec_mean, input$sprite_prec_sd),
        max_iter     = input$max_iter,
        init_temp    = na_null(input$init_temp) %||% 1e-3,  # empty: function default
        cooling_rate = na_null(input$cooling_rate),
        max_starts   = input$max_starts
      )
      ok <- check_vec_inputs(
        N = args$N, target_mean = args$target_mean, range = args$range,
        thresh = args$thresh, max_iter = args$max_iter, init_temp = args$init_temp,
        cooling_rate = args$cooling_rate, max_starts = args$max_starts
      )
      if (!ok) return()

      res <- s2d_execute("optim_vec", args, runs = input$runs, seed = input$seed,
                         result = result, call_code = call_code)

       runs <- if (inherits(res, "stats2data_parallel")) res$results else if (!is.null(res)) list(res)
      msgs <- unique(unlist(lapply(runs, function(r) {
        m  <- r$error_msgs
        ok <- !vapply(m, is.null, logical(1))
        if (any(ok)) paste0("Variable '", tg$variable[ok], "': ", unlist(m[ok]))
      })))
      for (m in msgs) {
        showNotification(paste(m, "Consider adjusting the SD, range, or switching to continuous."),
                          type = "warning", duration = 15)
      }
    })

    mod_results_server("results", result = result, call_code = call_code,
                       file_stub = "optim_vec")
  })
}
