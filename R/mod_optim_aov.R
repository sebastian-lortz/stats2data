#' optim_aov UI Function
#'
#' @description A shiny Module for \code{optim_aov()}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs toggle
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_table
#' @importFrom htmltools HTML
#' @noRd
mod_optim_aov_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    s2d_css(),
    div(class = "s2d-module",
        div(id = ns("inputs"), class = "s2d-inputs",
            div(class = "s2d-col",
                wellPanel(
                  h4("Targets"),
                  numericInput(ns("S"), arg_label(
                    "Number of Subjects", "Total number of subjects.", "S"),
                    value = 443, min = 10, step = 1),
                  tags$label(arg_label(
                    "Factors",
                    "One row per factor: its name, number of levels, and whether it varies between or within subjects.",
                     "levels, factor_type")),
                  rHandsontableOutput(ns("factors")),
                  fluidRow(
                    column(6, actionButton(ns("add_factor"),    "Add Factor",    class = "btn-sm btn-block")),
                    column(6, actionButton(ns("remove_factor"), "Remove Factor", class = "btn-sm btn-block"))
                  ),
                  br(),
                  tags$label(arg_label(
                    "Target Cell Means",
                    "Reported mean of every cell (combination of factor levels).",
                    "target_group_means")),
                  rHandsontableOutput(ns("target_group_means")),
                  br(),
                  div(id = ns("subgroup_sizes_box"),
                       tags$label(arg_label(
                        "Group Sizes",
                        "Number of subjects in each between-subjects group; must sum to the Number of Subjects. Leave all cells empty for an equal split.",
                        "subgroup_sizes")),
                      rHandsontableOutput(ns("subgroup_sizes")),
                      br()
                  ),
                  tags$label(arg_label(
                    "Target F Values", "Reported F value of each effect.", "target_f_list")),
                  rHandsontableOutput(ns("target_f_list")),
                  fluidRow(
                    column(6, actionButton(ns("add_effect"),    "Add Effect",    class = "btn-sm btn-block")),
                    column(6, actionButton(ns("remove_effect"), "Remove Effect", class = "btn-sm btn-block"))
                  ),
                  br(),
                  tags$label(arg_label(
                    "ANOVA Model", "Effects of the ANOVA model; an Error() term is added for within-subjects factors.",
                     "formula")),
                  fluidRow(
                    column(6, checkboxGroupInput(ns("main_effects"), "Main Effects", choices = NULL)),
                    column(6, checkboxGroupInput(ns("interactions"), "Interactions", choices = NULL))
                  ),
                  tags$div(style = "font-style: italic; color: #555; margin-bottom: 10px;",
                           textOutput(ns("formula"))),
                  shinyWidgets::numericRangeInput(
                    ns("range"),
                    label = arg_label("Outcome Range", "Lower and upper bound of the outcome.", "range"),
                    value = c(0, 60), separator = " to ", width = "220px"),
                   checkboxInput(ns("integer"), arg_label(
                    "Integer Outcome", "Generate integer-valued outcomes.", "integer"), value = TRUE)
                )
            ),
            div(class = "s2d-col s2d-settings",
                wellPanel(
                  h4("Settings"),
                  s2d_settings_inputs(ns, thresh = 5e-3, max_iter = 5e3, max_starts = 3,
                                      init_temp_default = "0.001"),
                  s2d_run_inputs(ns)
                )
            )
        ),
        div(class = "s2d-col s2d-results",
            h4("Results"),
             actionButton(ns("run"), name_with_info(
              "Run Optimization",
              "Simulate data that reproduce the target cell means and F values (optim_aov())."),
              class = "btn-primary"),
            br(), br(),
            mod_results_ui(ns("results"))
        )
    )
  )
}


#' optim_aov Server Functions
#'
#' @noRd
mod_optim_aov_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    factors <- reactiveVal(data.frame(
      name = c("Factor1", "Factor2"), levels = c(2, 2),
      factor_type = c("between", "within"), stringsAsFactors = FALSE
    ))
     cells <- reactiveVal(data.frame(
      Factor1 = rep(1:2, each = 2), Factor2 = rep(1:2, 2),
      target_group_means = c(31.4, 21.4, 30.1, 17.8)
    ))
    sizes <- reactiveVal(data.frame(Factor1 = 1:2, subgroup_sizes = c(230, 213)))
    f_list <- reactiveVal(data.frame(
      effect = c("Factor1", "Factor2", "Factor1:Factor2"), F_value = c(8.2, 321.2, 3.4),
      stringsAsFactors = FALSE
    ))
    design <- reactiveVal(list(names = c("Factor1", "Factor2"), levels = c(2L, 2L),
                               between = "Factor1"))

     factors_ok <- function(ff) {
      nrow(ff) >= 1 && all(nzchar(ff$name)) && !anyDuplicated(ff$name) &&
        !any(grepl(":", ff$name, fixed = TRUE)) &&
        !anyNA(ff$levels) && all(ff$levels >= 2) && all(ff$levels == round(ff$levels)) &&
        all(ff$factor_type %in% c("between", "within"))
    }
    canonical <- reactive({
      ff <- factors()
      stats::setNames(paste0("Factor", seq_len(nrow(ff))), ff$name)
    })
    remap <- function(effects, nm) {
      vapply(effects, function(e) paste(nm[strsplit(e, ":", fixed = TRUE)[[1]]], collapse = ":"),
             character(1), USE.NAMES = FALSE)
    }

    redraw_factors <- reactiveVal(0)
     redraw_design  <- reactiveVal(0)
    redraw_effects <- reactiveVal(0)

    output$factors <- renderRHandsontable({
      redraw_factors()
       hot_col(hot_col(hot_col(rhandsontable(isolate(factors()), rowHeaders = NULL,
                                            colHeaders = c("Factor", "Levels", "Type")),
                              "Factor", type = "text"),
                      "Levels", type = "numeric", format = "0"),
              "Type", type = "dropdown", source = c("between", "within"))
    })
    observeEvent(input$factors, factors(hot_to_r(input$factors)))
    observeEvent(input$add_factor, {
      ff <- factors()
      factors(rbind(ff, data.frame(name = paste0("Factor", nrow(ff) + 1), levels = 2,
                                   factor_type = "between", stringsAsFactors = FALSE)))
      redraw_factors(redraw_factors() + 1)
    })
    observeEvent(input$remove_factor, {
      ff <- factors()
       if (nrow(ff) > 1) factors(ff[-nrow(ff), , drop = FALSE])
      redraw_factors(redraw_factors() + 1)
    })

    observeEvent(factors(), {
      ff <- factors()
      if (!factors_ok(ff)) return()
      old <- design()
       lv  <- as.integer(ff$levels)
      bt  <- ff$factor_type == "between"

      cg <- s2d_level_grid(ff$name, lv)
      cg$target_group_means <- if (identical(old$levels, lv)) cells()$target_group_means else NA_real_
      cells(cg)

      if (any(bt)) {
        sg <- s2d_level_grid(ff$name[bt], lv[bt])
        keep <- !is.null(sizes()) && identical(old$levels[match(old$between, old$names)], lv[bt]) &&
          identical(match(old$between, old$names), which(bt))
        sg$subgroup_sizes <- if (keep) sizes()$subgroup_sizes else NA_real_
        sizes(sg)
      } else {
         sizes(NULL)
      }
      design(list(names = ff$name, levels = lv, between = ff$name[bt]))

      if (identical(old$levels, lv) && length(old$names) == length(ff$name)) {
        fl <- f_list()
         fl$effect <- vapply(fl$effect, function(e) {
          parts <- strsplit(e, ":", fixed = TRUE)[[1]]
          paste(ff$name[match(parts, old$names)], collapse = ":")
        }, character(1), USE.NAMES = FALSE)
        f_list(fl)
      }

      redraw_design(redraw_design() + 1)
      redraw_effects(redraw_effects() + 1)

      updateCheckboxGroupInput(session, "main_effects", choices = ff$name, selected = ff$name)
       inter <- s2d_effects(ff$name, min_order = 2L)
      updateCheckboxGroupInput(session, "interactions", choices = inter, selected = inter)
    })

    output$target_group_means <- renderRHandsontable({
      redraw_design()
      cg <- isolate(cells())
       req(cg)
      fac <- setdiff(names(cg), "target_group_means")
      tb  <- rhandsontable(cg, rowHeaders = NULL, colHeaders = c(fac, "Mean"))
      for (col in fac) tb <- hot_col(tb, col, readOnly = TRUE)
      hot_col(tb, "Mean", type = "numeric", format = "0.00")
    })
    observeEvent(input$target_group_means, cells(hot_to_r(input$target_group_means)))

    observe(shinyjs::toggle("subgroup_sizes_box", condition = !is.null(sizes())))
    output$subgroup_sizes <- renderRHandsontable({
      redraw_design()
      sg <- isolate(sizes())
       req(sg)
      fac <- setdiff(names(sg), "subgroup_sizes")
      tb  <- rhandsontable(sg, rowHeaders = NULL, colHeaders = c(fac, "Group Size"))
      for (col in fac) tb <- hot_col(tb, col, readOnly = TRUE)
      hot_col(tb, "Group Size", type = "numeric", format = "0")
    })
    observeEvent(input$subgroup_sizes, sizes(hot_to_r(input$subgroup_sizes)))

    output$target_f_list <- renderRHandsontable({
      redraw_effects()
      choices <- s2d_effects(isolate(factors())$name)
      hot_col(hot_col(hot_table(rhandsontable(isolate(f_list()), rowHeaders = NULL,
                                              colHeaders = c("Effect", "F Value"),
                                              colWidths = c(170, 80)), overflow = "visible"),
                      "Effect", type = "dropdown", source = choices, strict = TRUE,
                      allowInvalid = FALSE),
              "F Value", type = "numeric", format = "0.00")
    })
    observeEvent(input$target_f_list, f_list(hot_to_r(input$target_f_list)))
     observeEvent(input$add_effect, {
      fl <- f_list()
      f_list(rbind(fl, data.frame(effect = "", F_value = NA_real_, stringsAsFactors = FALSE)))
      redraw_effects(redraw_effects() + 1)
    })
    observeEvent(input$remove_effect, {
      fl <- f_list()
      if (nrow(fl) > 1) f_list(fl[-nrow(fl), , drop = FALSE])
      redraw_effects(redraw_effects() + 1)
    })

    formula_txt <- reactive({
      ff <- factors()
      req(factors_ok(ff))
      nm  <- canonical()
       sel <- c(input$main_effects, input$interactions)
      sel <- sel[sel %in% s2d_effects(ff$name)]
      rhs <- if (length(sel)) paste(remap(sel, nm), collapse = " + ") else "1"
      w   <- unname(nm[ff$factor_type == "within"])
      if (length(w) == 1L) {
        rhs <- paste0(rhs, " + Error(ID/", w, ")")
      } else if (length(w) > 1L) {
        rhs <- paste0(rhs, " + Error(ID/(", paste(w, collapse = " * "), "))")
      }
      paste("outcome ~", rhs)
    })
    output$formula <- renderText({
      nm <- canonical()
       map <- if (!identical(unname(nm), names(nm))) {
        paste0("   [", paste(unname(nm), "=", names(nm), collapse = ", "), "]")
      }
      paste0(formula_txt(), map)
    })

    result    <- reactiveVal(NULL)
     call_code <- reactiveVal(NULL)
    observeEvent(list(input$S, factors(), cells(), sizes(), f_list(), input$main_effects,
                      input$interactions, input$range, input$integer, input$thresh,
                      input$max_iter, input$init_temp, input$cooling_rate, input$max_starts,
                      input$runs, input$seed),
                 result(NULL), ignoreInit = TRUE)

    observeEvent(input$max_iter, s2d_cooling_placeholder(session, input$max_iter))

    observeEvent(input$run, {
      if (!s2d_filled(list("Number of Subjects" = input$S, "Convergence Threshold" = input$thresh,
                           "Max Iterations" = input$max_iter,
                           "Max Restarts" = input$max_starts))) return()
      ff <- factors()
       cg <- cells()
      sg <- sizes()
      fl <- f_list()
      if (!factors_ok(ff) || nrow(cg) != prod(ff$levels) || !all(ff$name %in% names(cg))) {
        s2d_notify("Each factor needs a unique name and a whole number of levels (>= 2).")
        return()
      }
      if (anyNA(cg$target_group_means)) {
        s2d_notify("Enter a mean for every cell (Target Cell Means).")
        return()
      }
      if (nrow(fl) < 1 || anyNA(fl$F_value) || !all(fl$effect %in% s2d_effects(ff$name))) {
        s2d_notify("Each row of Target F Values needs an effect of the design and an F value.")
        return()
      }
      subgroup_sizes <- if (is.null(sg)) NULL else na_null(sg$subgroup_sizes)
      if (!is.null(subgroup_sizes) && anyNA(subgroup_sizes)) {
        s2d_notify("Fill in all Group Sizes, or leave all of them empty.")
        return()
      }
      nm <- canonical()
      args <- list(
        S                  = input$S,
        levels             = as.integer(ff$levels),
         target_group_means = cg$target_group_means,
        target_f_list      = list(effect = remap(fl$effect, nm), F_value = fl$F_value),
        integer            = input$integer,
        range              = input$range,
        formula            = formula_txt(),
        factor_type        = ff$factor_type,
        subgroup_sizes     = subgroup_sizes,
        thresh             = input$thresh,
        max_iter           = input$max_iter,
        init_temp          = na_null(input$init_temp) %||% 1e-3,  # empty: function default
        cooling_rate       = na_null(input$cooling_rate),
        max_starts         = input$max_starts
      )
      ok <- check_aov_inputs(
        N = args$S, levels = args$levels, target_group_means = args$target_group_means,
        range = args$range, thresh = args$thresh, max_iter = args$max_iter,
        init_temp = args$init_temp, cooling_rate = args$cooling_rate,
         max_starts = args$max_starts
      )
      if (!ok) return()

      notes <- if (!identical(unname(nm), names(nm))) {
        paste0("# Factors: ", paste(unname(nm), "=", names(nm), collapse = ", "))
      }
      s2d_execute("optim_aov", args, runs = input$runs, seed = input$seed,
                  result = result, call_code = call_code, notes = notes)
    })

    mod_results_server("results", result = result, call_code = call_code,
                       file_stub = "optim_aov")
  })
}
