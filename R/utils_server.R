#' server
#'
#' @description Utility functions shared by the shiny modules.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# info logo in name html wrapper; icon() attaches the Font Awesome dependency
name_with_info <- function(name, description) {
  ic <- shiny::icon("circle-info")
  htmltools::attachDependencies(
    HTML(paste0(
      '<span title="', htmltools::htmlEscape(description, attribute = TRUE), '">',
      name, ' ', as.character(ic),
      '</span>')),
    htmltools::findDependencies(ic)
  )
}

arg_label <- function(label, description, arg = NULL) {
  if (!is.null(arg)) description <- paste0(description, " (argument: ", arg, ")")
  tags$span(class = "s2d-label", name_with_info(label, description))
}

# define . function
if (getRversion() >= "2.15.1") utils::globalVariables(".")

na_null <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) NULL else x
}

s2d_notify <- function(msg) showNotification(msg, type = "error", duration = 10)

s2d_filled <- function(x) {
  empty <- names(x)[vapply(x, function(v) is.null(v) || length(v) != 1L || is.na(v), logical(1))]
  if (length(empty)) {
    s2d_notify(paste("Please fill in:", paste(empty, collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

s2d_effects <- function(factors, min_order = 1L) {
  k <- length(factors)
  if (k < min_order) return(character(0))
  unlist(lapply(seq.int(min_order, k), function(m) {
    utils::combn(factors, m, FUN = paste, collapse = ":")
  }), use.names = FALSE)
}

s2d_level_grid <- function(names, levels) {
  g <- expand.grid(rev(lapply(levels, seq_len)), KEEP.OUT.ATTRS = FALSE)
  g <- g[, rev(seq_along(g)), drop = FALSE]
  names(g) <- names
  g
}


s2d_css <- function() {
  singleton(tags$head(tags$style(HTML("
    .s2d-module { white-space: nowrap; }
    .s2d-inputs { display: inline; }
    .s2d-col { display: inline-block; vertical-align: top; margin-right: 15px;
               white-space: normal; }
    .s2d-col.s2d-settings { width: 240px; }
    .s2d-busy { pointer-events: none; opacity: 0.55; }
    .s2d-results .btn { margin: 0 4px 6px 0; }
    .s2d-call { max-height: 320px; overflow: auto; font-size: 85%; }
    .btn code { color: inherit; background: transparent; padding: 0; }
    .s2d-module .control-label, .s2d-module .s2d-label { color: black; }
    .s2d-module .btn-primary { background-color: steelblue; border-color: #3a6a92; color: #fff; }
    .s2d-module .btn-primary:hover, .s2d-module .btn-primary:focus,
    .s2d-module .btn-primary:active { background-color: #3a6a92; border-color: #2e5576; color: #fff; }
    .s2d-module .btn-default:not(.btn-primary) { color: steelblue; border-color: steelblue; }
    .s2d-module .btn-default:not(.btn-primary):hover,
    .s2d-module .btn-default:not(.btn-primary):focus { background-color: #eef3f8; color: #3a6a92; }
    .s2d-module input[type=checkbox], .s2d-module input[type=radio] { accent-color: black; }
    .s2d-module .form-control:focus { border-color: black;
                                      box-shadow: 0 0 0 2px rgba(70, 130, 180, 0.25); }
    .modal .btn-default.shiny-download-link { color: steelblue; border-color: steelblue; }
    .rhandsontable .ht_master .wtHolder,
    .rhandsontable .ht_master .handsontable { overflow: visible !important; }
    .rhandsontable .ht_master .htDropdownMenu,
    .rhandsontable .ht_master .htContextMenu { z-index: 9999 !important; }
    .selectize-dropdown, .selectize-dropdown-content { z-index: 9999 !important; }
  "))))
}

# empty numericInput that shows the function default as placeholder
s2d_default_input <- function(id, label, placeholder, ...) {
  htmltools::tagAppendAttributes(
    numericInput(id, label, value = NA, ...),
    placeholder = placeholder, .cssSelector = "input"
  )
}

# default cooling rate of optim_vec(), optim_aov() and optim_mlr()
s2d_cooling_default <- function(max_iter) {
  if (!is.numeric(max_iter) || length(max_iter) != 1L || is.na(max_iter) || max_iter <= 10) {
    return("(Max Iterations - 10) / Max Iterations")
  }
  format((max_iter - 10) / max_iter, digits = 6)
}

s2d_cooling_placeholder <- function(session, max_iter) {
  shinyjs::runjs(sprintf("$('#%s').attr('placeholder', 'Default: %s');",
                         session$ns("cooling_rate"), s2d_cooling_default(max_iter)))
}

s2d_settings_inputs <- function(ns, thresh, max_iter, max_starts, init_temp_default) {
  tagList(
    numericInput(ns("thresh"), arg_label(
      "Convergence Threshold",
      "A run stops once the objective value falls below it.", "thresh"),
      value = thresh, min = 0, step = thresh / 10),
    numericInput(ns("max_iter"), arg_label(
      "Max Iterations", "Number of iterations per (re)start.", "max_iter"),
      value = max_iter, min = 1, step = 1000),
    s2d_default_input(ns("init_temp"), arg_label(
      "Initial Temperature",
      "Initial simulated-annealing temperature. Leave empty to use the default.", "init_temp"),
      placeholder = paste("Default:", init_temp_default), min = 0, step = 1e-3),
    s2d_default_input(ns("cooling_rate"), arg_label(
      "Cooling Rate", paste(
        "Factor by which the temperature is multiplied after each iteration, in (0, 1).",
        "Leave empty to use the default, (Max Iterations - 10) / Max Iterations."), "cooling_rate"),
      placeholder = paste("Default:", s2d_cooling_default(max_iter)), min = 0, max = 1, step = 1e-4),
    numericInput(ns("max_starts"), arg_label(
      "Max Restarts", "Maximum number of restarts from the best candidate found so far.",
      "max_starts"),
      value = max_starts, min = 1, step = 1)
  )
}

# number of runs and seed; runs > 1 calls parallel_optim()
s2d_run_inputs <- function(ns) {
  tagList(
    tags$hr(),
    numericInput(ns("runs"), arg_label(
      "Number of Runs",
      "Number of independent runs. With more than one run the app calls parallel_optim().",
      "runs"),
      value = 1, min = 1, step = 1),
    numericInput(ns("seed"), arg_label(
      "Random Seed",
      "Leave empty to draw one; the seed used is written to the exported function call.",
      "seed"),
      value = NA, min = 1, step = 1)
  )
}

s2d_sprite_prec_input <- function(ns) {
  tags$div(
    tags$label(arg_label(
      "Reported Decimals",
      "Decimals to which the target mean and SD are reported; used by SPRITE for integer variables.",
      "sprite_prec")),
    fluidRow(
      column(6, numericInput(ns("sprite_prec_mean"), "Mean", value = 2, min = 0, step = 1)),
      column(6, numericInput(ns("sprite_prec_sd"),   "SD",   value = 2, min = 0, step = 1))
    )
  )
}

s2d_targets_table <- function(df) {
  rhandsontable::hot_col(
    rhandsontable::rhandsontable(
      df, rowHeaders = NULL,
      colHeaders = c("Variable", "Mean", "SD", "Min", "Max", "Integer")),
    "Integer", type = "checkbox"
  )
}



# R code for one argument value
s2d_deparse <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.matrix(x) && is.null(dimnames(x))) {
    return(sprintf("matrix(%s, nrow = %d)",
                   paste(deparse(as.vector(x), width.cutoff = 500L), collapse = ""),
                   nrow(x)))
  }
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

s2d_call_code <- function(fun_name, args, runs = 1L, seed, notes = NULL) {
  arg_lines <- function(a, indent) {
    l <- vapply(names(a), function(nm) paste0(indent, nm, " = ", s2d_deparse(a[[nm]])),
                character(1))
    paste(l, collapse = ",\n")
  }
  header <- c(
    sprintf("# %s() call exported from the stats2data shiny app (%s)",
            fun_name, format(Sys.time(), "%Y-%m-%d %H:%M")),
    "# Run it in R to reproduce the result shown in the app.",
    notes,
    "library(stats2data)",
    ""
  )
  body <- if (runs > 1L) {
    c("res <- parallel_optim(",
      sprintf("  FUN  = %s,", fun_name),
      "  args = list(",
      arg_lines(args, "    "),
      "  ),",
      sprintf("  runs = %dL,", as.integer(runs)),
      sprintf("  seed = %dL", as.integer(seed)),
      ")")
  } else {
    c(sprintf("set.seed(%dL)", as.integer(seed)),
      sprintf("res <- %s(", fun_name),
      arg_lines(args, "  "),
      ")")
  }
  footer <- c("", "summary(res)", "plot_fit(res)",
              if (runs > 1L) "plot_stability(res)" else "plot_convergence(res)")
  paste(c(header, body, footer), collapse = "\n")
}


s2d_execute <- function(fun_name, args, runs, seed, result, call_code,
                        notes = NULL) {
  runs <- if (is.null(runs) || is.na(runs) || runs < 1) 1L else as.integer(runs)
  seed <- if (is.null(seed) || is.na(seed)) sample.int(1e8, 1L) else as.integer(seed)
  fun  <- switch(fun_name, optim_vec = optim_vec, optim_aov = optim_aov,
                 optim_mlr = optim_mlr)

  result(NULL)
  shinyjs::addClass("inputs", "s2d-busy")
  shinyjs::disable("run")
  on.exit({
    shinyjs::removeClass("inputs", "s2d-busy")
    shinyjs::enable("run")
  }, add = TRUE)

  res <- tryCatch(
    withProgress(message = "Running optimization ...", value = 0, {
      if (runs > 1L) {
        parallel_optim(FUN = fun, args = args, runs = runs, cores = 1L,
                       seed = seed, progress_mode = "shiny")
      } else {
        set.seed(seed)
        do.call(fun, c(args, list(progress_mode = "shiny")))
      }
    }),
    error = function(e) {
      s2d_notify(paste("Optimization failed:", conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) {
    call_code(s2d_call_code(fun_name, args, runs = runs, seed = seed, notes = notes))
    result(res)
  }
  invisible(res)
}


# input checks vec
#' Validate inputs for optim_vec (in Shiny)
#'
#' @noRd
check_vec_inputs <- function(
    N,
    target_mean,
    range,
    thresh,
    max_iter,
    init_temp,
    cooling_rate,
    max_starts
) {
  if (!is.numeric(N) || length(N) != 1 || N < 10  || N != as.integer(N)) {
    showNotification(
      sprintf("Sample Size (%s) must be a single integer >= 10.", N),
      type = "error"
    )
    return(FALSE)
  }
  bad_idx <- which(target_mean < range[1, ] | target_mean > range[2, ])
  if (length(bad_idx) > 0) {
    bad_vals <- target_mean[bad_idx]
    bad_str <- paste0(head(sort(bad_vals), 5), collapse = ", ")
    min_str <- paste0(range[1, ], collapse = ", ")
    max_str <- paste0(range[2, ], collapse = ", ")
    showNotification(
      sprintf(
        "Each target mean must lie between its min (%s) and max (%s). Offending means: %s",
        min_str, max_str,
        bad_str
      ),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0) {
    showNotification(
      sprintf("Convergence Threshold (%s) must be a single non-negative numeric value.", thresh),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("Max Iterations (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  # init_temp may be NULL (function default)
  if (!is.null(init_temp) &&
      (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0)) {
    showNotification(
      sprintf("Initial Temperature (%s) must be a single positive number, or left empty.", init_temp),
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "Cooling Rate must be a single number between 0 and 1, or left empty.",
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("Max Restarts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}


# input checks aov
#' Validate inputs for optim_aov (in Shiny)
#'
#' @noRd
check_aov_inputs <- function(
    N,
    levels,
    target_group_means,
    range,
    thresh,
    max_iter,
    init_temp,
    cooling_rate,
    max_starts
) {
  if (!is.numeric(N) || length(N) != 1 || N < 10  || N != as.integer(N)) {
    showNotification(
      sprintf("Number of Subjects (%s) must be a single integer >= 10.", N),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(levels) || length(levels) < 1 || anyNA(levels) ||
      any(levels != as.integer(levels))) {
    showNotification(
      sprintf("Levels (%s) must be whole numbers.",
              paste(levels, collapse = ", ")),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(range) || length(range) != 2 || anyNA(range) || range[1] >= range[2]) {
    showNotification(
      "The outcome range must be two numbers with lower < upper.",
      type = "error"
    )
    return(FALSE)
  }
  if (any(target_group_means < range[1] | target_group_means > range[2])) {
    showNotification(
      sprintf(
        "All cell means must lie within the outcome range %s to %s. Got means: %s",
        range[1], range[2],
        paste0(head(sort(target_group_means[ target_group_means < range[1] | target_group_means > range[2] ]), 5), collapse = ", ")
      ),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0) {
    showNotification(
      sprintf("Convergence Threshold (%s) must be a single non-negative numeric value.", thresh),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("Max Iterations (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
    showNotification(
      sprintf("Initial Temperature (%s) must be a single positive number.", init_temp),
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "Cooling Rate must be a single number between 0 and 1, or left empty.",
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("Max Restarts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}


# input checks mlr
#' Validate inputs for optim_mlr (in Shiny)
#'
#' @noRd
check_mlr_inputs <- function(
    thresh,
    max_iter,
    init_temp,
    cooling_rate,
    hill_climbs,
    max_starts
) {
  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0) {
    showNotification(
      sprintf("Convergence Threshold (%s) must be a single non-negative numeric value.", thresh),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0) {
    showNotification(
      sprintf("Max Iterations (%s) must be a single positive number.", max_iter),
      type = "error"
    )
    return(FALSE)
  }
  # init_temp may be NULL (auto-calibration)
  if (!is.null(init_temp)) {
    if (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0) {
      showNotification(
        sprintf("Initial Temperature (%s) must be a single positive number, or left empty for auto-calibration.", init_temp),
        type = "error"
      )
      return(FALSE)
    }
  }
  if (!(
    (is.numeric(cooling_rate) &&
     length(cooling_rate) == 1 &&
     cooling_rate > 0 &&
     cooling_rate < 1) ||
    is.null(cooling_rate)
  )) {
    showNotification(
      "Cooling Rate must be a single number between 0 and 1, or left empty.",
      type = "error"
    )
    return(FALSE)
  }
  if (!(
    is.null(hill_climbs) ||
    (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
     hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))
  )) {
    showNotification(
      sprintf("Hill-Climbing Iterations (%s) must be a non-negative integer.", hill_climbs),
      type = "error"
    )
    return(FALSE)
  }
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1) {
    showNotification(
      sprintf("Max Restarts (%s) must be a single positive integer.", max_starts),
      type = "error"
    )
    return(FALSE)
  }
  # all checks passed
  TRUE
}
