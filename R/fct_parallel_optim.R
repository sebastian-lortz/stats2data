#' Run an optimization function multiple times in parallel
#'
#' Wrapper that executes any \code{optim_*} function repeatedly and collects
#' the results.
#'
#' @param FUN Function. One of \code{\link{optim_vec}}, \code{\link{optim_mlr}},
#'   or \code{\link{optim_aov}}.
#' @param args List. Named arguments forwarded to \code{FUN}.
#'   \code{progress_mode} is silently forced to \code{"off"} inside workers.
#' @param runs Integer. Number of independent replications. Default \code{10}.
#' @param cores Integer. Number of parallel workers. Default \code{1}
#'   (sequential). Values \code{> 1} require the \pkg{future} and
#'   \pkg{future.apply} packages.
#' @param seed Integer or \code{NULL}. If non-\code{NULL}, passed to
#'   \code{future.apply::future_lapply} as \code{future.seed} for
#'   reproducibility. Default \code{NULL}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Controls the outer progress bar only; individual runs
#'   always execute silently. Default \code{"console"}.
#'
#' @return An object of class \code{stats2data_parallel} — a list with:
#' \describe{
#'   \item{results}{List of length \code{runs}. Each element is the object
#'     returned by \code{FUN} (class \code{stats2data_vec},
#'     \code{stats2data_mlr}, or \code{stats2data_aov}).}
#'   \item{best}{The single result with the lowest \code{best_error}.}
#'   \item{module}{Character. One of \code{"vec"}, \code{"mlr"}, or
#'     \code{"aov"}, inferred from the class of the first result.}
#'   \item{runs}{Integer. Number of runs completed.}
#' }
#'
#' @details
#' When \code{cores > 1}, a \code{\link[future]{multisession}} plan is set up
#' and torn down automatically. Any pre-existing \code{future} plan is
#' restored on exit.
#'
#' @examples
#' \dontrun{
#' # Parallel ANOVA runs
#' res <- parallel_optim(
#'   FUN  = optim_aov,
#'   args = list(S = 30, levels = c(2, 2), ...),
#'   runs = 20, cores = 4
#' )
#' summary(res)          # aggregated summary
#' get_stats(res)        # pooled stats across runs
#' get_rmse(res)         # between-run and target RMSE
#' res$best              # single best result
#' }
#'
#' @export
parallel_optim <- function(FUN,
                           args,
                           runs          = 10L,
                           cores         = 1L,
                           seed          = NULL,
                           progress_mode = "console") {


  # ---- input validation --------------------------------------------------
  if (!is.function(FUN)) {
    stop("`FUN` must be a function (one of optim_vec, optim_mlr, optim_aov).",
         call. = FALSE)
  }
  if (!is.list(args)) {
    stop("`args` must be a named list of arguments to pass to `FUN`.",
         call. = FALSE)
  }
  if (!is.numeric(runs) || length(runs) != 1L ||
      runs < 1L || runs != as.integer(runs)) {
    stop("`runs` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(cores) || length(cores) != 1L ||
      cores < 1L || cores != as.integer(cores)) {
    stop("`cores` must be a single positive integer.", call. = FALSE)
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L)) {
    stop("`seed` must be NULL or a single integer.", call. = FALSE)
  }
  if (!is.character(progress_mode) || length(progress_mode) != 1L ||
      !progress_mode %in% c("console", "shiny", "off")) {
    stop('`progress_mode` must be "console", "shiny", or "off".', call. = FALSE)
  }

  runs  <- as.integer(runs)
  cores <- as.integer(cores)

  # ---- worker function ---------------------------------------------------
  run_one <- function(i) {
    do.call(FUN, args)
  }

  # ---- execute -----------------------------------------------------------
  if (cores > 1L) {
    # Multi-core: suppress per-run progress (workers can't signal to the
    # main process), use an outer progressor that ticks once per completed run.
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for ",
           "parallel execution (cores > 1). Install them with:\n",
           "  install.packages(c(\"future\", \"future.apply\"))",
           call. = FALSE)
    }

    args$progress_mode <- "off"

    old_plan <- future::plan(future::multisession, workers = cores)
    on.exit(future::plan(old_plan), add = TRUE)

    handler <- switch(progress_mode,
                      console = list(progressr::handler_txtprogressbar()),
                      shiny   = list(progressr::handler_shiny()),
                      off     = list(progressr::handler_void())
    )

    progressr::with_progress({
      p <- progressr::progressor(steps = runs)
      results <- future.apply::future_lapply(
        seq_len(runs),
        function(i) {
          res <- run_one(i)
          p()
          res
        },
        future.seed = seed %||% TRUE
      )
    }, handlers = handler)

  } else {
    # Sequential: let each optim_* call handle its own progress bar.
    # Do NOT wrap in with_progress here — nesting with_progress contexts
    # causes the inner progressor signals to leak into the outer handler
    # and trigger "no longer listening" warnings.
    args$progress_mode <- progress_mode

    if (progress_mode != "off") {
      cat(sprintf("Running %d sequential replications...\n", runs))
    }
    results <- lapply(seq_len(runs), function(i) {
      if (progress_mode != "off") {
        cat(sprintf("\n--- Run %d / %d ---\n", i, runs))
      }
      run_one(i)
    })
  }

  # ---- identify module ---------------------------------------------------
  first_class <- class(results[[1L]])[1L]
  module <- switch(first_class,
                   stats2data_vec = "vec",
                   stats2data_mlr = "mlr",
                   stats2data_aov = "aov",
                   stop("Unrecognised result class: '", first_class, "'. ",
                        "`FUN` must return a stats2data_vec, stats2data_mlr, ",
                        "or stats2data_aov object.", call. = FALSE)
  )

  # ---- find best run -----------------------------------------------------
  errors <- vapply(results, function(r) {
    e <- r$best_error
    if (is.list(e)) {
      # stats2data_vec stores per-variable errors as a list
      mean(vapply(e, function(x) {
        if (is.numeric(x) && length(x) == 1L && is.finite(x)) x else Inf
      }, numeric(1L)))
    } else if (is.numeric(e) && length(e) == 1L) {
      e
    } else {
      Inf
    }
  }, numeric(1L))

  best_idx <- which.min(errors)

  # ---- return ------------------------------------------------------------
  structure(
    list(
      results = results,
      best    = results[[best_idx]],
      module  = module,
      runs    = runs
    ),
    class = "stats2data_parallel"
  )
}


# ---- print ---------------------------------------------------------------

#' @export
print.stats2data_parallel <- function(x, ...) {
  cat("stats2data parallel result\n")
  cat("  Module:", x$module, "\n")
  cat("  Runs:  ", x$runs, "\n")

  errors <- vapply(x$results, function(r) {
    e <- r$best_error
    if (is.list(e)) {
      mean(vapply(e, function(v) {
        if (is.numeric(v) && length(v) == 1L && is.finite(v)) v else NA_real_
      }, numeric(1L)), na.rm = TRUE)
    } else {
      as.numeric(e)
    }
  }, numeric(1L))

  cat("  Best error (across runs):", format(min(errors, na.rm = TRUE), digits = 6), "\n")
  cat("  Mean error (across runs):", format(mean(errors, na.rm = TRUE), digits = 6), "\n")
  invisible(x)
}
