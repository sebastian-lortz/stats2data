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
#' @param seed Integer or \code{NULL}. If non-\code{NULL}, a reproducible
#'   seed is generated per run. Default \code{NULL}.
#' @param stop_conv Logical. If \code{TRUE}, stop launching further runs as
#'   soon as one run has converged, i.e. its \code{best_error} is below the
#'   convergence threshold. Default \code{FALSE}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Controls the outer progress bar only; individual runs
#'   always execute silently. Default \code{"console"}.
#'
#' @return An object of class \code{stats2data_parallel} — a list with:
#' \describe{
#'   \item{results}{List of completed runs. Each element is the object
#'     returned by \code{FUN} (class \code{stats2data_vec},
#'     \code{stats2data_mlr}, or \code{stats2data_aov}).}
#'   \item{best}{The single result with the lowest \code{best_error}.}
#'   \item{module}{Character. One of \code{"vec"}, \code{"mlr"}, or
#'     \code{"aov"}, inferred from the class of the first result.}
#'   \item{runs}{Integer. Number of runs actually completed.}
#'   \item{runs_requested}{Integer. Number of runs requested via \code{runs}.}
#'   \item{converged}{Logical. \code{TRUE} if a converged run triggered early
#'     stopping.}
#'   \item{thresh}{Numeric or \code{NULL}. Threshold used to judge convergence.}
#' }
#'
#' @details
#' When \code{cores > 1}, a \code{\link[future]{multisession}} plan is set up
#' and torn down automatically. Any pre-existing \code{future} plan is
#' restored on exit.
#'
#' With \code{stop_conv = TRUE} and \code{cores > 1}, runs are dispatched in
#' batches of \code{cores} and the convergence check happens after each batch.
#' Up to \code{cores - 1} runs may therefore complete after the first
#' converged run; they are kept in \code{results}. With
#' \code{stop_conv = FALSE} all runs are dispatched in a single call, as before.
#'
#' Convergence of a single run is judged as \code{best_error < thresh}. For
#' \code{stats2data_vec} results, where \code{best_error} is a list of
#' per-variable errors, every component must be below \code{thresh}.
#'
#' @examples
#' \dontrun{
#' # Parallel ANOVA runs
#' res <- parallel_optim(
#'   FUN  = optim_aov,
#'   args = list(S = 30, levels = c(2, 2), ...),
#'   runs = 20, cores = 4
#' )
#' summary(res)
#'
#' # Stop as soon as one run converges
#' res <- parallel_optim(
#'   FUN  = optim_mlr,
#'   args = list(N = 100, thresh = 1e-3, ...),
#'   runs = 20, cores = 4, stop_conv = TRUE
#' )
#' }
#'
#' @export
parallel_optim <- function(FUN,
                           args,
                           runs          = 10L,
                           cores         = 1L,
                           seed          = NULL,
                           stop_conv     = FALSE,
                           progress_mode = "console") {


  # input validation
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
  if (!is.logical(stop_conv) || length(stop_conv) != 1L || is.na(stop_conv)) {
    stop("`stop_conv` must be a single logical (TRUE or FALSE).", call. = FALSE)
  }
  if (!is.character(progress_mode) || length(progress_mode) != 1L ||
      !progress_mode %in% c("console", "shiny", "off")) {
    stop('`progress_mode` must be "console", "shiny", or "off".', call. = FALSE)
  }

  runs  <- as.integer(runs)
  cores <- as.integer(cores)

  # error helpers
  error_components <- function(r) {
    e <- r$best_error
    if (is.list(e)) {
      vapply(e, function(x) {
        if (is.numeric(x) && length(x) == 1L && is.finite(x)) x else Inf
      }, numeric(1L))
    } else if (is.numeric(e) && length(e) == 1L && is.finite(e)) {
      as.numeric(e)
    } else {
      Inf
    }
  }
  # ranking score
  run_score <- function(r) mean(error_components(r))
  # convergence
  is_converged <- function(r, thresh) {
    comp <- error_components(r)
    all(is.finite(comp)) && max(comp) < thresh
  }

  # convergence threshold
  thresh <- NULL
  if (stop_conv) {
    if (!is.null(args$thresh)) {
      thresh <- args$thresh
    } else {
      fml <- formals(FUN)$thresh
      if (!is.null(fml) && !identical(fml, quote(expr = ))) {
        thresh <- tryCatch(eval(fml, envir = environment(FUN)),
                           error = function(e) NULL)
      }
    }
    if (!is.numeric(thresh) || length(thresh) != 1L ||
        !is.finite(thresh) || thresh < 0) {
      warning("`stop_conv = TRUE` but no usable `thresh` was found in `args` ",
              "or in the formals of `FUN`; early stopping is disabled.",
              call. = FALSE)
      stop_conv <- FALSE
      thresh    <- NULL
    }
  }

  # per-run RNG
  seeds <- NULL
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      get(".Random.seed", envir = globalenv()) else NULL
    old_kind <- RNGkind("L'Ecuyer-CMRG")[1L]
    on.exit({
      RNGkind(old_kind)
      if (!is.null(old_seed))
        assign(".Random.seed", old_seed, envir = globalenv())
    }, add = TRUE)

    set.seed(as.integer(seed))
    seeds <- vector("list", runs)
    s     <- get(".Random.seed", envir = globalenv())
    for (i in seq_len(runs)) {
      seeds[[i]] <- s
      s          <- parallel::nextRNGStream(s)
    }
  }

  # worker function
  run_one <- function(i) {
    do.call(FUN, args)
  }

  results   <- vector("list", runs)
  n_done    <- 0L
  converged <- FALSE

  # execute
  if (cores > 1L) {
    # Multi-core
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

    # batch only when early stopping is requested
    idx <- seq_len(runs)
    batches <- if (stop_conv) {
      split(idx, ceiling(idx / min(cores, runs)))
    } else {
      list(idx)
    }

    progressr::with_progress({
      p <- progressr::progressor(steps = runs)
      for (b in batches) {
        res_b <- future.apply::future_lapply(
          b,
          function(i) {
            res <- run_one(i)
            p()
            res
          },
          future.seed = if (is.null(seeds)) TRUE else seeds[b]
        )
        results[b] <- res_b
        n_done     <- max(b)

        if (stop_conv &&
            any(vapply(res_b, is_converged, logical(1L), thresh = thresh))) {
          converged <- TRUE
          break
        }
      }
    }, handlers = handler)

  } else {
    # Sequential
    args$progress_mode <- progress_mode

    if (progress_mode != "off") {
      cat(sprintf("Running %d sequential replications...\n", runs))
    }
    for (i in seq_len(runs)) {
      if (!is.null(seeds)) {
        assign(".Random.seed", seeds[[i]], envir = globalenv())
      }
      if (progress_mode != "off") {
        cat(sprintf("\n--- Run %d / %d ---\n", i, runs))
      }
      results[[i]] <- run_one(i)
      n_done       <- i

      if (stop_conv && is_converged(results[[i]], thresh)) {
        converged <- TRUE
        break
      }
    }
  }

  # drop slots
  results <- results[seq_len(n_done)]

  if (converged && progress_mode != "off") {
    cat(sprintf("\nConverged after %d of %d runs (thresh = %s).\n",
                n_done, runs, format(thresh)))
  }

  # identify module
  first_class <- class(results[[1L]])[1L]
  module <- switch(first_class,
                   stats2data_vec = "vec",
                   stats2data_mlr = "mlr",
                   stats2data_aov = "aov",
                   stop("Unrecognised result class: '", first_class, "'. ",
                        "`FUN` must return a stats2data_vec, stats2data_mlr, ",
                        "or stats2data_aov object.", call. = FALSE)
  )

  # best run
  errors   <- vapply(results, run_score, numeric(1L))
  best_idx <- which.min(errors)

  # return
  structure(
    list(
      results        = results,
      best           = results[[best_idx]],
      module         = module,
      runs           = length(results),
      runs_requested = runs,
      converged      = converged,
      thresh         = thresh
    ),
    class = "stats2data_parallel"
  )
}


# print

#' @export
print.stats2data_parallel <- function(x, ...) {
  cat("stats2data parallel result\n")
  cat("  Module:", x$module, "\n")
  if (!is.null(x$runs_requested) && !identical(x$runs, x$runs_requested)) {
    cat("  Runs:  ", x$runs, "of", x$runs_requested, "(stopped early)\n")
  } else {
    cat("  Runs:  ", x$runs, "\n")
  }
  if (isTRUE(x$converged)) {
    cat("  Converged: TRUE (thresh =", format(x$thresh), ")\n")
  }

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
