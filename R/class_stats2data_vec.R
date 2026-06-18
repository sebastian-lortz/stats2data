# --------------------------------------------------------------------------
# stats2data: S3 class for Descriptives module results
# --------------------------------------------------------------------------

# ---- Constructor ---------------------------------------------------------

#' Create a stats2data Descriptives result object
#'
#' Low-level constructor for objects of class \code{stats2data_vec}. Not intended to
#' be called directly by end users; called internally by
#' \code{\link{optim_vec}}.
#'
#' @param best_error List of per-variable best objective errors.
#' @param data Data frame of optimized variable columns.
#' @param inputs List of all input parameters used in the optimisation.
#' @param track_error List of per-variable numeric vectors tracking the best
#'   error at each iteration.
#'
#' @return An object of class \code{stats2data_vec}.
#'
#' @keywords internal
#' @noRd
new_s2d_vec <- function(best_error, data, inputs, track_error) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.list(inputs)) {
    stop("`inputs` must be a list.", call. = FALSE)
  }
  structure(
    list(
      best_error  = best_error,
      data        = data,
      inputs      = inputs,
      track_error = track_error
    ),
    class = "stats2data_vec"
  )
}


# ---- print ---------------------------------------------------------------

#' Print a stats2data Descriptives result
#'
#' @param x An object of class \code{stats2data_vec}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.stats2data_vec <- function(x, ...) {
  n_var <- ncol(x$data)
  cat("stats2data Descriptives result\n")
  cat("  Variables:", n_var, "\n")
  cat("  N:        ", nrow(x$data), "\n")

  errs <- vapply(x$best_error, function(e) {
    if (is.numeric(e) && length(e) == 1L && is.finite(e)) e else NA_real_
  }, numeric(1))
  names(errs) <- colnames(x$data)

  cat("  Best error per variable:\n")
  for (nm in names(errs)) {
    cat("    ", nm, ": ", format(errs[nm], digits = 4), "\n", sep = "")
  }
  invisible(x)
}


# ---- get_stats -----------------------------------------------------------

#' @rdname get_stats
#' @export
get_stats.stats2data_vec <- function(result, ...) {
  d <- result$data
  list(
    mean = vapply(d, mean, numeric(1)),
    sd   = vapply(d, stats::sd, numeric(1))
  )
}


# ---- get_rmse ------------------------------------------------------------

#' @rdname get_rmse
#' @export
get_rmse.stats2data_vec <- function(result, ...) {
  s   <- get_stats(result)
  tm  <- result$inputs$target_mean
  tsd <- result$inputs$target_sd

  list(
    rmse_mean = sqrt(mean((s$mean - tm)^2)),
    rmse_sd   = sqrt(mean((s$sd - tsd)^2, na.rm = TRUE))
  )
}

# ---- summary -------------------------------------------------------------

#' Summarize a stats2data Descriptives result
#'
#' Computes target-vs-simulated statistics and RMSE for a \code{stats2data_vec}
#' object.
#'
#' @param object An object of class \code{stats2data_vec}.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class \code{summary.stats2data_vec}, printed by
#'   \code{\link{print.summary.stats2data_vec}}.
#'
#' @export
summary.stats2data_vec <- function(object, ...) {
  s    <- get_stats(object)
  rmse <- get_rmse(object)
  inp  <- object$inputs

  comparison <- data.frame(
    variable    = names(s$mean),
    target_mean = as.numeric(inp$target_mean),
    sim_mean = as.numeric(s$mean),
    target_sd   = as.numeric(inp$target_sd),
    sim_sd = as.numeric(s$sd),
    row.names   = NULL
  )

  out <- list(
    comparison = comparison,
    rmse       = rmse,
    best_error = object$best_error,
    N          = nrow(object$data),
    n_var      = ncol(object$data)
  )
  class(out) <- "summary.stats2data_vec"
  out
}


#' Print a stats2data Descriptives summary
#'
#' @param x An object of class \code{summary.stats2data_vec}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print summary.stats2data_vec
#' @export
print.summary.stats2data_vec <- function(x, ...) {
  cat("stats2data Descriptives Summary\n")
  cat("-----------------------------------------------\n")
  cat("N:", x$N, " | Variables:", x$n_var, "\n\n")

  errs <- vapply(x$best_error, function(e) {
    if (is.numeric(e) && length(e) == 1L && is.finite(e)) e else NA_real_
  }, numeric(1))
  cat("Best error per variable:\n")
  for (i in seq_along(errs)) {
    cat("  ", x$comparison$variable[i], ": ",
        format(errs[i], digits = 4), "\n", sep = "")
  }

  cat("\nRMSE\n")
  cat("  Means:", format(x$rmse$rmse_mean, digits = 4), "\n")
  cat("  SDs:  ", format(x$rmse$rmse_sd, digits = 4), "\n")

  cat("\nTarget vs. Simulated:\n")
  print.data.frame(x$comparison, row.names = FALSE, digits = 4)

  invisible(x)
}
