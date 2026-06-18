# --------------------------------------------------------------------------
# stats2data: S3 class for MLR module results
# --------------------------------------------------------------------------

# ---- Constructor ---------------------------------------------------------

#' Create a stats2data MLR result object
#'
#' Low-level constructor for objects of class \code{stats2data_mlr}. Not intended to
#' be called directly by end users; called internally by
#' \code{\link{optim_mlr}}.
#'
#' @param best_error Numeric scalar. Minimum objective error achieved.
#' @param data Data frame of optimized predictor and outcome columns.
#' @param optim_vec An object of class \code{stats2data_vec} from the internal
#'   marginal optimisation step.
#' @param inputs List of all input parameters used in the optimisation.
#' @param track_error Numeric vector of best error at each iteration.
#' @param track_error_ratio Numeric vector of error ratios (correlation vs.
#'   regression component) per iteration.
#'
#' @return An object of class \code{stats2data_mlr}.
#'
#' @keywords internal
#' @noRd
new_s2d_mlr <- function(best_error, data, optim_vec, inputs,
                        track_error, track_error_ratio) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!inherits(optim_vec, "stats2data_vec")) {
    stop("`optim_vec` must be an object of class 'stats2data_vec'.", call. = FALSE)
  }
  if (!is.list(inputs)) {
    stop("`inputs` must be a list.", call. = FALSE)
  }
  structure(
    list(
      best_error        = best_error,
      data              = data,
      optim_vec         = optim_vec,
      inputs            = inputs,
      track_error       = track_error,
      track_error_ratio = track_error_ratio
    ),
    class = "stats2data_mlr"
  )
}


# ---- print ---------------------------------------------------------------

#' Print a stats2data MLR result
#'
#' @param x An object of class \code{stats2data_mlr}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.stats2data_mlr <- function(x, ...) {
  cat("stats2data MLR result\n")
  cat("  N:         ", x$inputs$N, "\n")
  cat("  Equation:  ", x$inputs$reg_equation, "\n")
  cat("  Best error:", format(x$best_error, digits = 6), "\n")
  invisible(x)
}


# ---- get_stats -----------------------------------------------------------

#' @rdname get_stats
#' @export
get_stats.stats2data_mlr <- function(result, ...) {
  d     <- result$data
  inp   <- result$inputs
  frm   <- stats::as.formula(inp$reg_equation)
  vars  <- names(inp$target_mean)
  model <- stats::lm(frm, data = d)

  cor_mat  <- stats::cor(d[, vars])
  cor_vals <- cor_mat[upper.tri(cor_mat)]

  list(
    model = model,
    reg   = stats::coef(model),
    se    = summary(model)$coefficients[, "Std. Error"],
    cor   = cor_vals,
    mean  = vapply(d[, vars], mean, numeric(1)),
    sd    = vapply(d[, vars], stats::sd, numeric(1))
  )
}


# ---- get_rmse ------------------------------------------------------------

#' @rdname get_rmse
#' @export
get_rmse.stats2data_mlr <- function(result, ...) {
  s   <- get_stats(result)
  inp <- result$inputs
  rmse_fn <- function(x, y) sqrt(mean((x - y)^2, na.rm = TRUE))

  tc <- inp$target_cor
  tr <- inp$target_reg
  ts <- inp$target_se

  list(
    rmse_cor = rmse_fn(s$cor[!is.na(tc)], tc[!is.na(tc)]),
    rmse_reg = rmse_fn(s$reg[!is.na(tr)], tr[!is.na(tr)]),
    rmse_se  = if (!is.null(ts)) {
      rmse_fn(s$se[!is.na(ts)], ts[!is.na(ts)])
    } else {
      NA_real_
    }
  )
}


# ---- coef ----------------------------------------------------------------

#' Extract regression coefficients from a stats2data MLR result
#'
#' Fits the regression model stored in \code{object$inputs$reg_equation} to
#' the simulated data and returns the estimated coefficients.
#'
#' @param object An object of class \code{stats2data_mlr}.
#' @param ... Additional arguments (unused).
#'
#' @return A named numeric vector of regression coefficients.
#'
#' @export
coef.stats2data_mlr <- function(object, ...) {
  frm <- stats::as.formula(object$inputs$reg_equation)
  fit <- stats::lm(frm, data = object$data)
  stats::coef(fit)
}


# ---- summary -------------------------------------------------------------

#' Summarize a stats2data MLR result
#'
#' Computes target-vs-simulated statistics and RMSE for a \code{stats2data_mlr}
#' object.
#'
#' @param object An object of class \code{stats2data_mlr}.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class \code{summary.stats2data_mlr}, printed by
#'   \code{\link{print.summary.stats2data_mlr}}.
#'
#' @export
summary.stats2data_mlr <- function(object, ...) {
  s    <- get_stats(object)
  rmse <- get_rmse(object)
  inp  <- object$inputs
  vars <- names(inp$target_mean)

  # means and SDs comparison
  descriptives <- data.frame(
    variable    = vars,
    target_mean = as.numeric(inp$target_mean),
    sim_mean    = as.numeric(s$mean),
    target_sd   = as.numeric(inp$target_sd),
    sim_sd      = as.numeric(s$sd),
    row.names   = NULL
  )

  # coefficients comparison
  reg_names <- names(s$reg)
  coefficients <- data.frame(
    term       = reg_names,
    target_reg = as.numeric(inp$target_reg),
    sim_reg    = as.numeric(s$reg),
    row.names  = NULL
  )
  if (!is.null(inp$target_se)) {
    coefficients$target_se <- as.numeric(inp$target_se)
    coefficients$sim_se    <- as.numeric(s$se)
  }

  # correlations comparison
  n_cor <- length(s$cor)
  pair_labels <- character(n_cor)
  idx <- 1L
  for (i in seq_len(length(vars) - 1L)) {
    for (j in (i + 1L):length(vars)) {
      pair_labels[idx] <- paste(vars[i], vars[j], sep = "-")
      idx <- idx + 1L
    }
  }
  correlations <- data.frame(
    pair       = pair_labels,
    target_cor = as.numeric(inp$target_cor),
    sim_cor    = as.numeric(s$cor),
    row.names  = NULL
  )

  out <- list(
    descriptives = descriptives,
    coefficients = coefficients,
    correlations = correlations,
    rmse         = rmse,
    best_error   = object$best_error,
    N            = inp$N,
    reg_equation = inp$reg_equation
  )
  class(out) <- "summary.stats2data_mlr"
  out
}


#' Print a stats2data MLR summary
#'
#' @param x An object of class \code{summary.stats2data_mlr}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print summary.stats2data_mlr
#' @export
print.summary.stats2data_mlr <- function(x, ...) {
  cat("stats2data MLR Summary\n")
  cat("-----------------------------------------------\n")
  cat("N:", x$N, " | Model:", x$reg_equation, "\n")
  cat("Best error:", format(x$best_error, digits = 6), "\n")

  cat("\nRMSE\n")
  cat("  Correlations:            ", format(x$rmse$rmse_cor, digits = 4), "\n")
  cat("  Regression Coefficients: ", format(x$rmse$rmse_reg, digits = 4), "\n")
  if (!is.na(x$rmse$rmse_se)) {
    cat("  Standard Errors:         ", format(x$rmse$rmse_se, digits = 4), "\n")
  }

  cat("\nDescriptives (Target vs. Simulated):\n")
  print.data.frame(x$descriptives, row.names = FALSE, digits = 4)

  cat("\nCoefficients (Target vs. Simulated):\n")
  print.data.frame(x$coefficients, row.names = FALSE, digits = 4)

  cat("\nCorrelations (Target vs. Simulated):\n")
  print.data.frame(x$correlations, row.names = FALSE, digits = 4)

  invisible(x)
}
