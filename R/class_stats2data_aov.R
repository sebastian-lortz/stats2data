# --------------------------------------------------------------------------
# stats2data: S3 class for ANOVA module results
# --------------------------------------------------------------------------

# ---- Constructor ---------------------------------------------------------

#' Create a stats2data ANOVA result object
#'
#' Low-level constructor for objects of class \code{stats2data_aov}. Not intended to
#' be called directly by end users; called internally by
#' \code{\link{optim_aov}}.
#'
#' @param best_error Numeric scalar. Minimum objective error achieved.
#' @param data Data frame with columns \code{ID}, factor columns, and
#'   \code{outcome}.
#' @param inputs List of all input parameters used in the optimisation.
#' @param adjusted_targets List with elements \code{group_means} and
#'   \code{F_values} containing the (possibly adjusted) target values used
#'   during optimisation.
#' @param track_error Numeric vector of best error at each iteration.
#'
#' @return An object of class \code{stats2data_aov}.
#'
#' @keywords internal
#' @noRd
new_s2d_aov <- function(best_error, data, inputs, adjusted_targets,
                        track_error) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.list(inputs)) {
    stop("`inputs` must be a list.", call. = FALSE)
  }
  if (!is.list(adjusted_targets) ||
      !all(c("group_means", "F_values") %in% names(adjusted_targets))) {
    stop("`adjusted_targets` must be a list with 'group_means' and 'F_values'.",
         call. = FALSE)
  }
  structure(
    list(
      best_error       = best_error,
      data             = data,
      inputs           = inputs,
      adjusted_targets = adjusted_targets,
      track_error      = track_error
    ),
    class = "stats2data_aov"
  )
}


# ---- print ---------------------------------------------------------------

#' Print a stats2data ANOVA result
#'
#' @param x An object of class \code{stats2data_aov}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.stats2data_aov <- function(x, ...) {
  inp <- x$inputs
  design_str <- paste(inp$levels, collapse = " x ")
  type_str   <- paste(inp$factor_type, collapse = ", ")

  cat("stats2data ANOVA result\n")
  cat("  Design:    ", design_str, " (", type_str, ")\n", sep = "")
  cat("  Subjects:  ", inp$N, "\n")
  cat("  Effects:   ", paste(inp$target_f_list$effect, collapse = ", "), "\n")
  cat("  Best error:", format(x$best_error, digits = 6), "\n")
  invisible(x)
}


# ---- get_stats -----------------------------------------------------------

#' @rdname get_stats
#' @export
get_stats.stats2data_aov <- function(result, ...) {
  d   <- result$data
  inp <- result$inputs

  if (!requireNamespace("afex", quietly = TRUE)) {
    stop("Package 'afex' is required for ANOVA statistics.", call. = FALSE)
  }

  # reconstruct afex-compatible formula with Error() term
  factor_names <- paste0("Factor", seq_along(inp$levels))
  within_names <- factor_names[inp$factor_type == "within"]

  if (length(within_names) == 0L) {
    # purely between: outcome ~ Factor1 * Factor2 + Error(ID)
    rhs <- paste(factor_names, collapse = " * ")
    afex_formula <- stats::as.formula(
      paste("outcome ~", rhs, "+ Error(ID)")
    )
  } else if (all(inp$factor_type == "within")) {
    # purely within: outcome ~ 1 + Error(ID / (Factor1 * Factor2))
    within_part <- paste(within_names, collapse = " * ")
    afex_formula <- stats::as.formula(
      paste("outcome ~ 1 + Error(ID / (", within_part, "))")
    )
  } else {
    # mixed: outcome ~ BetweenFactors + Error(ID / (WithinFactors))
    between_names <- factor_names[inp$factor_type == "between"]
    between_part  <- paste(between_names, collapse = " * ")
    within_part   <- paste(within_names, collapse = " * ")
    afex_formula  <- stats::as.formula(
      paste("outcome ~", between_part, "+ Error(ID / (", within_part, "))")
    )
  }

  fit <- afex::aov_car(
    formula   = afex_formula,
    data      = d,
    factorize = TRUE,
    type      = 3
  )
  tab <- fit$anova_table
  rn  <- trimws(rownames(tab))
  eff <- inp$target_f_list$effect
  f_vals <- vapply(eff, function(e) {
    row <- which(rn == e)
    if (length(row) == 0L) NA_real_ else tab[row, "F"]
  }, numeric(1))

  # compute observed group means from data
  factor_cols <- grep("^Factor", names(d), value = TRUE)
  group_id    <- apply(d[, factor_cols, drop = FALSE], 1, paste0, collapse = "_")
  obs_means   <- tapply(d$outcome, group_id, mean)
  obs_means   <- as.numeric(obs_means[order(names(obs_means))])

  list(
    model   = tab,
    F_value = as.numeric(f_vals),
    mean    = obs_means
  )
}


# ---- get_rmse ------------------------------------------------------------

#' @rdname get_rmse
#' @export
get_rmse.stats2data_aov <- function(result, ...) {
  s  <- get_stats(result)
  tf <- result$inputs$target_f_list$F
  tm <- result$inputs$target_group_means

  list(
    rmse_F    = sqrt(mean((s$F_value - tf)^2)),
    rmse_mean = sqrt(mean((s$mean - tm)^2))
  )
}


# ---- summary -------------------------------------------------------------

#' Summarize a stats2data ANOVA result
#'
#' Computes target-vs-simulated statistics and RMSE for a \code{stats2data_aov}
#' object.
#'
#' @param object An object of class \code{stats2data_aov}.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class \code{summary.stats2data_aov}, printed by
#'   \code{\link{print.summary.stats2data_aov}}.
#'
#' @export
summary.stats2data_aov <- function(object, ...) {
  s    <- get_stats(object)
  rmse <- get_rmse(object)
  inp  <- object$inputs

  # F-values comparison
  f_comparison <- data.frame(
    effect   = inp$target_f_list$effect,
    target_F = as.numeric(inp$target_f_list$F),
    sim_F    = as.numeric(s$F_value),
    row.names = NULL
  )

  # group means comparison
  n_groups <- length(inp$target_group_means)
  # build cell labels from factor levels
  level_grid <- expand.grid(lapply(inp$levels, seq_len))
  level_grid <- level_grid[do.call(order, level_grid), , drop = FALSE]
  cell_labels <- apply(level_grid, 1, function(r) {
    paste(paste0("F", seq_along(r), "=", r), collapse = ", ")
  })

  means_comparison <- data.frame(
    cell        = cell_labels[seq_len(n_groups)],
    target_mean = as.numeric(inp$target_group_means),
    sim_mean    = as.numeric(s$mean),
    row.names   = NULL
  )

  design_str <- paste(inp$levels, collapse = " x ")
  type_str   <- paste(inp$factor_type, collapse = ", ")

  out <- list(
    f_comparison        = f_comparison,
    means_comparison    = means_comparison,
    rmse                = rmse,
    best_error          = object$best_error,
    N                   = inp$N,
    design              = design_str,
    factor_type         = type_str
  )
  class(out) <- "summary.stats2data_aov"
  out
}


#' Print a stats2data ANOVA summary
#'
#' @param x An object of class \code{summary.stats2data_aov}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print summary.stats2data_aov
#' @export
print.summary.stats2data_aov <- function(x, ...) {
  cat("stats2data ANOVA Summary\n")
  cat("-----------------------------------------------\n")
  cat("Design:     ", x$design, " (", x$factor_type, ")\n", sep = "")
  cat("Subjects:  ", x$N, "\n")
  cat("Best error:", format(x$best_error, digits = 6), "\n")

  cat("\nRMSE\n")
  cat("  F-statistics:", format(x$rmse$rmse_F, digits = 4), "\n")
  cat("  Group means: ", format(x$rmse$rmse_mean, digits = 4), "\n")

  cat("\nF-values (Target vs. Simulated):\n")
  print(x$f_comparison, row.names = FALSE, digits = 4)

  cat("\nGroup Means (Target vs. Simulated):\n")
  print(x$means_comparison, row.names = FALSE, digits = 4)

  invisible(x)
}
