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


# ---- Internal: reuse the user-supplied formula --------------------------

#' Reconstruct the afex-compatible formula from the stored input formula.
#'
#' \code{optim_aov()} normalises the user-supplied formula to a one-sided
#' formula (LHS stripped); afex needs a two-sided formula with
#' \code{outcome} on the LHS. This helper handles both cases robustly and
#' protects against \code{deparse()} splitting long RHS expressions across
#' multiple character elements.
#'
#' @noRd
.aov_formula_from_inputs <- function(inp) {
  if (is.null(inp$formula)) {
    stop("Stored `inputs$formula` is missing; cannot reconstruct ANOVA model.",
         call. = FALSE)
  }
  fm <- inp$formula
  if (is.character(fm) && length(fm) == 1L) {
    fm <- stats::as.formula(fm)
  }
  if (!inherits(fm, "formula")) {
    stop("Stored `inputs$formula` is not a formula or character string.",
         call. = FALSE)
  }
  rhs_idx <- if (length(fm) == 3L) 3L else 2L  # 3L: two-sided; 2L: one-sided
  rhs <- paste(deparse(fm[[rhs_idx]]), collapse = " ")
  stats::as.formula(paste("outcome ~", rhs))
}


# ---- get_stats -----------------------------------------------------------

#' @rdname get_stats
#'
#' @section Cell ordering for \code{stats2data_aov}:
#' The vector \code{$mean} is returned in **sorted-key** cell order
#' (Factor1 fastest, then Factor2, then Factor3, ...). This matches the
#' order the optimiser uses internally and the order in which
#' \code{target_group_means} is consumed by \code{\link{optim_aov}}. The
#' returned vector is named with the cell identifier (e.g. \code{"1_1"},
#' \code{"1_2"}, \code{"2_1"}, ...) so the cell each value belongs to is
#' unambiguous downstream.
#'
#' @section Model formula:
#' \code{get_stats} reuses the formula stored in
#' \code{result$inputs$formula} rather than reconstructing one from the
#' factor names. This guarantees the model fitted here is the same as the
#' one the optimiser used, eliminating drift across afex versions or design
#' types (purely-within, purely-between, mixed).
#'
#' @export
get_stats.stats2data_aov <- function(result, ...) {
  d   <- result$data
  inp <- result$inputs

  if (!requireNamespace("afex", quietly = TRUE)) {
    stop("Package 'afex' is required for ANOVA statistics.", call. = FALSE)
  }

  # ---- model fit -------------------------------------------------------
  afex_formula <- .aov_formula_from_inputs(inp)

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

  # ---- observed cell means in sorted-key order, with cell-ID names -----
  factor_cols <- grep("^Factor", names(d), value = TRUE)
  group_id    <- apply(d[, factor_cols, drop = FALSE], 1, paste0, collapse = "_")
  obs_means   <- tapply(d$outcome, group_id, mean)
  obs_means   <- obs_means[order(names(obs_means))]
  nm          <- names(obs_means)
  obs_means   <- as.numeric(obs_means)
  names(obs_means) <- nm

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
  tf <- result$inputs$target_f_list$F_value
  tm <- result$inputs$target_group_means

  list(
    rmse_F    = sqrt(mean((s$F_value - tf)^2)),
    rmse_mean = sqrt(mean((as.numeric(s$mean) - tm)^2))
  )
}


# ---- summary -------------------------------------------------------------

#' Summarize a stats2data ANOVA result
#'
#' Computes target-vs-simulated statistics and RMSE for a
#' \code{stats2data_aov} object.
#'
#' @param object An object of class \code{stats2data_aov}.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class \code{summary.stats2data_aov}, printed by
#'   \code{\link{print.summary.stats2data_aov}}. The list always contains
#'   \code{f_comparison} and \code{means_comparison} data frames, even if
#'   \code{get_stats()} returned \code{NA} values; in that case the
#'   simulated columns are \code{NA} and the print method emits a warning.
#'
#' @export
summary.stats2data_aov <- function(object, ...) {
  s    <- get_stats(object)
  rmse <- get_rmse(object)
  inp  <- object$inputs

  # F-values comparison
  f_comparison <- data.frame(
    effect    = inp$target_f_list$effect,
    target_F  = as.numeric(inp$target_f_list$F_value),
    sim_F     = as.numeric(s$F_value),
    row.names = NULL
  )

  # Group-means comparison.
  # Cell labels follow the sorted-key order used internally and by
  # `target_group_means`: Factor1 fastest, then Factor2, ...
  level_grid <- expand.grid(lapply(inp$levels, seq_len))
  level_grid <- level_grid[do.call(order, level_grid), , drop = FALSE]
  cell_labels <- apply(level_grid, 1, function(r) {
    paste(paste0("F", seq_along(r), "=", r), collapse = ", ")
  })

  n_groups <- length(inp$target_group_means)
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
#' Prints (in order): the design header, RMSE summary, F-values comparison
#' table, and group-means comparison table. Each comparison block is
#' preceded by a horizontal rule. If a comparison data frame is missing or
#' empty, the method emits an explicit warning instead of silently skipping
#' it; this makes diagnostics easier when an upstream step has gone wrong.
#'
#' @param x An object of class \code{summary.stats2data_aov}.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#'
#' @method print summary.stats2data_aov
#' @export
print.summary.stats2data_aov <- function(x, ...) {
  rule <- "-----------------------------------------------"
  cat("stats2data ANOVA Summary\n")
  cat(rule, "\n", sep = "")
  cat("Design:     ", x$design, " (", x$factor_type, ")\n", sep = "")
  cat("Subjects:  ", x$N, "\n")
  cat("Best error:", format(x$best_error, digits = 6), "\n")

  cat("\nRMSE\n")
  cat("  F-statistics:", format(x$rmse$rmse_F, digits = 4), "\n")
  cat("  Group means: ", format(x$rmse$rmse_mean, digits = 4), "\n")

  # ---- F-values block --------------------------------------------------
  cat("\n", rule, "\n", sep = "")
  cat("F-values (Target vs. Simulated):\n")
  if (is.null(x$f_comparison) || !is.data.frame(x$f_comparison) ||
      nrow(x$f_comparison) == 0L) {
    warning("`f_comparison` is missing or empty; ",
            "check that `get_stats()` succeeded.", call. = FALSE)
    cat("  <no F-value comparison available>\n")
  } else {
    print.data.frame(x$f_comparison, row.names = FALSE, digits = 4)
  }

  # ---- Group means block -----------------------------------------------
  cat("\n", rule, "\n", sep = "")
  cat("Group Means (Target vs. Simulated):\n")
  if (is.null(x$means_comparison) || !is.data.frame(x$means_comparison) ||
      nrow(x$means_comparison) == 0L) {
    warning("`means_comparison` is missing or empty; ",
            "check that `get_stats()` succeeded.", call. = FALSE)
    cat("  <no group-mean comparison available>\n")
  } else {
    print.data.frame(x$means_comparison, row.names = FALSE, digits = 4)
  }

  invisible(x)
}
