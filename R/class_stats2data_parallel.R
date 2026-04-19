# --------------------------------------------------------------------------
# stats2data: S3 methods for stats2data_parallel results
# --------------------------------------------------------------------------

# ---- get_stats -----------------------------------------------------------

#' @rdname get_stats
#'
#' @details
#' For \code{stats2data_parallel} objects, \code{get_stats} aggregates each
#' numeric component returned by the per-run \code{get_stats} method across
#' all runs.  The result is a named list of data frames, one per component
#' (e.g., \code{F_value}, \code{mean}, \code{reg}, \code{cor}, \code{sd}),
#' each containing columns \code{mean}, \code{median}, \code{sd},
#' \code{min}, and \code{max}.
#'
#' @export
get_stats.stats2data_parallel <- function(result, ...) {

  stats_list <- lapply(result$results, get_stats)

  # identify numeric components (vectors/scalars) worth aggregating
  first <- stats_list[[1L]]
  numeric_comps <- names(first)[vapply(first, function(x) {
    is.numeric(x) && !inherits(x, "lm")
  }, logical(1L))]

  stats::setNames(lapply(numeric_comps, function(comp) {
    # stack each run's vector into a matrix (rows = elements, cols = runs)
    vals <- lapply(stats_list, `[[`, comp)
    mat  <- do.call(cbind, vals)

    data.frame(
      mean   = rowMeans(mat, na.rm = TRUE),
      median = apply(mat, 1, stats::median, na.rm = TRUE),
      sd     = apply(mat, 1, stats::sd,     na.rm = TRUE),
      min    = apply(mat, 1, min,            na.rm = TRUE),
      max    = apply(mat, 1, max,            na.rm = TRUE),
      row.names = names(vals[[1L]])
    )
  }), numeric_comps)
}


# ---- get_rmse ------------------------------------------------------------

#' @rdname get_rmse
#'
#' @details
#' For \code{stats2data_parallel} objects, \code{get_rmse} returns a list
#' with three elements:
#' \describe{
#'   \item{between_rmse}{Data frame.
#'     Per-metric summary (Mean, SD, Min, Max) of run-to-run RMSE, computed
#'     against the grand mean across runs.}
#'   \item{target_rmse}{Data frame.
#'     Per-metric summary of RMSE from each run to the original targets.}
#'   \item{raw}{List with numeric vectors \code{between} and \code{target}
#'     holding the per-run raw RMSE values, keyed by metric name.}
#' }
#'
#' @export
get_rmse.stats2data_parallel <- function(result, ...) {

  module <- result$module

  switch(module,
         vec = .rmse_parallel_vec(result),
         mlr = .rmse_parallel_mlr(result),
         aov = .rmse_parallel_aov(result))
}


# ---- Internal: RMSE helpers ---------------------------------------------

#' Summarise a named list of numeric vectors into a data frame
#' @noRd
.summarise_rmse <- function(rmse_list) {
  # drop metrics that are entirely NA (e.g. rmse_se when target_se is NULL)
  keep <- vapply(rmse_list, function(v) !all(is.na(v)), logical(1L))
  rmse_list <- rmse_list[keep]
  if (length(rmse_list) == 0L) {
    return(data.frame(metric = character(0), mean = numeric(0),
                      sd = numeric(0), min = numeric(0), max = numeric(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    metric = names(rmse_list),
    mean   = vapply(rmse_list, mean,      numeric(1L), na.rm = TRUE),
    sd     = vapply(rmse_list, stats::sd, numeric(1L), na.rm = TRUE),
    min    = vapply(rmse_list, min,       numeric(1L), na.rm = TRUE),
    max    = vapply(rmse_list, max,       numeric(1L), na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


# ---- vec module ----------------------------------------------------------

#' @noRd
.rmse_parallel_vec <- function(result) {

  stats_list  <- lapply(result$results, get_stats)
  inp         <- result$results[[1L]]$inputs
  target_mean <- inp$target_mean
  target_sd   <- inp$target_sd
  mean_dec    <- max(count_decimals(target_mean))
  sd_dec      <- max(count_decimals(target_sd))

  rounded <- lapply(stats_list, function(s) {
    list(mean = round(s$mean, mean_dec),
         sd   = round(s$sd,   sd_dec))
  })

  n_runs       <- length(rounded)
  overall_mean <- Reduce("+", lapply(rounded, `[[`, "mean")) / n_runs
  overall_sd   <- Reduce("+", lapply(rounded, `[[`, "sd"))   / n_runs

  # between-run
  between_mean <- vapply(rounded, function(r)
    sqrt(mean((r$mean - overall_mean)^2)), numeric(1L))
  between_sd   <- vapply(rounded, function(r)
    sqrt(mean((r$sd   - overall_sd)^2)),   numeric(1L))

  # target
  target_rmse_mean <- vapply(rounded, function(r)
    sqrt(mean((r$mean - target_mean)^2)), numeric(1L))
  target_rmse_sd   <- vapply(rounded, function(r)
    sqrt(mean((r$sd   - target_sd)^2)),   numeric(1L))

  between_raw <- list(rmse_mean = between_mean, rmse_sd = between_sd)
  target_raw  <- list(rmse_mean = target_rmse_mean, rmse_sd = target_rmse_sd)

  list(
    between_rmse = .summarise_rmse(between_raw),
    target_rmse  = .summarise_rmse(target_raw),
    raw          = list(between = between_raw, target = target_raw)
  )
}


# ---- mlr module ----------------------------------------------------------

#' @noRd
.rmse_parallel_mlr <- function(result) {

  stats_list <- lapply(result$results, get_stats)
  inp        <- result$results[[1L]]$inputs
  target_cor <- inp$target_cor
  target_reg <- inp$target_reg
  target_se  <- inp$target_se
  cor_dec    <- max(count_decimals(target_cor))
  reg_dec    <- max(count_decimals(target_reg))
  se_dec     <- if (!is.null(target_se)) max(count_decimals(target_se)) else NULL

  rounded <- lapply(stats_list, function(s) {
    list(
      cor = round(s$cor, cor_dec),
      reg = round(s$reg, reg_dec),
      se  = if (!is.null(target_se)) round(s$se, se_dec) else NULL
    )
  })

  n_runs      <- length(rounded)
  overall_cor <- Reduce("+", lapply(rounded, `[[`, "cor")) / n_runs
  overall_reg <- Reduce("+", lapply(rounded, `[[`, "reg")) / n_runs
  overall_se  <- if (!is.null(target_se)) {
    Reduce("+", lapply(rounded, `[[`, "se")) / n_runs
  } else {
    NULL
  }

  na_cor <- is.na(target_cor)
  na_reg <- is.na(target_reg)
  na_se  <- if (!is.null(target_se)) is.na(target_se) else NULL

  # between-run
  b_cor <- vapply(rounded, function(r) {
    d <- r$cor[!na_cor] - overall_cor[!na_cor]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  b_reg <- vapply(rounded, function(r) {
    d <- r$reg[!na_reg] - overall_reg[!na_reg]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  b_se <- if (!is.null(target_se)) {
    vapply(rounded, function(r) {
      d <- r$se[!na_se] - overall_se[!na_se]
      sqrt(mean(d^2, na.rm = TRUE))
    }, numeric(1L))
  } else {
    rep(NA_real_, n_runs)
  }

  # target
  t_cor <- vapply(rounded, function(r) {
    d <- r$cor[!na_cor] - target_cor[!na_cor]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  t_reg <- vapply(rounded, function(r) {
    d <- r$reg[!na_reg] - target_reg[!na_reg]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  t_se <- if (!is.null(target_se)) {
    vapply(rounded, function(r) {
      d <- r$se[!na_se] - target_se[!na_se]
      sqrt(mean(d^2, na.rm = TRUE))
    }, numeric(1L))
  } else {
    rep(NA_real_, n_runs)
  }

  between_raw <- list(rmse_cor = b_cor, rmse_reg = b_reg, rmse_se = b_se)
  target_raw  <- list(rmse_cor = t_cor, rmse_reg = t_reg, rmse_se = t_se)

  list(
    between_rmse = .summarise_rmse(between_raw),
    target_rmse  = .summarise_rmse(target_raw),
    raw          = list(between = between_raw, target = target_raw)
  )
}


# ---- aov module ----------------------------------------------------------

#' @noRd
.rmse_parallel_aov <- function(result) {

  stats_list <- lapply(result$results, get_stats)
  inp        <- result$results[[1L]]$inputs
  target_F   <- inp$target_f_list$F
  F_dec      <- max(count_decimals(target_F))

  F_mat <- do.call(rbind, lapply(stats_list, function(s) round(s$F_value, F_dec)))
  overall_F <- colMeans(F_mat, na.rm = TRUE)

  # between-run
  between_F <- apply(F_mat, 1, function(row)
    sqrt(mean((row - overall_F)^2, na.rm = TRUE)))

  # target
  target_rmse_F <- apply(F_mat, 1, function(row)
    sqrt(mean((row - target_F)^2, na.rm = TRUE)))

  between_raw <- list(rmse_F = between_F)
  target_raw  <- list(rmse_F = target_rmse_F)

  list(
    between_rmse = .summarise_rmse(between_raw),
    target_rmse  = .summarise_rmse(target_raw),
    raw          = list(between = between_raw, target = target_raw)
  )
}


# ---- summary -------------------------------------------------------------

#' Summarize a stats2data parallel result
#'
#' @param object An object of class \code{stats2data_parallel}.
#' @param ... Additional arguments (unused).
#'
#' @return An object of class \code{summary.stats2data_parallel}, printed by
#'   \code{\link{print.summary.stats2data_parallel}}.
#'
#' @export
summary.stats2data_parallel <- function(object, ...) {

  agg_stats <- get_stats(object)
  rmse      <- get_rmse(object)

  errors <- vapply(object$results, function(r) {
    e <- r$best_error
    if (is.list(e)) {
      mean(vapply(e, function(v) {
        if (is.numeric(v) && length(v) == 1L && is.finite(v)) v else NA_real_
      }, numeric(1L)), na.rm = TRUE)
    } else {
      as.numeric(e)
    }
  }, numeric(1L))

  out <- list(
    module     = object$module,
    runs       = object$runs,
    errors     = errors,
    agg_stats  = agg_stats,
    rmse       = rmse
  )
  class(out) <- "summary.stats2data_parallel"
  out
}


#' @method print summary.stats2data_parallel
#' @export
print.summary.stats2data_parallel <- function(x, ...) {

  cat("stats2data Parallel Summary\n")
  cat("-----------------------------------------------\n")
  cat("Module:", x$module, " | Runs:", x$runs, "\n")
  cat("Best error: ", format(min(x$errors, na.rm = TRUE), digits = 6), "\n")
  cat("Mean error: ", format(mean(x$errors, na.rm = TRUE), digits = 6), "\n")
  cat("SD error:   ", format(stats::sd(x$errors, na.rm = TRUE), digits = 6), "\n")

  cat("\nTarget RMSE (across runs):\n")
  print(x$rmse$target_rmse, row.names = FALSE, digits = 4)

  cat("\nBetween-run RMSE:\n")
  print(x$rmse$between_rmse, row.names = FALSE, digits = 4)

  cat("\nAggregated Statistics:\n")
  for (nm in names(x$agg_stats)) {
    cat("  ", nm, ":\n", sep = "")
    print(x$agg_stats[[nm]], digits = 4)
    cat("\n")
  }

  invisible(x)
}


# ---- plot ----------------------------------------------------------------

#' Plot RMSE distributions for a parallel result
#'
#' Visualises between-run variability and deviation-from-target RMSE
#' distributions as box-and-jitter plots, faceted by metric.
#'
#' @param x An object of class \code{stats2data_parallel}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- parallel_optim(FUN = optim_aov, args = list(...), runs = 20)
#' plot(res)
#' }
#'
#' @importFrom rlang .data
#' @export
plot.stats2data_parallel <- function(x, ...) {

  rmse_obj <- get_rmse(x)
  raw      <- rmse_obj$raw

  # ---- assemble long data frame ------------------------------------------
  metrics <- names(raw$between)

  rmse_data <- do.call(rbind, lapply(metrics, function(metric) {
    between_vals <- raw$between[[metric]]
    target_vals  <- raw$target[[metric]]

    # drop all-NA metrics (e.g. rmse_se when target_se is NULL)
    if (all(is.na(between_vals)) && all(is.na(target_vals))) return(NULL)

    rbind(
      data.frame(Metric = metric, Type = "Between Runs",
                 RMSE = between_vals, stringsAsFactors = FALSE),
      data.frame(Metric = metric, Type = "vs. Target",
                 RMSE = target_vals,  stringsAsFactors = FALSE)
    )
  }))

  rmse_data <- rmse_data[!is.na(rmse_data$RMSE), , drop = FALSE]
  if (nrow(rmse_data) == 0L) {
    stop("No RMSE data available to plot.", call. = FALSE)
  }

  # prettify metric labels
  rmse_data$Metric <- gsub("^rmse_", "", rmse_data$Metric)
  rmse_data$Metric <- toupper(rmse_data$Metric)
  rmse_data$Metric <- factor(rmse_data$Metric,
                             levels = unique(rmse_data$Metric))

  # ---- module title suffix -----------------------------------------------
  module_label <- switch(x$module,
                         vec = "Descriptives",
                         mlr = "MLR",
                         aov = "ANOVA",
                         x$module)

  # ---- build plot --------------------------------------------------------
  p <- ggplot2::ggplot(rmse_data, ggplot2::aes(
    x = .data$Type, y = .data$RMSE
  )) +
    ggplot2::stat_boxplot(
      geom = "errorbar", width = 0.25, color = "gray40"
    ) +
    ggplot2::geom_boxplot(
      width = 0.5, alpha = 0.15, fill = "gray80",
      outlier.shape = NA, color = "gray40"
    ) +
    ggplot2::geom_jitter(
      color = "steelblue", width = 0.12, height = 0,
      alpha = 0.7, size = 2
    ) +
    ggplot2::facet_wrap(
      ~ .data$Metric, scales = "free_y"
    ) +
    ggplot2::labs(
      title = paste0("RMSE Distribution - ", module_label, " Module"),
      x     = NULL,
      y     = "RMSE"
    ) +
    theme_stats2data() +
    ggplot2::theme(
      legend.position    = "none",
      strip.background   = ggplot2::element_rect(fill = "gray90", color = "gray50"),
      strip.text         = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(face = "bold")
    )

  print(p)
  invisible(p)
}
