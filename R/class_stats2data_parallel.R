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

  combined <- lapply(stats_list, function(s) {
    list(mean = s$mean,
         sd   = s$sd)
  })

  n_runs       <- length(combined)
  overall_mean <- Reduce("+", lapply(combined, `[[`, "mean")) / n_runs
  overall_sd   <- Reduce("+", lapply(combined, `[[`, "sd"))   / n_runs

  # between-run
  between_mean <- vapply(combined, function(r)
    sqrt(mean((r$mean - overall_mean)^2)), numeric(1L))
  between_sd   <- vapply(combined, function(r)
    sqrt(mean((r$sd   - overall_sd)^2)),   numeric(1L))

  # target
  target_rmse_mean <- vapply(combined, function(r)
    sqrt(mean((r$mean - target_mean)^2)), numeric(1L))
  target_rmse_sd   <- vapply(combined, function(r)
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

  combined <- lapply(stats_list, function(s) {
    list(
      cor = s$cor,
      reg = s$reg,
      se  = if (!is.null(target_se)) s$se else NULL
    )
  })

  n_runs      <- length(combined)
  overall_cor <- Reduce("+", lapply(combined, `[[`, "cor")) / n_runs
  overall_reg <- Reduce("+", lapply(combined, `[[`, "reg")) / n_runs
  overall_se  <- if (!is.null(target_se)) {
    Reduce("+", lapply(combined, `[[`, "se")) / n_runs
  } else {
    NULL
  }

  na_cor <- is.na(target_cor)
  na_reg <- is.na(target_reg)
  na_se  <- if (!is.null(target_se)) is.na(target_se) else NULL

  # between-run
  b_cor <- vapply(combined, function(r) {
    d <- r$cor[!na_cor] - overall_cor[!na_cor]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  b_reg <- vapply(combined, function(r) {
    d <- r$reg[!na_reg] - overall_reg[!na_reg]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  b_se <- if (!is.null(target_se)) {
    vapply(combined, function(r) {
      d <- r$se[!na_se] - overall_se[!na_se]
      sqrt(mean(d^2, na.rm = TRUE))
    }, numeric(1L))
  } else {
    rep(NA_real_, n_runs)
  }

  # target
  t_cor <- vapply(combined, function(r) {
    d <- r$cor[!na_cor] - target_cor[!na_cor]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  t_reg <- vapply(combined, function(r) {
    d <- r$reg[!na_reg] - target_reg[!na_reg]
    sqrt(mean(d^2, na.rm = TRUE))
  }, numeric(1L))

  t_se <- if (!is.null(target_se)) {
    vapply(combined, function(r) {
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
  target_F   <- inp$target_f_list$F_value

  F_mat <- do.call(rbind, lapply(stats_list, function(s) s$F_value))
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
  print.data.frame(x$rmse$target_rmse, row.names = FALSE, digits = 4)

  cat("\nBetween-run RMSE:\n")
  print.data.frame(x$rmse$between_rmse, row.names = FALSE, digits = 4)

  cat("\nAggregated Statistics:\n")
  for (nm in names(x$agg_stats)) {
    cat("  ", nm, ":\n", sep = "")
    print.data.frame(x$agg_stats[[nm]], digits = 4)
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


# ---- plot_summary --------------------------------------------------------

#' @describeIn plot_summary Method for parallel results
#'   (\code{stats2data_parallel}). Aggregates each simulated parameter as the
#'   across-run mean (rounded to the target's reported precision) and overlays
#'   an error bar showing the across-run min/max
#'   (\code{error_type = "range"}, default) or \code{mean \u00b1 SD}
#'   (\code{error_type = "sd"}). Module-specific data assembly is dispatched
#'   internally based on \code{x$module}.
#'
#' @param x An object of class \code{stats2data_parallel}.
#' @param standardised Logical; if \code{TRUE} (default), differences are
#'   divided by target values, with a fall-back to the unstandardised
#'   difference for targets whose absolute value is below \code{eps}.
#' @param eps Numeric; threshold below which a target is treated as zero
#'   for standardisation. Default \code{1e-12}.
#' @param error_type Character; \code{"range"} (default) draws across-run
#'   min/max error bars, \code{"sd"} draws \code{mean \u00b1 SD} across runs.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- parallel_optim(FUN = optim_mlr, args = list(...), runs = 20)
#' plot_summary(res)
#' plot_summary(res, standardised = FALSE, error_type = "sd")
#' }
#'
#' @importFrom rlang .data
#' @export
plot_summary.stats2data_parallel <- function(x,
                                             standardised = TRUE,
                                             eps          = 1e-12,
                                             error_type   = c("range", "sd"),
                                             ...) {
  if (!is.logical(standardised) || length(standardised) != 1L) {
    stop("`standardised` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(eps) || length(eps) != 1L) {
    stop("`eps` must be a single numeric value.", call. = FALSE)
  }
  error_type <- match.arg(error_type)

  module <- x$module
  built  <- switch(
    module,
    vec = .plot_summary_parallel_vec(x, standardised, eps, error_type),
    mlr = .plot_summary_parallel_mlr(x, standardised, eps, error_type),
    aov = .plot_summary_parallel_aov(x, standardised, eps, error_type),
    stop("Unrecognised module: ", module, call. = FALSE)
  )

  module_label <- switch(module,
                         vec = "Descriptives Module (parallel)",
                         mlr = "MLR Module (parallel)",
                         aov = "ANOVA Module (parallel)")

  p <- .plot_summary_engine(
    built$df, standardised,
    x_lab        = built$x_lab,
    title_module = module_label
  )

  err_label <- switch(error_type,
                      range = "across-run min/max",
                      sd    = "mean \u00b1 SD across runs")

  # ---- per-run cloud, on the same Centered scale as the lollipop --------
  jitter_df <- .parallel_jitter_df(x, built$df, standardised, eps)

  p <- p +
    ggplot2::geom_jitter(
      data        = jitter_df,
      mapping     = ggplot2::aes(x = .data$Variable, y = .data$Centered),
      inherit.aes = FALSE,
      color       = "steelblue",
      width       = 0.12,
      height      = 0,
      alpha       = 0.6,
      size        = 1.6,
      na.rm       = TRUE
    )

  print(p)
  invisible(p)
}

# ---- Internal: parallel plot_summary helpers ----------------------------

#' Aggregate a per-parameter matrix (rows = parameters, cols = runs) into
#' the across-run point estimate plus low/high bounds on the simulated scale.
#'
#' @noRd
.parallel_sim_block <- function(mat, error_type) {
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  sim   <- rowMeans(mat, na.rm = TRUE)

  if (error_type == "range") {
    lo <- apply(mat, 1, min, na.rm = TRUE)
    hi <- apply(mat, 1, max, na.rm = TRUE)
  } else {
    s  <- apply(mat, 1, stats::sd, na.rm = TRUE)
    lo <- sim - s
    hi <- sim + s
  }

  # rowMeans / min / max return Inf or NaN for all-NA rows; coerce to NA
  sim[!is.finite(sim)] <- NA_real_
  lo[!is.finite(lo)]   <- NA_real_
  hi[!is.finite(hi)]   <- NA_real_

  list(sim = sim, lo = lo, hi = hi)
}

#' Long per-run frame on the lollipop's Centered scale.
#'
#' Returns columns Measure, Variable, Centered with one row per
#' (parameter x run). The Variable factor reuses the levels from
#' `agg_df$Variable` so the jitter columns align with the lollipop
#' dots.
#'
#' @noRd
.parallel_jitter_df <- function(x, agg_df, standardised, eps) {

  module     <- x$module
  inp        <- x$results[[1L]]$inputs
  stats_list <- lapply(x$results, get_stats)

  # ---- collect (matrix, target, measure label, var names) per measure ---
  blocks <- list()

  add_block <- function(mat, target, measure, var_names) {
    if (is.null(mat) || is.null(target)) return(NULL)
    blocks[[length(blocks) + 1L]] <<- list(
      mat = mat, target = target, measure = measure, vars = var_names
    )
  }

  if (module == "vec") {
    vars <- names(inp$target_mean)
    if (is.null(vars)) vars <- colnames(x$results[[1L]]$data)
    add_block(do.call(cbind, lapply(stats_list, `[[`, "mean")),
              inp$target_mean, "Mean", vars)
    add_block(do.call(cbind, lapply(stats_list, `[[`, "sd")),
              inp$target_sd,   "SD",   vars)
  } else if (module == "mlr") {
    add_block(do.call(cbind, lapply(stats_list, `[[`, "reg")),
              inp$target_reg, "Regression Coefficient",
              names(inp$target_reg))
    var_cor <- names(inp$target_cor)
    if (is.null(var_cor)) var_cor <- paste0("Cor", seq_along(inp$target_cor))
    add_block(do.call(cbind, lapply(stats_list, `[[`, "cor")),
              inp$target_cor, "Correlation", var_cor)
    if (!is.null(inp$target_se)) {
      var_se <- if (!is.null(names(inp$target_se))) names(inp$target_se) else
        names(inp$target_reg)[seq_along(inp$target_se)]
      add_block(do.call(cbind, lapply(stats_list, `[[`, "se")),
                inp$target_se, "Standard Error", var_se)
    }
  } else if (module == "aov") {
    eff <- inp$target_f_list$effect
    if (is.null(eff)) eff <- paste0("Effect", seq_along(inp$target_f_list$F_value))
    add_block(do.call(cbind, lapply(stats_list, `[[`, "F_value")),
              inp$target_f_list$F_value, "F", eff)
  }

  # ---- to long frame, centered the same way as the lollipop -------------
  out <- do.call(rbind, lapply(blocks, function(b) {
    centered <- apply(b$mat, 2, .compute_centered,
                      target       = b$target,
                      standardised = standardised,
                      eps          = eps)
    data.frame(
      Measure  = b$measure,
      Variable = rep(b$vars, ncol(b$mat)),
      Centered = as.numeric(centered),
      stringsAsFactors = FALSE
    )
  }))

  # reuse the aggregated frame's factor levels so x-positions align
  out$Variable <- factor(out$Variable, levels = levels(agg_df$Variable))
  out
}


#' Convert simulated-scale low/high values to centered-scale Lower/Upper,
#' guarding against the sign-flip that occurs when standardising by a
#' negative target.
#'
#' @noRd
.centered_bounds <- function(sim_lo, sim_hi, target, standardised, eps) {
  c_lo <- .compute_centered(sim_lo, target, standardised, eps)
  c_hi <- .compute_centered(sim_hi, target, standardised, eps)
  list(Lower = pmin(c_lo, c_hi), Upper = pmax(c_lo, c_hi))
}


#' @noRd
.plot_summary_parallel_vec <- function(x, standardised, eps, error_type) {

  inp         <- x$results[[1L]]$inputs
  target_mean <- inp$target_mean
  target_sd   <- inp$target_sd
  vars        <- names(target_mean)
  if (is.null(vars)) vars <- colnames(x$results[[1L]]$data)

  stats_list <- lapply(x$results, get_stats)
  mat_mean   <- do.call(cbind, lapply(stats_list, `[[`, "mean"))
  mat_sd     <- do.call(cbind, lapply(stats_list, `[[`, "sd"))

  mb <- .parallel_sim_block(mat_mean, error_type)
  sb <- .parallel_sim_block(mat_sd, error_type)

  cm <- .compute_centered(mb$sim, target_mean, standardised, eps)
  cs <- .compute_centered(sb$sim, target_sd,   standardised, eps)
  bm <- .centered_bounds(mb$lo, mb$hi, target_mean, standardised, eps)
  bs <- .centered_bounds(sb$lo, sb$hi, target_sd,   standardised, eps)

  df <- data.frame(
    Variable  = rep(vars, 2L),
    Measure   = rep(c("Mean", "SD"), each = length(vars)),
    Simulated = c(as.numeric(mb$sim), as.numeric(sb$sim)),
    Target    = c(as.numeric(target_mean), as.numeric(target_sd)),
    Centered  = c(cm, cs),
    Lower     = c(bm$Lower, bs$Lower),
    Upper     = c(bm$Upper, bs$Upper),
    stringsAsFactors = FALSE
  )
  df <- .tag_sim_type(df, standardised, eps)

  df <- df %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::mutate(
      Variable = factor(.data$Variable, levels = unique(.data$Variable))
    ) %>%
    dplyr::ungroup()

  list(df = df, x_lab = "Variable")
}


#' @noRd
.plot_summary_parallel_mlr <- function(x, standardised, eps, error_type) {

  inp        <- x$results[[1L]]$inputs
  target_reg <- inp$target_reg
  target_cor <- inp$target_cor
  target_se  <- inp$target_se

  stats_list <- lapply(x$results, get_stats)
  mat_reg    <- do.call(cbind, lapply(stats_list, `[[`, "reg"))
  mat_cor    <- do.call(cbind, lapply(stats_list, `[[`, "cor"))

  rb <- .parallel_sim_block(mat_reg, error_type)
  cb <- .parallel_sim_block(mat_cor, error_type)

  # regression coefficients ------------------------------------------------
  cr  <- .compute_centered(rb$sim, target_reg, standardised, eps)
  brb <- .centered_bounds(rb$lo, rb$hi, target_reg, standardised, eps)
  df_reg <- data.frame(
    Measure   = "Regression Coefficient",
    Variable  = names(target_reg),
    Simulated = as.numeric(rb$sim),
    Target    = as.numeric(target_reg),
    Centered  = cr,
    Lower     = brb$Lower,
    Upper     = brb$Upper,
    stringsAsFactors = FALSE
  )
  df_reg <- .tag_sim_type(df_reg, standardised, eps)

  # correlations -----------------------------------------------------------
  var_names_cor <- names(target_cor)
  if (is.null(var_names_cor)) {
    var_names_cor <- paste0("Cor", seq_along(target_cor))
  }
  cc  <- .compute_centered(cb$sim, target_cor, standardised, eps)
  bcb <- .centered_bounds(cb$lo, cb$hi, target_cor, standardised, eps)
  df_cor <- data.frame(
    Measure   = "Correlation",
    Variable  = var_names_cor,
    Simulated = as.numeric(cb$sim),
    Target    = as.numeric(target_cor),
    Centered  = cc,
    Lower     = bcb$Lower,
    Upper     = bcb$Upper,
    stringsAsFactors = FALSE
  )
  df_cor <- .tag_sim_type(df_cor, standardised, eps)

  # standard errors (optional) --------------------------------------------
  df_se <- NULL
  if (!is.null(target_se)) {
    mat_se <- do.call(cbind, lapply(stats_list, `[[`, "se"))
    sb     <- .parallel_sim_block(mat_se, error_type)
    cs     <- .compute_centered(sb$sim, target_se, standardised, eps)
    bsb    <- .centered_bounds(sb$lo, sb$hi, target_se, standardised, eps)
    var_names_se <- if (!is.null(names(target_se))) {
      names(target_se)
    } else {
      names(target_reg)[seq_along(target_se)]
    }
    df_se <- data.frame(
      Measure   = "Standard Error",
      Variable  = var_names_se,
      Simulated = as.numeric(sb$sim),
      Target    = as.numeric(target_se),
      Centered  = cs,
      Lower     = bsb$Lower,
      Upper     = bsb$Upper,
      stringsAsFactors = FALSE
    )
    df_se <- .tag_sim_type(df_se, standardised, eps)
  }

  df_all <- dplyr::bind_rows(df_reg, df_cor, df_se) %>%
    dplyr::group_by(.data$Measure) %>%
    dplyr::mutate(
      Variable = factor(.data$Variable, levels = unique(.data$Variable))
    ) %>%
    dplyr::ungroup()

  list(df = df_all, x_lab = "Parameter")
}


#' @noRd
.plot_summary_parallel_aov <- function(x, standardised, eps, error_type) {

  inp      <- x$results[[1L]]$inputs
  target_F <- inp$target_f_list$F_value

  stats_list <- lapply(x$results, get_stats)
  mat_F      <- do.call(cbind, lapply(stats_list, `[[`, "F_value"))

  fb  <- .parallel_sim_block(mat_F, error_type)
  cf  <- .compute_centered(fb$sim, target_F, standardised, eps)
  bfb <- .centered_bounds(fb$lo, fb$hi, target_F, standardised, eps)

  effect_names <- inp$target_f_list$effect
  if (is.null(effect_names)) {
    effect_names <- paste0("Effect", seq_along(target_F))
  }

  df <- data.frame(
    Measure   = "F",
    Variable  = effect_names,
    Simulated = as.numeric(fb$sim),
    Target    = as.numeric(target_F),
    Centered  = cf,
    Lower     = bfb$Lower,
    Upper     = bfb$Upper,
    stringsAsFactors = FALSE
  )
  df <- .tag_sim_type(df, standardised, eps)
  df$Variable <- factor(df$Variable, levels = effect_names)

  list(df = df, x_lab = "Effect")
}

