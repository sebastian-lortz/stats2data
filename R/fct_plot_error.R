# --------------------------------------------------------------------------
# stats2data: plot_error S3 methods
# --------------------------------------------------------------------------

#' Reduce a piecewise-constant trace to its change points
#'
#' \code{track_error} stores the best-so-far error at every iteration, so it
#' is a step function that is constant over long stretches. Keeping only the
#' first index of each run of equal values, plus the final index, reproduces
#' the curve exactly when drawn with \code{geom_step(direction = "hv")}.
#' Not exported.
#'
#' @param y Numeric vector.
#'
#' @return An integer vector of indices into \code{y}.
#'
#' @noRd
.step_points <- function(y) {
  n <- length(y)
  if (n <= 1L) return(seq_len(n))

  same <- y[-1L] == y[-n]
  same[is.na(same)] <- FALSE

  sort(unique(c(1L, which(!same) + 1L, n)))
}


#' Build an error-trajectory ggplot from a numeric vector
#'
#' Shared plotting logic used by all \code{plot_error.*} methods.
#' Not exported.
#'
#' @param err_vec Numeric vector of error values (one per iteration).
#' @param show_best Logical; if \code{TRUE}, mark the minimum error with a
#'   point.
#' @param first_iter Integer; number of leading iterations to omit from the
#'   plot (zero-based).
#' @param title Character; plot title.
#' @param y_lab Character; y-axis label.
#' @param log_y Logical; if \code{TRUE}, draw the y axis on a base-10
#'   logarithmic scale. Non-positive errors cannot be shown on that scale and
#'   are dropped with a warning.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @noRd
.plot_error_engine <- function(err_vec,
                               show_best  = TRUE,
                               first_iter = 1L,
                               title      = "Error Reduction of Objective Function",
                               y_lab      = "Error",
                               log_y      = FALSE) {

  # --- guards -------------------------------------------------------------
  n <- length(err_vec)
  if (n == 0L) {
    stop("Error vector is empty; nothing to plot.", call. = FALSE)
  }
  if (!is.numeric(first_iter) || length(first_iter) != 1L ||
      first_iter != as.integer(first_iter) || first_iter < 0L) {
    stop("`first_iter` must be a single non-negative integer.", call. = FALSE)
  }
  if (first_iter >= n) {
    stop(
      sprintf(
        "`first_iter` (%d) must be smaller than the length of the error vector (%d).",
        first_iter, n
      ),
      call. = FALSE
    )
  }
  if (!is.logical(log_y) || length(log_y) != 1L || is.na(log_y)) {
    stop("`log_y` must be a single logical value.", call. = FALSE)
  }

  # --- data ---------------------------------------------------------------
  seg_err <- err_vec[(first_iter + 1L):n]
  seg_it  <- first_iter + seq_along(seg_err)

  keep <- .step_points(seg_err)
  df <- data.frame(
    Iteration = seg_it[keep],
    Error     = seg_err[keep]
  )

  drop <- !is.finite(df$Error) | (log_y & df$Error <= 0)
  if (any(drop)) {
    warning(
      sprintf("%d error value%s could not be plotted and %s dropped.",
              sum(drop), if (sum(drop) == 1L) "" else "s",
              if (sum(drop) == 1L) "was" else "were"),
      call. = FALSE
    )
    df <- df[!drop, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    stop("No plottable error values remain.", call. = FALSE)
  }

  best_idx   <- which.min(err_vec)[1L]
  best_error <- err_vec[best_idx]

  # --- plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$Iteration, y = .data$Error
  )) +
    ggplot2::geom_step(color = "steelblue", linewidth = 1, direction = "hv") +
    ggplot2::labs(title = title, x = "Iteration", y = y_lab) +
    theme_stats2data() +
    ggplot2::coord_cartesian(clip = "off")

  if (nrow(df) == 1L) {
    p <- p + ggplot2::geom_point(color = "steelblue", size = 2)
  }

  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  } else {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  }

  if (show_best && is.finite(best_error) && (!log_y || best_error > 0)) {
    best_df <- data.frame(Iteration = best_idx, Error = best_error)
    p <- p +
      ggplot2::geom_point(
        data    = best_df,
        mapping = ggplot2::aes(
          x = .data$Iteration, y = .data$Error, color = "Lowest error"
        ),
        size        = 3,
        show.legend = TRUE
      )
  }

  p <- p +
    ggplot2::scale_color_manual(
      name   = "",
      values = c("Lowest error" = "darkgreen"),
      breaks = "Lowest error",
      labels = paste(
        "Lowest error =",
        formatC(best_error, format = "e", digits = 3)
      )
    ) +
    ggplot2::theme(legend.position = "bottom")

  p
}


#' Build an error-ratio trajectory ggplot from a numeric vector
#'
#' Shared plotting logic for the correlation-to-regression error ratio.
#' Only meaningful for MLR results. Not exported.
#'
#' @param ratio_vec Numeric vector of error ratio values (one per iteration).
#' @param first_iter Integer; number of leading iterations to omit from the
#'   plot (zero-based).
#' @param show_mean Logical; draw a horizontal line at the mean ratio.
#' @param show_median Logical; draw a horizontal line at the median ratio.
#' @param show_final Logical; draw a horizontal line at the final ratio.
#' @param title Character; plot title.
#' @param log_y Logical; draw the y axis on a base-10 logarithmic scale.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @noRd
.plot_error_ratio_engine <- function(ratio_vec,
                                     first_iter  = 1L,
                                     show_mean   = TRUE,
                                     show_median = TRUE,
                                     show_final  = TRUE,
                                     title       = "Error Ratio Cor/Reg",
                                     log_y       = FALSE) {

  # --- guards -------------------------------------------------------------
  n <- length(ratio_vec)
  if (n == 0L) {
    stop("Error ratio vector is empty; nothing to plot.", call. = FALSE)
  }
  if (!is.numeric(first_iter) || length(first_iter) != 1L ||
      first_iter != as.integer(first_iter) || first_iter < 0L) {
    stop("`first_iter` must be a single non-negative integer.", call. = FALSE)
  }
  if (first_iter >= n) {
    stop(
      sprintf(
        "`first_iter` (%d) must be smaller than the length of the ratio vector (%d).",
        first_iter, n
      ),
      call. = FALSE
    )
  }
  if (!is.logical(log_y) || length(log_y) != 1L || is.na(log_y)) {
    stop("`log_y` must be a single logical value.", call. = FALSE)
  }

  # --- data ---------------------------------------------------------------
  seg    <- ratio_vec[(first_iter + 1L):n]
  seg_it <- first_iter + seq_along(seg)

  keep <- .step_points(seg)
  df <- data.frame(
    Iteration  = seg_it[keep],
    ErrorRatio = seg[keep]
  )

  drop <- !is.finite(df$ErrorRatio) | (log_y & df$ErrorRatio <= 0)
  if (any(drop)) {
    warning(
      sprintf("%d ratio value%s could not be plotted and %s dropped.",
              sum(drop), if (sum(drop) == 1L) "" else "s",
              if (sum(drop) == 1L) "was" else "were"),
      call. = FALSE
    )
    df <- df[!drop, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    stop("No plottable ratio values remain.", call. = FALSE)
  }

  mean_val   <- round(mean(ratio_vec), 1)
  median_val <- round(stats::median(ratio_vec), 1)
  final_val  <- round(ratio_vec[n], 1)

  # --- plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$Iteration, y = .data$ErrorRatio
  )) +
    ggplot2::geom_step(color = "steelblue", linewidth = 1, direction = "hv") +
    ggplot2::labs(title = title, x = "Iteration", y = "Error Ratio Cor/Reg") +
    theme_stats2data() +
    ggplot2::coord_cartesian(clip = "off")

  if (nrow(df) == 1L) {
    p <- p + ggplot2::geom_point(color = "steelblue", size = 2)
  }
  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }

  # reference lines
  ref_stats  <- character(0)
  ref_vals   <- numeric(0)
  ref_ltypes <- character(0)

  if (show_mean) {
    ref_stats  <- c(ref_stats,  paste0("Mean: ", mean_val))
    ref_vals   <- c(ref_vals,   mean_val)
    ref_ltypes <- c(ref_ltypes, "dashed")
  }
  if (show_median) {
    ref_stats  <- c(ref_stats,  paste0("Median: ", median_val))
    ref_vals   <- c(ref_vals,   median_val)
    ref_ltypes <- c(ref_ltypes, "dotted")
  }
  if (show_final) {
    ref_stats  <- c(ref_stats,  paste0("Final: ", final_val))
    ref_vals   <- c(ref_vals,   final_val)
    ref_ltypes <- c(ref_ltypes, "solid")
  }

  if (length(ref_stats) > 0L) {
    df_ref <- data.frame(
      stat     = ref_stats,
      y        = ref_vals,
      linetype = ref_ltypes,
      stringsAsFactors = FALSE
    )
    p <- p +
      ggplot2::geom_hline(
        data    = df_ref,
        mapping = ggplot2::aes(
          yintercept = .data$y,
          color      = .data$stat,
          linetype   = .data$stat
        ),
        linewidth = 0.8
      ) +
      ggplot2::scale_color_manual(
        name   = "",
        values = stats::setNames(rep("gray40", nrow(df_ref)), df_ref$stat),
        breaks = df_ref$stat
      ) +
      ggplot2::scale_linetype_manual(
        name   = "",
        values = stats::setNames(df_ref$linetype, df_ref$stat),
        breaks = df_ref$stat
      ) +
      ggplot2::theme(legend.position = "bottom")
  }

  p
}


#' Extract a single error vector from track_error (vector or list)
#'
#' @param track_error Numeric vector or list of numeric vectors.
#' @param run Integer; which run to extract.
#'
#' @return A numeric vector.
#'
#' @noRd
.extract_error_vec <- function(track_error, run = 1L) {

  if (!is.numeric(run) || length(run) != 1L ||
      run != as.integer(run) || run < 1L) {
    stop("`run` must be a single positive integer.", call. = FALSE)
  }

  if (is.list(track_error)) {
    n_runs <- length(track_error)
    if (run > n_runs) {
      stop(
        sprintf(
          "`run` = %d is out of bounds. There %s only %d run%s available.",
          run,
          if (n_runs == 1L) "is" else "are",
          n_runs,
          if (n_runs == 1L) "" else "s"
        ),
        call. = FALSE
      )
    }
    return(track_error[[run]])
  }

  # Scalar vector (single run)
  if (run != 1L) {
    stop("Only 1 run is available (track_error is a single vector).",
         call. = FALSE)
  }
  track_error
}


# ---- S3 methods ----------------------------------------------------------

# -- stats2data_aov --------------------------------------------------------

#' @describeIn plot_error Method for ANOVA results (\code{stats2data_aov}).
#'
#' @param x An object of class \code{stats2data_aov} produced by
#'   \code{\link{optim_aov}}.
#' @param run Integer. Index of the run to plot when \code{track_error}
#'   contains multiple runs; default \code{1}.
#' @param show_best Logical. If \code{TRUE} (default), marks the iteration
#'   with the smallest error.
#' @param first_iter Integer. Number of initial iterations to skip before
#'   plotting (zero-based); default \code{1}.
#' @param log_y Logical. If \code{TRUE}, the y axis uses a base-10
#'   logarithmic scale; non-positive errors are dropped with a warning.
#'   Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_aov(...)
#' plot_error(res)
#' plot_error(res, log_y = TRUE)
#' }
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_step geom_hline geom_point labs
#'   scale_color_manual scale_y_log10 coord_cartesian theme
#' @export
plot_error.stats2data_aov <- function(x, run = 1L, show_best = TRUE,
                                      first_iter = 1L, log_y = FALSE, ...) {

  if (is.null(x$track_error)) {
    stop("No `track_error` element found in this stats2data_aov object.",
         call. = FALSE)
  }
  err_vec <- .extract_error_vec(x$track_error, run = run)

  p <- .plot_error_engine(
    err_vec    = err_vec,
    show_best  = show_best,
    first_iter = first_iter,
    title      = "Error Reduction \u2014 ANOVA Module",
    log_y      = log_y
  )
  print(p)
  invisible(p)
}


# -- stats2data_mlr ---------------------------------------------------------

#' @describeIn plot_error Method for MLR results (\code{stats2data_mlr}).
#'
#' @param x An object of class \code{stats2data_mlr} produced by
#'   \code{\link{optim_mlr}}.
#' @param run Integer. Index of the run to plot; default \code{1}.
#' @param show_best Logical. If \code{TRUE} (default), marks the minimum
#'   error.
#' @param first_iter Integer. Iterations to skip; default \code{1}.
#' @param ratio Logical. If \code{TRUE}, plot the correlation-to-regression
#'   error ratio instead of the total error trajectory. Default \code{FALSE}.
#' @param show_mean Logical. When \code{ratio = TRUE}, draw a horizontal line
#'   at the mean ratio. Default \code{TRUE}. Ignored when \code{ratio = FALSE}.
#' @param show_median Logical. When \code{ratio = TRUE}, draw a horizontal
#'   line at the median ratio. Default \code{TRUE}. Ignored when
#'   \code{ratio = FALSE}.
#' @param show_final Logical. When \code{ratio = TRUE}, draw a horizontal
#'   line at the final ratio. Default \code{TRUE}. Ignored when
#'   \code{ratio = FALSE}.
#' @param log_y Logical. If \code{TRUE}, the y axis uses a base-10
#'   logarithmic scale; non-positive values are dropped with a warning.
#'   Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_mlr(...)
#' plot_error(res)
#' plot_error(res, log_y = TRUE)
#' plot_error(res, ratio = TRUE)
#' plot_error(res, ratio = TRUE, show_mean = FALSE)
#' }
#'
#' @export
plot_error.stats2data_mlr <- function(x, run = 1L, show_best = TRUE,
                                      first_iter = 1L,
                                      ratio = FALSE,
                                      show_mean = TRUE,
                                      show_median = TRUE,
                                      show_final = TRUE,
                                      log_y = FALSE,
                                      ...) {

  if (!is.logical(ratio) || length(ratio) != 1L) {
    stop("`ratio` must be a single logical value.", call. = FALSE)
  }

  if (ratio) {
    # --- error ratio plot ---
    if (is.null(x$track_error_ratio)) {
      stop(
        "No `track_error_ratio` element found in this stats2data_mlr object.",
        call. = FALSE
      )
    }
    if (!is.logical(show_mean) || length(show_mean) != 1L) {
      stop("`show_mean` must be a single logical value.", call. = FALSE)
    }
    if (!is.logical(show_median) || length(show_median) != 1L) {
      stop("`show_median` must be a single logical value.", call. = FALSE)
    }
    if (!is.logical(show_final) || length(show_final) != 1L) {
      stop("`show_final` must be a single logical value.", call. = FALSE)
    }

    ratio_vec <- .extract_error_vec(x$track_error_ratio, run = run)

    p <- .plot_error_ratio_engine(
      ratio_vec   = ratio_vec,
      first_iter  = first_iter,
      show_mean   = show_mean,
      show_median = show_median,
      show_final  = show_final,
      title       = "Error Ratio Cor/Reg \u2014 MLR Module",
      log_y       = log_y
    )
    print(p)
    return(invisible(p))
  }

  if (is.null(x[["track_error"]])) {          # was x$track_error
    stop("No `track_error` element found in this stats2data_mlr object.",
         call. = FALSE)
  }
  err_vec <- .extract_error_vec(x[["track_error"]], run = run)

  p <- .plot_error_engine(
    err_vec    = err_vec,
    show_best  = show_best,
    first_iter = first_iter,
    title      = "Error Reduction \u2014 MLR Module",
    log_y      = log_y
  )
  print(p)
  invisible(p)
}


# -- stats2data_vec ---------------------------------------------------------

#' @describeIn plot_error Method for Descriptives results
#' (\code{stats2data_vec}).
#'
#' @param x An object of class \code{stats2data_vec} produced by
#'   \code{\link{optim_vec}}.
#' @param run Integer. Index of the run (variable) to plot; default \code{1}.
#' @param show_best Logical. If \code{TRUE} (default), marks the minimum
#'   error.
#' @param first_iter Integer. Iterations to skip; default \code{1}.
#' @param log_y Logical. If \code{TRUE}, the y axis uses a base-10
#'   logarithmic scale; non-positive errors are dropped with a warning.
#'   Default \code{FALSE}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(...)
#' plot_error(res)
#' plot_error(res, log_y = TRUE)
#' }
#'
#' @export
plot_error.stats2data_vec <- function(x, run = 1L, show_best = TRUE,
                                      first_iter = 1L, log_y = FALSE, ...) {

  if (is.null(x$track_error)) {
    stop(
      "No `track_error` element found in this stats2data_vec object. ",
      "PSO routines for continuous data do not track per-iteration error.",
      call. = FALSE
    )
  }
  err_vec <- .extract_error_vec(x$track_error, run = run)

  # Build a more informative title when variable names are available
  var_name <- NULL
  if (is.list(x$track_error) && !is.null(names(x$track_error))) {
    var_name <- names(x$track_error)[run]
  } else if (!is.null(colnames(x$data)) && run <= ncol(x$data)) {
    var_name <- colnames(x$data)[run]
  }
  title <- if (!is.null(var_name)) {
    paste0("Error Reduction \u2014 Descriptives (", var_name, ")")
  } else {
    "Error Reduction \u2014 Descriptives Module"
  }

  p <- .plot_error_engine(
    err_vec    = err_vec,
    show_best  = show_best,
    first_iter = first_iter,
    title      = title,
    log_y      = log_y
  )
  print(p)
  invisible(p)
}
