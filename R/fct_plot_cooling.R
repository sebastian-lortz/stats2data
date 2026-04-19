# --------------------------------------------------------------------------
# stats2data: plot_cooling S3 generic and methods
# --------------------------------------------------------------------------


#' Build a cooling-schedule ggplot from SA parameters
#'
#' Shared plotting logic used by all \code{plot_cooling.*} methods.
#' Not exported.
#'
#' @param max_iter Integer; total iterations.
#' @param init_temp Numeric; initial temperature.
#' @param cooling_rate Numeric; multiplicative decay per iteration.
#' @param title Character; plot title.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @noRd
.plot_cooling_engine <- function(max_iter, init_temp, cooling_rate,
                                 title = "Cooling Schedule") {

  # --- guards -------------------------------------------------------------
  if (!is.numeric(max_iter) || length(max_iter) != 1L || max_iter < 1L) {
    stop("`max_iter` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(init_temp) || length(init_temp) != 1L || init_temp <= 0) {
    stop("`init_temp` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(cooling_rate) || length(cooling_rate) != 1L ||
      cooling_rate <= 0 || cooling_rate >= 1) {
    stop("`cooling_rate` must be a single number in (0, 1).", call. = FALSE)
  }

  # --- data ---------------------------------------------------------------
  iterations   <- seq_len(max_iter)
  temperatures <- init_temp * cooling_rate^iterations

  df <- data.frame(
    Iteration   = iterations,
    Temperature = temperatures
  )

  # --- plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$Iteration, y = .data$Temperature
  )) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = title, x = "Iteration", y = "Temperature") +
    theme_stats2data() +
    ggplot2::coord_cartesian(clip = "off")

  p
}


# ---- S3 methods ----------------------------------------------------------

#' @describeIn plot_cooling Method for ANOVA results (\code{stats2data_aov}).
#'
#' @param x An object of class \code{stats2data_aov}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @export
plot_cooling.stats2data_aov <- function(x, ...) {
  inp <- x$inputs
  .validate_cooling_inputs(inp)

  p <- .plot_cooling_engine(
    max_iter     = inp$max_iter,
    init_temp    = inp$init_temp,
    cooling_rate = inp$cooling_rate,
    title        = "Cooling Schedule \u2014 ANOVA Module"
  )
  print(p)
  invisible(p)
}


#' @describeIn plot_cooling Method for MLR results (\code{stats2data_mlr}).
#'
#' @param x An object of class \code{stats2data_mlr}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @export
plot_cooling.stats2data_mlr <- function(x, ...) {
  inp <- x$inputs
  .validate_cooling_inputs(inp)

  p <- .plot_cooling_engine(
    max_iter     = inp$max_iter,
    init_temp    = inp$init_temp,
    cooling_rate = inp$cooling_rate,
    title        = "Cooling Schedule \u2014 MLR Module"
  )
  print(p)
  invisible(p)
}


#' @describeIn plot_cooling Method for Descriptives results
#'   (\code{stats2data_vec}).
#'
#' @param x An object of class \code{stats2data_vec}.
#' @param ... Currently unused.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @export
plot_cooling.stats2data_vec <- function(x, ...) {
  inp <- x$inputs
  .validate_cooling_inputs(inp)

  p <- .plot_cooling_engine(
    max_iter     = inp$max_iter,
    init_temp    = inp$init_temp,
    cooling_rate = inp$cooling_rate,
    title        = "Cooling Schedule \u2014 Descriptives Module"
  )
  print(p)
  invisible(p)
}


# ---- Internal validation helper -----------------------------------------

#' Validate that inputs contain SA schedule parameters
#'
#' @param inputs List; the \code{$inputs} element of a stats2data result.
#'
#' @return NULL (invisibly); stops with an informative error if validation
#'   fails.
#'
#' @noRd
.validate_cooling_inputs <- function(inputs) {
  missing <- character(0)
  if (is.null(inputs$max_iter))     missing <- c(missing, "max_iter")
  if (is.null(inputs$init_temp))    missing <- c(missing, "init_temp")
  if (is.null(inputs$cooling_rate)) missing <- c(missing, "cooling_rate")
  if (length(missing) > 0L) {
    stop(
      "Missing required SA parameters in `inputs`: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}
