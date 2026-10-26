# stats2data: plot() methods for stats2data result objects

#' Default plots for stats2data results
#'
#' Convenience \code{\link[base]{plot}} methods so that \code{plot(res)} works
#' on any stats2data result object.
#'
#' The explicit functions remain the primary interface and expose the full
#' argument set:
#' \code{\link{plot_convergence}}, \code{\link{plot_cooling}},
#' \code{\link{plot_fit}}, and \code{\link{plot_stability}}.
#'
#' Available panels by class:
#' \describe{
#'   \item{\code{stats2data_vec}}{\code{"convergence"} (default),
#'     \code{"cooling"}, \code{"fit"}.}
#'   \item{\code{stats2data_aov}}{\code{"convergence"} (default),
#'     \code{"cooling"}, \code{"fit"}.}
#'   \item{\code{stats2data_mlr}}{\code{"convergence"} (default),
#'     \code{"ratio"}, \code{"cooling"}, \code{"fit"}.}
#'   \item{\code{stats2data_parallel}}{\code{"stability"} (default),
#'     \code{"fit"}.}
#' }
#'
#' @param x A stats2data result object.
#' @param which Character; which panel to draw.
#' @param ... Further arguments passed to the underlying plotting function.
#'
#' @return The plot is drawn as a side effect; the
#'   \code{\link[ggplot2]{ggplot}} object is returned invisibly.
#'
#' @seealso \code{\link{plot_convergence}}, \code{\link{plot_cooling}},
#'   \code{\link{plot_fit}}, \code{\link{plot_stability}}
#'
#' @examples
#' args_mlr <- list(
#'   N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
#'   target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
#'   integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
#'   target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
#'   max_iter = 2000, max_starts = 1, hill_climbs = 100,
#'   progress_mode = "off"
#' )
#' res_mlr <- do.call(optim_mlr, args_mlr)
#' plot(res_mlr)                                  # convergence
#' plot(res_mlr, which = "ratio", log_y = TRUE)
#' plot(res_mlr, which = "cooling")
#' plot(res_mlr, which = "fit", standardised = FALSE)
#'
#' res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
#'                           progress_mode = "off")
#' plot(res_par)                                  # stability
#' plot(res_par, which = "fit")
#'
#' @name plot.stats2data
NULL


#' @rdname plot.stats2data
#' @export
plot.stats2data_vec <- function(x,
                                which = c("convergence", "cooling", "fit"),
                                ...) {
  which <- match.arg(which)
  p <- switch(
    which,
    convergence = plot_convergence(x, ...),
     cooling     = plot_cooling(x, ...),
    fit         = plot_fit(x, ...)
  )
  invisible(p)
}


#' @rdname plot.stats2data
#' @export
plot.stats2data_aov <- function(x,
                                which = c("convergence", "cooling", "fit"),
                                ...) {
  which <- match.arg(which)
  p <- switch(
    which,
    convergence = plot_convergence(x, ...),
    cooling     = plot_cooling(x, ...),
    fit         = plot_fit(x, ...)
  )
  invisible(p)
}


#' @rdname plot.stats2data
#' @export
plot.stats2data_mlr <- function(x,
                                which = c("convergence", "ratio",
                                           "cooling", "fit"),
                                ...) {
  which <- match.arg(which)

  if ("ratio" %in% names(list(...))) {
    stop(
      "Use `which = \"ratio\"` rather than `ratio = TRUE` when calling ",
       "`plot()`. Pass `ratio` directly to `plot_convergence()` instead.",
      call. = FALSE
    )
  }

  p <- switch(
    which,
    convergence = plot_convergence(x, ratio = FALSE, ...),
    ratio       = plot_convergence(x, ratio = TRUE,  ...),
     cooling     = plot_cooling(x, ...),
    fit         = plot_fit(x, ...)
  )
  invisible(p)
}


#' @rdname plot.stats2data
#' @export
plot.stats2data_parallel <- function(x,
                                     which = c("stability", "fit"),
                                     ...) {
  which <- match.arg(which)
  p <- switch(
    which,
     stability = plot_stability(x, ...),
    fit       = plot_fit(x, ...)
  )
  invisible(p)
}
