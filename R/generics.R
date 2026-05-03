# --------------------------------------------------------------------------
# stats2data: S3 generic definitions
# --------------------------------------------------------------------------

#' Extract statistics from a `stats2data` result
#'
#' Computes and returns key analytical outputs from a `stats2data` result
#' object, including means, standard deviations, correlations, regression
#' coefficients, or F-statistics depending on the module that produced the
#' result.
#'
#' @param result A stats2data result object (\code{stats2data_vec},
#'   \code{stats2data_mlr}, or \code{stats2data_aov}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list of statistics. Contents depend on the class of
#'   \code{result}:
#'   \describe{
#'     \item{For \code{stats2data_vec}:}{Elements \code{mean} and \code{sd}.}
#'     \item{For \code{stats2data_mlr}:}{Elements \code{model}, \code{reg},
#'       \code{se}, \code{cor}, \code{mean}, and \code{sd}.}
#'     \item{For \code{stats2data_aov}:}{Elements \code{model}, \code{F_value},
#'       and \code{mean}.}
#'   }
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
#'   range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
#'   max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' get_stats(res)
#' }
#'
#' @export
get_stats <- function(result, ...) UseMethod("get_stats")


#' Compute RMSE for a stats2data result
#'
#' Calculates root-mean-square error metrics comparing the achieved summary
#' statistics of the simulated data against the target inputs.
#'
#' @param result A stats2data result object (\code{stats2data_vec},
#'   \code{stats2data_mlr}, or \code{stats2data_aov}).
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list of RMSE values. Contents depend on the class of
#'   \code{result}:
#'   \describe{
#'     \item{For \code{stats2data_vec}:}{Elements \code{rmse_mean} and
#'       \code{rmse_sd}.}
#'     \item{For \code{stats2data_mlr}:}{Elements \code{rmse_cor},
#'       \code{rmse_reg}, and \code{rmse_se}.}
#'     \item{For \code{stats2data_aov}:}{Elements \code{rmse_F} and
#'       \code{rmse_mean}.}
#'   }
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
#'   range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
#'   max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' get_rmse(res)
#' }
#'
#' @export
get_rmse <- function(result, ...) UseMethod("get_rmse")


#' Plot error trajectory for a stats2data result
#'
#' Visualizes how the objective-function error evolves across iterations of
#' the simulated-annealing optimiser.  Dispatches to class-specific methods
#' for \code{stats2data_aov}, \code{stats2data_mlr}, and \code{stats2data_vec}.
#'
#' For \code{stats2data_mlr} objects, set \code{ratio = TRUE} to plot the
#' correlation-to-regression error ratio instead of the total error.
#'
#' @param x A stats2data result object (\code{stats2data_aov},
#'   \code{stats2data_mlr}, or \code{stats2data_vec}).
#' @param ... Arguments passed to methods (see individual method pages).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{plot_error.stats2data_aov}},
#'   \code{\link{plot_error.stats2data_mlr}},
#'   \code{\link{plot_error.stats2data_vec}},
#'   \code{\link{plot_cooling}}
#'
#' @examples
#' \dontrun{
#' res <- optim_mlr(...)
#' plot_error(res)
#' plot_error(res, ratio = TRUE)
#' plot_error(res, run = 2, first_iter = 500)
#' }
#'
#' @export
plot_error <- function(x, ...) UseMethod("plot_error")


#' Plot cooling schedule for a stats2data result
#'
#' Visualizes the simulated-annealing temperature decay across iterations.
#' Dispatches to class-specific methods for \code{stats2data_aov},
#' \code{stats2data_mlr}, and \code{stats2data_vec}.
#'
#' @param x A stats2data result object (\code{stats2data_aov},
#'   \code{stats2data_mlr}, or \code{stats2data_vec}).
#' @param ... Arguments passed to methods.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{plot_error}}
#'
#' @examples
#' \dontrun{
#' res <- optim_aov(...)
#' plot_cooling(res)
#' }
#'
#' @export
plot_cooling <- function(x, ...) UseMethod("plot_cooling")


#' Plot summary of target-vs-simulated statistics
#'
#' Creates a lollipop chart comparing simulated and target summary statistics.
#' Dispatches to class-specific methods for \code{stats2data_aov},
#' \code{stats2data_mlr}, and \code{stats2data_vec}.
#'
#' @param x A stats2data result object (\code{stats2data_aov},
#'   \code{stats2data_mlr}, or \code{stats2data_vec}).
#' @param ... Arguments passed to methods (typically \code{standardised} and
#'   \code{eps}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{plot_error}}, \code{\link{plot_cooling}}
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(...)
#' plot_summary(res)
#' plot_summary(res, standardised = FALSE)
#' }
#'
#' @export
plot_summary <- function(x, ...) UseMethod("plot_summary")
