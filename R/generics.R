# stats2data: S3 generic definitions

#' Extract statistics from a `stats2data` result
#'
#' Computes and returns outputs from a `stats2data` result
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
#' res <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
#'   range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
#'   max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' get_stats(res)
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
#' res <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1),
#'   range = c(0, 10), integer = FALSE, sprite_prec = c(2, 2),
#'   max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' get_rmse(res)
#'
#' @export
get_rmse <- function(result, ...) UseMethod("get_rmse")


#' Plot error trajectory for a stats2data result
#'
#' Visualises how the objective-function error evolves across iterations of
#' the simulated-annealing optimiser.
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
#' @seealso \code{\link{plot_convergence.stats2data_aov}},
#'   \code{\link{plot_convergence.stats2data_mlr}},
#'   \code{\link{plot_convergence.stats2data_vec}},
#'   \code{\link{plot_cooling}}
#'
#' @examples
#' set.seed(1)
#' res_vec <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1), range = c(0, 10),
#'   integer = FALSE, max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' plot_convergence(res_vec)
#'
#' args_mlr <- list(
#'   N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
#'   target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
#'   integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
#'   target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
#'   max_iter = 2000, max_starts = 1, hill_climbs = 100,
#'   progress_mode = "off"
#' )
#' res_mlr <- do.call(optim_mlr, args_mlr)
#' plot_convergence(res_mlr, log_y = TRUE)
#' plot_convergence(res_mlr, ratio = TRUE, show_mean = FALSE)
#'
#' @export
plot_convergence <- function(x, ...) UseMethod("plot_convergence")


#' Plot cooling schedule for a stats2data result
#'
#' Visualises the simulated-annealing temperature decay across iterations.
#'
#' @param x A stats2data result object (\code{stats2data_aov},
#'   \code{stats2data_mlr}, or \code{stats2data_vec}).
#' @param ... Arguments passed to methods.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{plot_convergence}}
#'
#' @examples
#' set.seed(1)
#' res_aov <- optim_aov(
#'   S = 20, levels = 2, target_group_means = c(4, 6),
#'   target_f_list = list(effect = "Factor1", F_value = 10),
#'   integer = FALSE, range = c(0, 10), formula = "outcome ~ Factor1",
#'   factor_type = "between", max_iter = 200, max_starts = 1,
#'   progress_mode = "off"
#' )
#' plot_cooling(res_aov)
#'
#' @export
plot_cooling <- function(x, ...) UseMethod("plot_cooling")


#' Plot summary of target-vs-simulated statistics
#'
#' Creates a chart comparing simulated and target summary statistics.
#'
#' @param x A stats2data result object (\code{stats2data_aov},
#'   \code{stats2data_mlr}, or \code{stats2data_vec}).
#' @param ... Arguments passed to methods (typically \code{standardised} and
#'   \code{eps}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{plot_convergence}}, \code{\link{plot_cooling}}
#'
#' @examples
#' set.seed(1)
#' res_vec <- optim_vec(
#'   N = 50, target_mean = c(x = 5), target_sd = c(x = 1), range = c(0, 10),
#'   integer = FALSE, max_iter = 1e4, max_starts = 1, progress_mode = "off"
#' )
#' plot_fit(res_vec)
#' plot_fit(res_vec, standardised = FALSE)
#'
#' args_mlr <- list(
#'   N = 30, target_mean = c(X1 = 3.93, X2 = 3.83, Y = 4.17),
#'   target_sd = c(X1 = 1.44, X2 = 1.70, Y = 1.34), range = c(1, 7),
#'   integer = TRUE, target_cor = c(0.22, 0.49, 0.42),
#'   target_reg = c(1.65, 0.39, 0.26), reg_equation = "Y ~ X1 + X2",
#'   max_iter = 2000, max_starts = 1, hill_climbs = 100,
#'   progress_mode = "off"
#' )
#' res_mlr <- do.call(optim_mlr, args_mlr)
#' plot_fit(res_mlr, order = "fit")
#'
#' res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
#'                           progress_mode = "off")
#' plot_fit(res_par, error_type = "sd")
#'
#' @export
plot_fit <- function(x, ...) UseMethod("plot_fit")

#' Plot run-to-run stability for a stats2data result
#'
#' Visualises two RMSE distributions across replications: run-to-run
#' variability, computed against the grand mean over runs, and accuracy,
#' computed against the requested targets. Box-and-jitter, faceted by
#' metric. Requires replications, so it is defined only for
#' \code{stats2data_parallel} objects produced by \code{parallel_optim}.
#'
#' @param x A \code{stats2data_parallel} object.
#' @param ... Arguments passed to methods.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, returned invisibly.
#'
#' @seealso \code{\link{get_rmse}}, \code{\link{plot_fit}},
#'   \code{\link{plot.stats2data}}
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
#' res_par <- parallel_optim(optim_mlr, args_mlr, runs = 3, seed = 2026,
#'                           progress_mode = "off")
#' plot_stability(res_par)
#'
#' @export
plot_stability <- function(x, ...) UseMethod("plot_stability")
