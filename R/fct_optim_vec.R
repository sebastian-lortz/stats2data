#' Optimize a vector or matrix to match target means and SDs
#'
#' Uses the nds3 algorithmic framework to simulate one or multiple
#' vectors so that each matches specified target means and standard deviations
#' under given input parameters.
#'
#' @param N Integer. Number of values in each vector.
#' @param target_mean Named numeric vector. Desired means for each variable.
#' @param target_sd Named numeric vector. Desired standard deviations for each variable.
#' @param range Numeric vector of length 2 or numeric matrix. Allowed value
#'   range for all variables (vector), or per-variable bounds as a two-row
#'   matrix matching \code{target_mean}.
#' @param integer Logical or logical vector. If TRUE, optimize integer values;
#'   length 1 or same length as \code{target_mean}.
#' @param tolerance Numeric. Convergence threshold. Default \code{1e-3}.
#' @param sprite_prec Integer vector of length 2. Decimal precision for mean
#'   and SD when using SPRITE for integer data. Default \code{c(2, 2)}.
#' @param max_iter Integer. Iterations per restart. Default \code{1e4}.
#' @param init_temp Numeric. Initial SA temperature. Default \code{1}.
#' @param cooling_rate Numeric in (0,1) or \code{NULL} (auto). Default \code{NULL}.
#' @param max_starts Integer. Number of restarts. Default \code{1}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Default \code{"console"}.
#'
#' @return A \code{nds3.object} list with components \code{best_error},
#'   \code{data}, \code{inputs}, and \code{track_error}.
#'
#' @examples
#' \dontrun{
#' res <- optim_vec(
#'   N           = 100,
#'   target_mean = c(x = 10),
#'   target_sd   = c(x = 2),
#'   range       = c(0, 20),
#'   integer     = TRUE,
#'   sprite_prec = c(0, 0),
#'   max_iter    = 50000,
#'   max_starts  = 2
#' )
#' }
#' @export
optim_vec <- function(N,
                      target_mean,
                      target_sd,
                      range,
                      integer,
                      tolerance = 1e-2,
                      sprite_prec = c(2, 2),
                      max_iter = 1e5,
                      init_temp = 1e-3,
                      cooling_rate = NULL,
                      max_starts = 3,
                      progress_mode = "console") {

  # input checks
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N))
    stop("`N` must be a single positive integer.")
  if (!is.numeric(target_mean) || !is.numeric(target_sd) ||
      length(target_mean) != length(target_sd) || length(target_mean) < 1)
    stop("`target_mean` and `target_sd` must be numeric vectors of the same positive length.")
  if (is.null(names(target_mean)) || any(names(target_mean) == ""))
    stop("`target_mean` must have non-empty names.")
  if (!(is.numeric(range) &&
        (length(range) == 2 ||
         (is.matrix(range) && ncol(range) == length(target_mean)))))
    stop("`range` must be a numeric vector of length 2 or a matrix with columns matching length of `target_mean`.")
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0)
    stop("`tolerance` must be a single non-negative number.")
  if (!is.logical(integer) || length(integer) < 1 ||
      (length(integer) != 1 && length(integer) != length(target_mean)))
    stop("`integer` must be a logical vector (length 1 or length(target_mean)).")
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0)
    stop("`max_iter` must be a single positive number.")
  if (!is.null(init_temp) && (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0))
    stop("`init_temp` must be a single positive number or NULL.")
  if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
         cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate)))
    stop("`cooling_rate` must be in (0,1) or NULL.")
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1)
    stop("`max_starts` must be a single positive integer.")
  if (any(integer) && is.null(sprite_prec))
    stop("`sprite_prec` must be specified when `integer = TRUE`. ",
         "Provide an integer vector of length 2: c(mean_decimals, sd_decimals).")
  if (!is.null(sprite_prec)) {
    if (!is.numeric(sprite_prec) || length(sprite_prec) != 2 ||
        any(sprite_prec < 0) || any(sprite_prec != as.integer(sprite_prec)))
      stop("`sprite_prec` must be an integer vector of length 2 (mean precision, SD precision).")
  }
  if (!is.character(progress_mode) || length(progress_mode) != 1 ||
      !progress_mode %in% c("console", "shiny", "off"))
    stop('`progress_mode` must be "console", "shiny", or "off".')

    if (is.null(cooling_rate)) cooling_rate <- (max_iter - 10) / max_iter
    if (is.null(init_temp)) init_temp <- 1e-3
    n_var <- length(target_mean)
    if (length(integer) < n_var) integer <- rep(integer[1], n_var)
    if (!is.matrix(range)) range <- matrix(rep(range, n_var), nrow = 2, ncol = n_var)

    solution_matrix <- matrix(NA, nrow = N, ncol = n_var)
    best_error_vec  <- vector("list", n_var)
    track_error_all <- vector("list", n_var)

    handler <- switch(progress_mode,
                      console = list(progressr::handler_txtprogressbar()),
                      shiny   = list(progressr::handler_shiny()),
                      off     = list(progressr::handler_void())
    )

    progressr::with_progress({
      p <- progressr::progressor(steps = n_var)
      for (v in seq_len(n_var)) {
        res <- optim_vec_single(
          N = N, t_mean = target_mean[v], t_sd = target_sd[v],
          rng = range[, v], is_int = integer[v],
          sprite_prec = sprite_prec, tolerance = tolerance,
          max_iter = max_iter, init_temp = init_temp,
          cooling_rate = cooling_rate, max_starts = max_starts
        )
        solution_matrix[, v] <- res$data
        best_error_vec[[v]]  <- res$best_error
        track_error_all[[v]] <- res$track_error
        p()
      }
    }, handlers = handler)

    colnames(solution_matrix) <- names(target_mean)
    solution_matrix <- as.data.frame(solution_matrix)

    new_s2d_vec(
      best_error  = best_error_vec,
      data        = solution_matrix,
      inputs      = list(N = N, target_mean = target_mean, target_sd = target_sd,
                         range = range, integer = integer, tolerance = tolerance,
                         sprite_prec = sprite_prec, max_iter = max_iter,
                         init_temp = init_temp, cooling_rate = cooling_rate,
                         max_starts = max_starts, progress_mode = progress_mode),
      track_error = track_error_all
    )

  }
