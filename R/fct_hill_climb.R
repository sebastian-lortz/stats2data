#' Perform hill-climbing optimization
#'
#' Executes a hill-climbing algorithm to iteratively improve a candidate data set
#' by minimizing a supplied error function. Supports the LM module.
#'
#' @param current_candidate Matrix. The predictor matrix to be optimized.
#' @param error_function Function. Objective function that takes a candidate and
#'   returns a list with element \code{$total_error}.
#' @param N Integer. Number of observations (rows) in \code{current_candidate}.
#' @param hill_climbs Integer. Maximum number of iterations. Default \code{1e2}.
#' @param num_preds Integer. Number of predictor columns.
#' @param neighborhood_size Integer. Number of candidate moves evaluated per
#'   iteration. Default \code{4}.
#' @param progressor Function or NULL. A \code{progressr} progressor callback.
#'   Default \code{NULL}.
#' @param pb_interval Integer or NULL. Interval (in iterations) between
#'   progressor calls. Default \code{NULL}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Default \code{"console"}.
#'
#' @return A list with components:
#' \describe{
#'   \item{best_candidate}{The optimized candidate matrix achieving lowest error.}
#'   \item{best_error}{Numeric. The minimum objective function value found.}
#' }
#'
#' @examples
#' \dontrun{
#' hill_climb(
#'   current_candidate = matrix(rnorm(200), 100, 2),
#'   error_function = function(cand) list(total_error = sum(cand^2)),
#'   N = 100,
#'   hill_climbs = 100,
#'   num_preds = 2
#' )
#' }
#' @export
hill_climb <- function(current_candidate, error_function, N,
                       hill_climbs = 1e2,
                       num_preds = NULL,
                       neighborhood_size = 4,
                       progressor = NULL,
                       pb_interval = NULL,
                       progress_mode = "console") {
  # input checks
  if (missing(current_candidate)) {
    stop("`current_candidate` must be provided, the matrix to be optimized.")
  }
  if (!is.function(error_function)) {
    stop("`error_function` must be a function returning a list with element `$total_error`.")
  }
  if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N)) {
    stop("`N` must be a single positive integer indicating the number of observations.")
  }
  if (!(
    is.null(hill_climbs) ||
    (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
     hill_climbs > 0 && hill_climbs == as.integer(hill_climbs))
  )) {
    stop("`hill_climbs` must be NULL or a single positive integer.")
  }
  if (!is.numeric(num_preds) || length(num_preds) != 1 || num_preds < 1) {
    stop("`num_preds` must be a single positive integer.")
  }
  if (!is.numeric(neighborhood_size) || length(neighborhood_size) != 1 ||
      neighborhood_size < 1 || neighborhood_size != as.integer(neighborhood_size)) {
    stop("`neighborhood_size` must be a single positive integer specifying moves per iteration.")
  }
  if (!is.character(progress_mode) || length(progress_mode) != 1 ||
      !progress_mode %in% c("console", "shiny", "off")) {
    stop('`progress_mode` must be "console", "shiny", or "off".')
  }

  # initial error and best
  curr_err  <- error_function(current_candidate)$total_error
  best_cand <- current_candidate
  best_err  <- curr_err

  # progress interval
  if (is.null(pb_interval)) {
    pb_interval <- max(floor(hill_climbs / 100), 1)
  }

  # main loop
  for (i in seq_len(hill_climbs)) {
    loc_cand <- current_candidate
    loc_err  <- curr_err

    # generate neighborhood
    for (j in seq_len(neighborhood_size)) {
      cand <- current_candidate
      col  <- sample(seq_len(num_preds), 1)
      idx  <- sample(N, 2)
      cand[idx, col] <- cand[rev(idx), col]

      # evaluate
      cand_err <- error_function(cand)$total_error
      if (cand_err < loc_err) {
        loc_err  <- cand_err
        loc_cand <- cand
      }
    }

    # accept if improved
    if (loc_err < curr_err) {
      current_candidate <- loc_cand
      curr_err  <- loc_err
      best_cand <- loc_cand
      best_err  <- loc_err
    }

    # update progress
    if (!is.null(progressor) && (i %% pb_interval == 0)) {
      progressor()
    }

    if (is.finite(best_err) && best_err < .Machine$double.eps) break
  }

  if (progress_mode == "console") {
    cat("\nHill climbing best error:", best_err, "\n")
  }

  list(best_candidate = best_cand, best_error = best_err)
}
