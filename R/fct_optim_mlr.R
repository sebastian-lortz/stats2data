#' Optimize simulated data to match target correlations and regression estimates
#'
#' Simulates data such that the
#' resulting correlations and regression coefficients match specified targets
#' under a given regression model. Internally calls \code{\link{optim_vec}} to
#' first generate marginals matching target means and standard deviations,
#' then optimizes predictor orderings via simulated annealing (and
#' hill climbing) to match correlation and regression targets.
#'
#' @param N Integer. Total number of observations.
#' @param target_mean Named numeric vector. Desired means for each variable
#'   (names must match variables in \code{reg_equation}).
#' @param target_sd Named numeric vector. Desired standard deviations for each
#'   variable (same length and names as \code{target_mean}).
#' @param range Numeric vector of length 2 or numeric matrix. Allowed value
#'   range for all variables (vector), or per-variable bounds as a two-row
#'   matrix with columns matching \code{target_mean}.
#' @param integer Logical or logical vector. If TRUE, generate integer-valued
#'   data; length 1 or same length as \code{target_mean}.
#' @param sprite_prec Integer vector of length 2. Decimal precision for mean
#'   and SD when using SPRITE for integer data; must be at least the number of
#'   decimals of the means and SDs of the integer variables, because SPRITE
#'   does not round the targets. Default \code{c(2, 2)}.
#' @param target_cor Numeric vector. Target upper-triangular (excluding
#'   diagonal) correlation values of all variables in \code{target_mean},
#'   ordered as \code{upper.tri()} of the correlation matrix with rows and
#'   columns in the order of \code{target_mean}; \code{NA} for unreported
#'   correlations.
#' @param target_reg Numeric vector. Target regression coefficients including
#'   intercept, matching terms in \code{reg_equation}.
#' @param reg_equation Character. Regression model formula
#'   (e.g., \code{"Y ~ X1 + X2 + X1:X2"}).
#' @param target_se Numeric vector or NULL. Target standard errors for
#'   regression coefficients (same length as \code{target_reg}).
#'   Default \code{NULL}.
#' @param weight Numeric in [0, 1]. Weight of the correlation error in the
#'   objective function; the regression error is weighted by
#'   \code{1 - weight}. Default \code{0.5}.
#' @param thresh Numeric. Convergence threshold. Default \code{5e-3}.
#' @param max_iter Integer. Iterations per restart. Default \code{1e5}.
#' @param init_temp Numeric or \code{NULL}. Initial SA temperature; if
#'   \code{NULL}, it is set such that the median objective change of 500
#'   random swaps is accepted with probability 0.5. Default \code{NULL}.
#' @param cooling_rate Numeric in (0,1) or \code{NULL} (auto).
#'   Default \code{NULL}.
#' @param max_starts Integer. Number of restarts. Default \code{3}.
#' @param hill_climbs Integer or NULL. Number of hill-climbing iterations for
#'   optional local refinement; if NULL or 0, skips refinement.
#'   Default \code{1e4}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Default \code{"console"}.
#'
#' @details The marginals are generated first by \code{\link{optim_vec}};
#'   its warnings are passed on with the prefix "Marginal step" (e.g. a
#'   continuous variable that did not converge, or an integer variable for which
#'   SPRITE found only an approximate distribution). If no data could be
#'   generated for a variable, \code{optim_mlr()} stops and names the variable
#'   and the reason.
#'
#' @return An object of class \code{stats2data_mlr}, a list with components:
#' \describe{
#'   \item{best_objective}{Numeric. Minimum objective error achieved.}
#'   \item{data}{Data frame of optimised predictor and outcome values.}
#'   \item{optim_vec}{The \code{stats2data_vec} object returned by the internal
#'     \code{optim_vec} call (marginal optimization results).}
#'   \item{inputs}{List of the input parameters as used internally:
#'     \code{weight} as \code{c(weight, 1 - weight)}, \code{target_cor}
#'     reordered to (predictors, outcome), and the set
#'     \code{init_temp} and \code{cooling_rate}.}
#'   \item{track_objective}{Numeric vector of best objective value at each iteration.}
#'   \item{track_objective_ratio}{Numeric vector of error ratios (cor vs. reg)
#'     per iteration.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- optim_mlr(
#'   N            = 100,
#'   target_mean  = c(X1 = 5, X2 = 3, Y = 10),
#'   target_sd    = c(X1 = 1, X2 = 2, Y = 3),
#'   range        = c(0, 20),
#'   integer      = FALSE,
#'   target_cor   = c(.23, .10, .45),
#'   target_reg   = c(2.1, 1.2, -0.8),
#'   reg_equation = "Y ~ X1 + X2",
#'   max_iter     = 10000,
#'   hill_climbs  = 50
#' )
#' }
#' @export
optim_mlr <- function(
    N,
    target_mean,
    target_sd,
    range,
    integer,
    target_cor,
    target_reg,
    reg_equation,
    sprite_prec  = c(2, 2),
    target_se    = NULL,
    weight       = .5,
    thresh    = 5e-3,
    max_iter     = 1e5,
    init_temp    = NULL,
    cooling_rate = NULL,
    max_starts   = 3,
    hill_climbs  = 1e4,
    progress_mode = "console"
) {

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
  if (!is.logical(integer) || length(integer) < 1 ||
      (length(integer) != 1 && length(integer) != length(target_mean)))
    stop("`integer` must be a logical vector (length 1 or length(target_mean)).")
  if (any(integer) && is.null(sprite_prec))
    stop("`sprite_prec` must be specified when `integer = TRUE`. ",
         "Provide an integer vector of length 2: c(mean_decimals, sd_decimals).")
  if (!is.null(sprite_prec)) {
    if (!is.numeric(sprite_prec) || length(sprite_prec) != 2 ||
        any(sprite_prec < 0) || any(sprite_prec != as.integer(sprite_prec)))
      stop("`sprite_prec` must be an integer vector of length 2 (mean precision, SD precision).")
  }
  check_sprite_prec(target_mean, target_sd, integer, range, sprite_prec)

  if (!is.character(reg_equation) || length(reg_equation) != 1)
    stop("`reg_equation` must be a single character string giving the regression formula.")
  frm        <- stats::as.formula(reg_equation)
  all_vars   <- all.vars(frm)
  dv_name    <- all_vars[1]
  pred_names <- all_vars[-1]
  missing_vars <- setdiff(all_vars, names(target_mean))
  if (length(missing_vars) > 0)
    stop("The following variables in `reg_equation` are not in `target_mean`: ",
         paste(missing_vars, collapse = ", "))
  n_cols  <- length(target_mean)
  exp_cor <- n_cols * (n_cols - 1) / 2
  term_lbls <- attr(stats::terms(frm), "term.labels")
  exp_reg   <- length(term_lbls) + 1
  if (!is.numeric(target_cor) || any(!is.na(target_cor) & (target_cor < -1 | target_cor > 1))) {
    stop("`target_cor` must contain only NA or numeric values between -1 and 1.")
  }
  if (length(target_cor) != exp_cor)
    stop(sprintf("`target_cor` must be a numeric vector of length %d, not %d.",
                 exp_cor, length(target_cor)))
  if (!is.numeric(target_reg) || !any(!is.na(target_reg)))
    stop("`target_reg` must contain at least one non-NA numeric value.")
  if (length(target_reg) != exp_reg)
    stop(sprintf("`target_reg` must be a numeric vector of length %d, not %d.",
                 exp_reg, length(target_reg)))
  if (!is.null(target_se)) {
    if (!is.numeric(target_se) || length(target_se) != length(target_reg))
      stop("`target_se`, if provided, must be a numeric vector the same length as `target_reg`.")
  }
  if (!is.numeric(weight) || length(weight) != 1)
    stop("`weight` must be numeric and of length 1 (correlation vs. regression error mixing weight).")
  if (weight < 0 || weight > 1)
    stop("`weight` must be between 0 and 1.")
  weight = c(weight, 1-weight)
  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0)
    stop("`thresh` must be a single non-negative number.")
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0)
    stop("`max_iter` must be a single positive number.")
  if (!is.null(init_temp) && (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0))
    stop("`init_temp` must be a single positive number or NULL.")
  if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
         cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate)))
    stop("`cooling_rate` must be in (0,1) or NULL.")
  if (!is.numeric(max_starts) || length(max_starts) != 1 ||
      !is.finite(max_starts) || max_starts < 1 || max_starts %% 1 != 0)
    stop("`max_starts` must be a single positive integer.")
  if (!(is.null(hill_climbs) ||
        (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
         hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))))
    stop("`hill_climbs` must be NULL or a single non-negative integer.")
  if (!is.character(progress_mode) || length(progress_mode) != 1 ||
      !progress_mode %in% c("console", "shiny", "off"))
    stop('`progress_mode` must be "console", "shiny", or "off".')

  # marginals via optim_vec; its warnings are marked as marginal-step warnings
  vec_result <- withCallingHandlers(optim_vec(
    N             = N,
    target_mean   = target_mean,
    target_sd     = target_sd,
    range         = range,
    integer       = integer,
    sprite_prec   = sprite_prec,
    thresh     = thresh,
    max_iter      = 5e5,
    init_temp     = 1e-3,
    cooling_rate  = NULL,
    max_starts    = 3,
    progress_mode = progress_mode
  ), warning = function(w) {
    # variables without data are reported by the stop() below
    if (!grepl("no data generated", conditionMessage(w), fixed = TRUE)) {
      warning("Marginal step (optim_vec): ", conditionMessage(w), call. = FALSE)
    }
    invokeRestart("muffleWarning")
  })
  failed <- vapply(vec_result$data, anyNA, logical(1))
  if (any(failed)) {
    stop("Marginal step (optim_vec): no data generated for ",
         paste0("'", names(target_mean)[failed], "' (", unlist(vec_result$error_msgs[failed]), ")",
                collapse = "; "),
         ". Check the mean, SD, range, and sprite_prec of these variables.", call. = FALSE)
  }
  sim_data <- vec_result$data

  # design structure
  col_names  <- c(pred_names, dv_name)
  predictors <- as.matrix(sim_data[, pred_names, drop = FALSE])
  outcome    <- sim_data[[dv_name]]
  num_preds  <- ncol(predictors)

  # derive term positions for Rcpp
  terms_obj  <- stats::terms(frm)
  design_cpp <- get_design(candidate = predictors, reg_equation, terms_obj)$positions
  names(target_reg) <- get_design(candidate = predictors, reg_equation, terms_obj)$target_names

  # map the target correlations
  target_cor <- remap_target_cor(target_cor, sim_data, col_names)

  # objective function
  if (is.null(target_se)) {
    error_function <- function(candidate) {
      error_function_cpp(
        candidate, outcome, target_cor, target_reg,
        weight, design_cpp
      )
    }
  } else {
    target_reg_se <- cbind(target_reg, target_se)
    error_function <- function(candidate) {
      error_function_cpp_se(
        candidate, outcome, target_cor, target_reg_se,
        weight, design_cpp
      )
    }
  }

  # SA setup
  if (is.null(cooling_rate)) cooling_rate <- (max_iter - 10) / max_iter

  current_candidate <- predictors
  best_candidate    <- current_candidate
  initial           <- error_function(current_candidate)
  current_error     <- initial$total_error
  if (!is.finite(current_error)) current_error <- Inf
  best_objective        <- current_error
  best_ratio        <- initial$error_ratio
  track_objective       <- numeric(max_iter * max_starts)
  track_objective_ratio <- numeric(max_iter * max_starts)
  global_iter       <- 0L

  # progress setup
  handler <- switch(progress_mode,
                    console = list(progressr::handler_txtprogressbar()),
                    shiny   = list(progressr::handler_shiny()),
                    off     = list(progressr::handler_void())
  )
  total_iter  <- max_iter * max_starts + if (!is.null(hill_climbs)) hill_climbs else 0L
  pb_interval <- max(floor(total_iter / 100), 1)

  # set init temp
  if (is.null(init_temp)) {
    deltas <- replicate(500, {
      cand <- current_candidate
      col_idx <- sample(num_preds, 1)
      idx <- sample(N, 2)
      cand[idx, col_idx] <- cand[rev(idx), col_idx]
      abs(error_function(cand)$total_error - current_error)
    })
    deltas <- deltas[is.finite(deltas) & deltas > 0]
    init_temp <- -stats::median(deltas) / log(0.5)
  }
  temp <- init_temp

  # optimization loop
  progressr::with_progress({
    p <- progressr::progressor(steps = ceiling(total_iter / pb_interval))
    for (s in seq_len(max_starts)) {
      current_error <- error_function(current_candidate)$total_error
      for (i in seq_len(max_iter)) {
        candidate <- current_candidate

        # candidate modification
        move_size <- sample(c(1, 2, 3), 1, prob = c(.9, 0.05, 0.05))
        if (move_size == 1) {
          col_idx <- sample(num_preds, 1)
          found <- FALSE
          for (try in 1:100) {
            idx <- sample(N, 2)
            if (candidate[idx[1], col_idx] != candidate[idx[2], col_idx]) {
              found <- TRUE
              break
            }
          }
          if (!found) next
          candidate[idx, col_idx] <- candidate[rev(idx), col_idx]
        } else if (move_size == 2) {
          # swap a block
          col_idx <- sample(num_preds, 1)
          k <- max(2, round(0.05 * N))
          idx <- sample(N, k)
          candidate[idx, col_idx] <- sample(candidate[idx, col_idx])
        } else {
          # swap 2 rows
          idx <- sample(N, 2)
          candidate[idx, ] <- candidate[rev(idx), ]
        }

        # candidate evaluation
        err <- error_function(candidate)
        if (!is.finite(err$total_error)) next
        prob <- exp((current_error - err$total_error) / temp)
        if (err$total_error < current_error || stats::runif(1) < prob) {
          current_candidate <- candidate
          current_error     <- err$total_error
          if (current_error < best_objective) {
            best_candidate <- current_candidate
            best_objective     <- current_error
            best_ratio     <- err$error_ratio
          }
        }

        temp        <- temp * cooling_rate
        global_iter <- global_iter + 1L
        track_objective[global_iter]       <- best_objective
        track_objective_ratio[global_iter] <- best_ratio
        if (global_iter %% pb_interval == 0) p()
        if (is.finite(best_objective) && best_objective < thresh) break
      }
      current_candidate <- best_candidate
      if (is.finite(best_objective) && best_objective < thresh) break
      temp <- init_temp
    }

    track_objective       <- track_objective[seq_len(global_iter)]
    track_objective_ratio <- track_objective_ratio[seq_len(global_iter)]

    # hill climbing
    if (!is.null(hill_climbs) && hill_climbs > 0) {
      local_opt <- hill_climb(
        current_candidate = best_candidate,
        error_function    = error_function,
        N                 = N,
        hill_climbs       = hill_climbs,
        num_preds         = num_preds,
        progressor        = p,
        pb_interval       = pb_interval,
        progress_mode     = progress_mode
      )
      best_objective     <- local_opt$best_objective
      best_candidate <- local_opt$best_candidate
    }
  }, handlers = handler)

  # assemble output
  best_solution <- cbind(best_candidate, outcome)
  colnames(best_solution) <- col_names

  # return
  new_s2d_mlr(
    best_objective        = best_objective,
    data              = as.data.frame(best_solution),
    optim_vec         = vec_result,
    inputs            = list(
      N = N, target_mean = target_mean, target_sd = target_sd,
      range = range, integer = integer, sprite_prec = sprite_prec,
      target_cor = target_cor, target_reg = target_reg,
      reg_equation = reg_equation, target_se = target_se,
      weight = weight, thresh = thresh, max_iter = max_iter,
      init_temp = init_temp, cooling_rate = cooling_rate,
      max_starts = max_starts, hill_climbs = hill_climbs,
      progress_mode = progress_mode
    ),
    track_objective       = track_objective,
    track_objective_ratio = track_objective_ratio
  )
}
