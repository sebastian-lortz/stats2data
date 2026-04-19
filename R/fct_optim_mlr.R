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
#'   and SD when using SPRITE for integer data. Default \code{c(2, 2)}.
#' @param target_cor Numeric vector. Target upper-triangular (excluding
#'   diagonal) correlation values for predictor and outcome variables.
#' @param target_reg Numeric vector. Target regression coefficients including
#'   intercept, matching terms in \code{reg_equation}.
#' @param reg_equation Character. Regression model formula
#'   (e.g., \code{"Y ~ X1 + X2 + X1:X2"}).
#' @param target_se Numeric vector or NULL. Target standard errors for
#'   regression coefficients (same length as \code{target_reg}).
#'   Default \code{NULL}.
#' @param weight Numeric vector of length 2. Weights for correlation vs.
#'   regression error in the objective function. Default \code{c(1, 1)}.
#' @param tolerance Numeric. Convergence threshold. Default \code{1e-6}.
#' @param max_iter Integer. Iterations per restart. Default \code{1e5}.
#' @param init_temp Numeric. Initial SA temperature. Default \code{1}.
#' @param cooling_rate Numeric in (0,1) or \code{NULL} (auto).
#'   Default \code{NULL}.
#' @param max_starts Integer. Number of restarts. Default \code{1}.
#' @param hill_climbs Integer or NULL. Number of hill-climbing iterations for
#'   optional local refinement; if NULL, skips refinement. Default \code{NULL}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Default \code{"console"}.
#'
#' @return A \code{nds3.object} list with components:
#' \describe{
#'   \item{best_error}{Numeric. Minimum objective error achieved.}
#'   \item{data}{Data frame of optimized predictor and outcome values.}
#'   \item{optim_vec}{The \code{nds3.object} returned by the internal
#'     \code{optim_vec} call (marginal optimization results).}
#'   \item{inputs}{List of all input parameters for reproducibility.}
#'   \item{track_error}{Numeric vector of best error at each iteration.}
#'   \item{track_error_ratio}{Numeric vector of error ratios (cor vs. reg)
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
    weight       = c(1, 1),
    tolerance    = 1e-3,
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
  if (!is.numeric(target_cor) || !any(!is.na(target_cor)))
    stop("`target_cor` must contain at least one non-NA numeric value.")
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
  if (!is.numeric(weight) || length(weight) != 2)
    stop("`weight` must be a numeric vector of length 2 (correlation vs. regression error weights).")

  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0)
    stop("`tolerance` must be a single non-negative number.")
  if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0)
    stop("`max_iter` must be a single positive number.")
  if (!is.null(init_temp) && (!is.numeric(init_temp) || length(init_temp) != 1 || init_temp <= 0))
    stop("`init_temp` must be a single positive number or NULL.")
  if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
         cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate)))
    stop("`cooling_rate` must be in (0,1) or NULL.")
  if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1)
    stop("`max_starts` must be a single positive integer.")
  if (!(is.null(hill_climbs) ||
        (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
         hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))))
    stop("`hill_climbs` must be NULL or a single non-negative integer.")
  if (!is.character(progress_mode) || length(progress_mode) != 1 ||
      !progress_mode %in% c("console", "shiny", "off"))
    stop('`progress_mode` must be "console", "shiny", or "off".')

  # marginals optimization via optim_vec
  vec_result <- optim_vec(
    N             = N,
    target_mean   = target_mean,
    target_sd     = target_sd,
    range         = range,
    integer       = integer,
    sprite_prec   = sprite_prec,
    tolerance     = tolerance,
    max_iter      = max_iter,
    init_temp     = 1e-3,
    cooling_rate  = cooling_rate,
    max_starts    = max_starts,
    progress_mode = progress_mode
  )
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
  best_error        <- current_error
  best_ratio        <- initial$error_ratio
  track_error       <- numeric(max_iter * max_starts)
  track_error_ratio <- numeric(max_iter * max_starts)
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
    init_temp <- -median(deltas) / log(0.5)
  }
  temp <- init_temp

  # optimization loop
  progressr::with_progress({
    p <- progressr::progressor(steps = ceiling(total_iter / pb_interval))
    for (s in seq_len(max_starts)) {
      # fresh marginals on restarts
      if (s > 1) {
        int_expanded <- rep(integer, length.out = length(target_mean))
        rng_mat <- if (is.matrix(range)) range else
          matrix(rep(range, length(target_mean)), nrow = 2)
        for (v in seq_len(num_preds)) {
          vname <- pred_names[v]
          vidx  <- match(vname, names(target_mean))
          for (reinit in 1:5) {
            res_v <- optim_vec_single(
              N = N, t_mean = target_mean[vidx], t_sd = target_sd[vidx],
              rng = rng_mat[, vidx], is_int = int_expanded[vidx],
              sprite_prec = sprite_prec, tolerance = tolerance,
              max_iter = max_iter, init_temp = init_temp,
              cooling_rate = cooling_rate, max_starts = 1
            )
            if (length(unique(res_v$data)) >= 2) break
          }
          if (length(unique(res_v$data)) < 2) next
          predictors[, v] <- res_v$data
        }
        current_candidate <- predictors
      }
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
          if (current_error < best_error) {
            best_candidate <- current_candidate
            best_error     <- current_error
            best_ratio     <- err$error_ratio
          }
        }

        temp        <- temp * cooling_rate
        global_iter <- global_iter + 1L
        track_error[global_iter]       <- best_error
        track_error_ratio[global_iter] <- best_ratio
        if (global_iter %% pb_interval == 0) p()
        if (is.finite(best_error) && best_error < tolerance) break
      }
      current_candidate <- best_candidate
      if (is.finite(best_error) && best_error < tolerance) break
      temp <- init_temp
    }

    track_error       <- track_error[seq_len(global_iter)]
    track_error_ratio <- track_error_ratio[seq_len(global_iter)]

    # hill climbing refinement
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
      best_error     <- local_opt$best_error
      best_candidate <- local_opt$best_candidate
    }
  }, handlers = handler)

  # assemble output
  best_solution <- cbind(best_candidate, outcome)
  colnames(best_solution) <- col_names

  # return
  new_s2d_mlr(
    best_error        = best_error,
    data              = as.data.frame(best_solution),
    optim_vec         = vec_result,
    inputs            = list(
      N = N, target_mean = target_mean, target_sd = target_sd,
      range = range, integer = integer, sprite_prec = sprite_prec,
      target_cor = target_cor, target_reg = target_reg,
      reg_equation = reg_equation, target_se = target_se,
      weight = weight, tolerance = tolerance, max_iter = max_iter,
      init_temp = init_temp, cooling_rate = cooling_rate,
      max_starts = max_starts, hill_climbs = hill_climbs,
      progress_mode = progress_mode
    ),
    track_error       = track_error,
    track_error_ratio = track_error_ratio
  )
}
