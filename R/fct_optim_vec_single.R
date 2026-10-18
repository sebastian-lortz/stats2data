#' @noRd
optim_vec_single <- function(N, t_mean, t_sd, rng, is_int,
                             sprite_prec, thresh,
                             max_iter, init_temp, cooling_rate,
                             max_starts) {
  # input checks
  if (rng[1] > rng[2])
    stop("`range` must satisfy range[1] <= range[2].")
  if (rng[1] == rng[2] && (t_mean != rng[1] || t_sd != 0))
    stop("Zero-width range requires `target_mean == range[1]` and `target_sd == 0`.")
  if (rng[1] == rng[2])
    return(list(data = rep(rng[1], N), best_objective = 0, track_objective = 0,
                error_msg = NULL))
  if (t_mean < rng[1] || t_mean > rng[2])
    stop("`target_mean` must lie within `range`.")

  # objective function
  objective <- function(x) {
    objective_cpp(x, target_sd = t_sd)
  }

  # integer, sprite
  if (is_int) {
    if (abs(rng[2]-rng[1]) == 1) {
      d1 = round((t_mean - rng[1]) * N)
      dummy = c(rep(rng[2], d1), rep(rng[1], N - d1))
      return(list(
        data        = dummy,
         best_objective  = 0,
        track_objective = 0,
        error_msg   = NULL
      ))
    }

    pars <- tryCatch(
      rsprite2::set_parameters(
        mean = t_mean, sd = t_sd, n_obs = N,
        min_val = rng[1], max_val = rng[2],
         m_prec = sprite_prec[1], sd_prec = sprite_prec[2],
        dont_test = TRUE
      ),
      error = function(e) {
        return(list(.error = TRUE, .message = conditionMessage(e)))
      }
    )
    if (is.list(pars) && isTRUE(pars$.error)) {
      return(list(
        data        = rep(NA_real_, N),
         best_objective  = Inf,
        track_objective = Inf,
        error_msg   = pars$.message
      ))
    }

    x_sprite <- tryCatch(
      rsprite2::find_possible_distribution(pars),
       error = function(e) {
        return(list(.error = TRUE, .message = conditionMessage(e)))
      }
    )
    if (is.list(x_sprite) && isTRUE(x_sprite$.error)) {
      return(list(
         data        = rep(NA_real_, N),
        best_objective  = Inf,
        track_objective = Inf,
        error_msg   = x_sprite$.message
      ))
    }

    return(list(
      data        = x_sprite$values,
      best_objective  = if (x_sprite$outcome == "success") objective(x_sprite$values) else Inf,
       track_objective = if (x_sprite$outcome == "success") 0 else Inf,
      error_msg   = if (x_sprite$outcome == "success") NULL else
        paste0("SPRITE could not find a valid distribution (outcome: ",
               x_sprite$outcome, ").")
    ))
  }

  # continuous, simulated annealing
  current_candidate <- sprite_start_vector_cont(
    tMean = t_mean, n = N, range = rng, thresh = thresh
  )
  current_error  <- objective(current_candidate)
  if (!is.finite(current_error)) current_error <- Inf
  best_candidate <- current_candidate
  best_objective     <- current_error
   track_objective    <- numeric(max_iter * max_starts)
  global_iter    <- 0L
  temp           <- init_temp

  for (s in seq_len(max_starts)) {
    for (i in seq_len(max_iter)) {
      candidate  <- heuristic_move_cont(current_candidate,
                                        target_sd = t_sd, range = rng)
      cand_error <- objective(candidate)
      if (!is.finite(cand_error)) next

      prob <- exp((current_error - cand_error) / temp)
      if (cand_error < current_error || stats::runif(1) < prob) {
        current_candidate <- candidate
        current_error     <- cand_error
         if (cand_error < best_objective) {
          best_objective     <- cand_error
          best_candidate <- candidate
        }
      }

      temp        <- temp * cooling_rate
      global_iter <- global_iter + 1L
      track_objective[global_iter] <- best_objective
      if (is.finite(best_objective) && best_objective < thresh) break
    }
    current_candidate <- best_candidate
    current_error <- best_objective
    if (is.finite(best_objective) && best_objective < thresh) break
    temp <- init_temp
  }

  track_objective <- track_objective[seq_len(global_iter)]

  list(
    data        = best_candidate,
    best_objective  = best_objective,
     track_objective = track_objective,
    error_msg   = NULL
  )
}
