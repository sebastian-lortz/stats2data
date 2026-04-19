#' @noRd
optim_vec_single <- function(N, t_mean, t_sd, rng, is_int,
                             sprite_prec, tolerance,
                             max_iter, init_temp, cooling_rate,
                             max_starts) {
  # input checks
  if (rng[1] > rng[2])
    stop("`range` must satisfy range[1] <= range[2].")
  if (rng[1] == rng[2] && (t_mean != rng[1] || t_sd != 0))
    stop("Zero-width range requires `target_mean == range[1]` and `target_sd == 0`.")
  if (rng[1] == rng[2])
    return(list(data = rep(rng[1], N), best_error = 0, track_error = 0))
  if (t_mean < rng[1] || t_mean > rng[2])
    stop("`target_mean` must lie within `range`.")

  # integer data: use rsprite2
  if (is_int) {
    pars <- rsprite2::set_parameters(
      mean = t_mean, sd = t_sd, n_obs = N,
      min_val = rng[1], max_val = rng[2],
      m_prec = sprite_prec[1], sd_prec = sprite_prec[2],
      dont_test = TRUE
    )
    x_sprite <- rsprite2::find_possible_distribution(pars)
    return(list(
      data        = x_sprite$values,
      best_error  = if (x_sprite$outcome == "success") 0 else Inf,
      track_error = if (x_sprite$outcome == "success") 0 else Inf
    ))
  }

  # continuous data: simulated annealing
  objective <- function(x) {
    objective_cpp(x, target_sd = t_sd)
  }

  current_candidate <- sprite_start_vector_cont(
    tMean = t_mean, n = N, range = rng, tolerance = tolerance
  )
  current_error  <- objective(current_candidate)
  if (!is.finite(current_error)) current_error <- Inf
  best_candidate <- current_candidate
  best_error     <- current_error
  track_error    <- numeric(max_iter * max_starts)
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
        if (cand_error < best_error) {
          best_error     <- cand_error
          best_candidate <- candidate
        }
      }

      temp        <- temp * cooling_rate
      global_iter <- global_iter + 1L
      track_error[global_iter] <- best_error
      if (is.finite(best_error) && best_error < tolerance) break
    }
    current_candidate <- best_candidate
    if (is.finite(best_error) && best_error < tolerance) break
    temp <- init_temp
  }

  track_error <- track_error[seq_len(global_iter)]

  list(
    data        = best_candidate,
    best_error  = best_error,
    track_error = track_error
  )
}
