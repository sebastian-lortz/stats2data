# animations for presentations

for (pkg in c("stats2data", "ggplot2", "afex")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required.")
}
if (!requireNamespace("gifski", quietly = TRUE) &&
    !requireNamespace("magick", quietly = TRUE))
  stop("Install either 'gifski' (recommended) or 'magick' to write GIFs.")
library(ggplot2)


out_dir <- "stats2data_animations"
dir.create(out_dir, showWarnings = FALSE)

settings <- list(
  vec = list(max_iter = 500,  max_starts = 1, seed = 1),
  aov = list(max_iter = 800,  max_starts = 1, seed = 2),
  mlr = list(max_iter = 4000, max_starts = 1, hill_climbs = 500, seed = 4)
)
verify_copies <- TRUE

frame_quota <- list(
  vec = c(start = 30, fit = 120),
  aov = c(cell_start = 40, cell_fit = 60, anneal = 120),
  mlr = c(marg_start = 40, marg_fit = 60, anneal = 120, hill = 30)
)

fps            <- 20   # use 20, 25 or 50 (GIF delays are in 1/100 s)
hold_start     <- 10   # extra copies of the first frame
hold_unit_end  <- 3    # extra copies at the end of each cell / variable
hold_phase_end <- 12   # extra copies at the end of each phase
hold_final     <- 40   # extra copies of the last frame

# Output size
gif_width  <- 960      # px
gif_height <- 620      # px
gif_res    <- 110      # ppi
bar_height <- 58       # pt, height of the objective bar below the plot

col <- c(ink = "#111111", muted = "#6B6B6B", grid = "#E9E9EC",
         main = "#2166AC", target = "#B2182B", track = "#E9E9EC")

ns <- asNamespace("stats2data")
needed <- c("objective_cpp", "heuristic_move_cont", "sd_range_integer",
            "build_aov_structure", "aov_targets", "compute_MSE_between",
            "move_between", "move_within_stratum", "get_design",
            "remap_target_cor", "error_function_cpp", "error_function_cpp_se",
            "new_s2d_vec", "new_s2d_aov", "new_s2d_mlr")
miss <- needed[!vapply(needed, exists, logical(1), envir = ns, inherits = FALSE)]
if (length(miss))
  stop("stats2data internals not found: ", paste(miss, collapse = ", "),
       ". The installed version differs from the one this script copies.")

rec_env <- new.env(parent = ns)   # copies see the package internals
local(envir = rec_env, {

  rec_sprite_start_vector_cont <- function(tMean, n, range, thresh, rec = NULL) {
    scaleMin <- range[1]
    scaleMax <- range[2]

    if (scaleMin >= scaleMax) stop("range[1] must be < range[2].")
    if (tMean < scaleMin || tMean > scaleMax) stop("Target mean is outside the allowable range.")
    if (n < 2) stop("n must be >= 2.")

    half_width <- min(tMean - scaleMin, scaleMax - tMean)
    vec <- stats::runif(n, min = tMean - half_width, max = tMean + half_width)
    if (!is.null(rec)) rec$push("start", 0L, abs(mean(vec) - tMean), thresh, vec)  # [REC]

    W <- scaleMax - scaleMin
    step_floor <- max(n * thresh, .Machine$double.eps)
    max_loops  <- min(ceiling(W * n / step_floor) * 100, 1e7)

    iter <- 0L
    cMean <- mean(vec)

    while (abs(cMean - tMean) > thresh && iter < max_loops) {
      iter <- iter + 1L
      cMean <- mean(vec)
      if (abs(cMean - tMean) < thresh) break

      #if (runif(1) < .2) delta <- 2 * abs(tMean - cMean) else
      delta <- abs(tMean - cMean)
      increaseMean <- (cMean < tMean)

      if (increaseMean) {
        can <- which(vec <= (scaleMax - delta))
        if (!length(can)) {
          delta <- abs(tMean - cMean)
          can <- which(vec <= (scaleMax - delta))
        }
        if (length(can)) {
          idx <- sample(can, 1)
          vec[idx] <- vec[idx] + delta
        }
      } else {
        can <- which(vec >= (scaleMin + delta))
        if (!length(can)) {
          delta <- abs(tMean - cMean)
          can <- which(vec >= (scaleMin + delta))
        }
        if (length(can)) {
          idx <- sample(can, 1)
          vec[idx] <- vec[idx] - delta
        }
      }
      if (!is.null(rec) && length(can))                                          # [REC]
        rec$push("start", iter, abs(mean(vec) - tMean), thresh, vec)             # [REC]
    }

    vec
  }

  rec_optim_vec_single <- function(N, t_mean, t_sd, rng, is_int,
                                   sprite_prec, thresh,
                                   max_iter, init_temp, cooling_rate,
                                   max_starts, rec = NULL) {
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

    current_candidate <- rec_sprite_start_vector_cont(
      tMean = t_mean, n = N, range = rng, thresh = thresh, rec = rec
    )
    current_error  <- objective(current_candidate)
    if (!is.finite(current_error)) current_error <- Inf
    best_candidate <- current_candidate
    best_objective     <- current_error
    track_objective    <- numeric(max_iter * max_starts)
    global_iter    <- 0L
    temp           <- init_temp
    if (!is.null(rec)) rec$push("fit", 0L, current_error, thresh, current_candidate)  # [REC]

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
          if (!is.null(rec))                                                     # [REC]
            rec$push("fit", global_iter + 1L, current_error, thresh, current_candidate)
        }

        temp        <- temp * cooling_rate
        global_iter <- global_iter + 1L
        track_objective[global_iter] <- best_objective
        if (is.finite(best_objective) && best_objective < thresh) break
      }
      current_candidate <- best_candidate
      if (is.finite(best_objective) && best_objective < thresh) break
      if (!is.null(rec) && s < max_starts)                                       # [REC]
        rec$push("fit", global_iter, best_objective, thresh, best_candidate, kind = "restart")
      temp <- init_temp
    }

    track_objective <- track_objective[seq_len(global_iter)]
    if (!is.null(rec))                                                           # [REC]
      rec$push("fit", global_iter, best_objective, thresh, best_candidate, kind = "best")

    list(
      data        = best_candidate,
      best_objective  = best_objective,
      track_objective = track_objective,
      error_msg   = NULL
    )
  }

  rec_optim_vec <- function(N,
                            target_mean,
                            target_sd,
                            range,
                            integer,
                            thresh = 5e-3,
                            sprite_prec = c(2, 2),
                            max_iter = 5e5,
                            init_temp = 1e-3,
                            cooling_rate = NULL,
                            max_starts = 3,
                            progress_mode = "console",
                            rec = NULL) {

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
    if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0)
      stop("`thresh` must be a single non-negative number.")
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
    if (length(integer) == 1) integer <- rep(integer[1], n_var)
    if (length(integer) < n_var && length(integer) > 1) {
      stop("`integer` must have length 1 or the same length as `target_mean`.")
    }
    if (!is.matrix(range)) range <- matrix(rep(range, n_var), nrow = 2, ncol = n_var)

    solution_matrix <- matrix(NA, nrow = N, ncol = n_var)
    best_objective_vec  <- vector("list", n_var)
    track_objective_all <- vector("list", n_var)
    error_msgs      <- vector("list", n_var)
    sol_rec <- matrix(NA_real_, nrow = N, ncol = n_var,                          # [REC]
                      dimnames = list(NULL, names(target_mean)))                 # [REC]

    handler <- switch(progress_mode,
                      console = list(progressr::handler_txtprogressbar()),
                      shiny   = list(progressr::handler_shiny()),
                      off     = list(progressr::handler_void())
    )

    progressr::with_progress({
      p <- progressr::progressor(steps = n_var)
      for (v in seq_len(n_var)) {
        # [REC] per-variable proxy: store the variable inside the full matrix
        unit_rec <- if (is.null(rec)) NULL else list(push = function(
    phase, iter, objective, thresh, data, unit = NA_character_,
    kind = "step", info = NULL) {
          sol_rec[, v] <<- data
          rec$push(phase, iter, objective, thresh, sol_rec,
                   unit = names(target_mean)[v], kind = kind, info = info)
        })
        res <- rec_optim_vec_single(                                             # [REC]
          N = N, t_mean = target_mean[v], t_sd = target_sd[v],
          rng = range[, v], is_int = integer[v],
          sprite_prec = sprite_prec, thresh = thresh,
          max_iter = max_iter, init_temp = init_temp,
          cooling_rate = cooling_rate, max_starts = max_starts,
          rec = unit_rec                                                         # [REC]
        )
        solution_matrix[, v] <- res$data
        sol_rec[, v] <- res$data                                                 # [REC]
        best_objective_vec[[v]]  <- res$best_objective
        track_objective_all[[v]] <- res$track_objective
        error_msgs[[v]]      <- res$error_msg
        p()
      }
    }, handlers = handler)

    colnames(solution_matrix) <- names(target_mean)
    solution_matrix <- as.data.frame(solution_matrix)

    result <- new_s2d_vec(
      best_objective  = best_objective_vec,
      data        = solution_matrix,
      inputs      = list(N = N, target_mean = target_mean, target_sd = target_sd,
                         range = range, integer = integer, thresh = thresh,
                         sprite_prec = sprite_prec, max_iter = max_iter,
                         init_temp = init_temp, cooling_rate = cooling_rate,
                         max_starts = max_starts, progress_mode = progress_mode),
      track_objective = track_objective_all
    )
    result$error_msgs <- error_msgs
    result
  }

  rec_hill_climb <- function(current_candidate, error_function, N,
                             hill_climbs = 1e2,
                             num_preds = NULL,
                             neighborhood_size = 4,
                             progressor = NULL,
                             pb_interval = NULL,
                             progress_mode = "console",
                             rec = NULL) {                                       # [REC]
    # input checks
    if (missing(current_candidate)) {
      stop("`current_candidate` must be provided, the matrix to be optimised.")
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

    curr_err  <- error_function(current_candidate)$total_error
    best_cand <- current_candidate
    best_err  <- curr_err
    if (!is.null(rec)) rec$push("hill", 0L, curr_err, .Machine$double.eps, current_candidate)  # [REC]

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
        if (!is.null(rec))                                                       # [REC]
          rec$push("hill", i, curr_err, .Machine$double.eps, current_candidate)
      }

      # update progress
      if (!is.null(progressor) && (i %% pb_interval == 0)) {
        progressor()
      }

      if (is.finite(best_err) && best_err < .Machine$double.eps) break
    }

    if (progress_mode == "console") {
      cat("\nHill climbing best objective:", best_err, "\n")
    }

    list(best_candidate = best_cand, best_objective = best_err)
  }

  # [REC] copy of optim_aov (fct_optim_aov.R)
  rec_optim_aov <- function(
    S,
    levels,
    target_group_means,
    target_f_list,
    integer,
    range,
    formula,
    factor_type,
    subgroup_sizes = NULL,
    thresh = 5e-3,
    max_iter = 1e4,
    init_temp = 1e-3,
    cooling_rate = NULL,
    max_starts = 3,
    progress_mode = "console",
    rec = NULL                                                                 # [REC]
  ) {

    # input checks
    N = S
    if (!is.numeric(range) || length(range) != 2 || range[1] >= range[2])
      stop("`range` must be a numeric vector of length 2 with range[1] < range[2].")
    if (!is.numeric(N) || length(N) != 1 || N <= 0 || N != as.integer(N))
      stop("`N` must be a single positive integer.")
    if (!is.numeric(levels) || length(levels) < 1 ||
        any(levels != as.integer(levels)) || !all(levels >= 2))
      stop("`levels` must be an integer vector with all values >= 2.")
    if (!is.logical(integer) || length(integer) != 1)
      stop("`integer` must be a single logical value.")
    if (!is.numeric(target_group_means) ||
        length(target_group_means) != prod(levels))
      stop("`target_group_means` must be numeric with length prod(levels) = ", prod(levels), ".")
    if (any(target_group_means < range[1] | target_group_means > range[2]))
      stop("All `target_group_means` must lie within `range`.")
    if (!is.list(target_f_list) ||
        !is.numeric(target_f_list$F_value) || length(target_f_list$F_value) < 1)
      stop("`target_f_list` must be a list with a numeric vector `F_value`.")
    if (!is.character(target_f_list$effect) ||
        length(target_f_list$effect) != length(target_f_list$F_value))
      stop("`target_f_list$effect` must match length of `target_f_list$F_value`.")
    if (any(!is.finite(target_f_list$F_value)) || any(target_f_list$F_value < 0))
      stop("`target_f_list$F_value` must be finite and non-negative.")
    if (is.character(formula) && length(formula) == 1) {
      formula <- stats::as.formula(formula)
    } else if (!inherits(formula, "formula")) {
      stop("`formula` must be a formula or single character string.")
    }
    # Always extract RHS only — outcome is named 'outcome' internally
    if (length(formula) == 3L) {
      formula <- stats::as.formula(paste("~", deparse(formula[[3]])))
    }
    if (!is.character(factor_type) ||
        length(factor_type) != length(levels) ||
        !all(factor_type %in% c("between", "within")))
      stop("`factor_type` must be 'between'/'within' matching length of `levels`.")
    n_between <- prod(levels[factor_type == "between"])
    if (!is.null(subgroup_sizes)) {
      if (!is.numeric(subgroup_sizes) || length(subgroup_sizes) != n_between)
        stop("`subgroup_sizes` must have length ", n_between, ".")
      if (N != sum(subgroup_sizes))
        stop("`N` must equal sum(subgroup_sizes).")
    }
    if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0)
      stop("`thresh` must be a single non-negative number.")
    if (!is.numeric(max_iter) || length(max_iter) != 1 || max_iter <= 0)
      stop("`max_iter` must be a single positive number.")
    if (!is.numeric(init_temp) || length(init_temp) != 1 || !is.finite(init_temp) || init_temp <= 0)
      stop("`init_temp` must be a single positive finite number.")
    if (!((is.numeric(cooling_rate) && length(cooling_rate) == 1 &&
           cooling_rate > 0 && cooling_rate < 1) || is.null(cooling_rate)))
      stop("`cooling_rate` must be in (0,1) or NULL.")
    if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1)
      stop("`max_starts` must be a single positive integer.")
    if (!is.character(progress_mode) || length(progress_mode) != 1 ||
        !progress_mode %in% c("console", "shiny", "off"))
      stop('`progress_mode` must be "console", "shiny", or "off".')

    # configure contrasts
    old_con <- options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
    on.exit(options(old_con), add = TRUE)

    # design structure
    structure   <- build_aov_structure(N, levels, factor_type, subgroup_sizes)
    factor_mat  <- structure$df[, -1, drop = FALSE]
    ID          <- structure$df$ID
    group_ids   <- apply(factor_mat, 1, paste0, collapse = "")
    group_idx   <- split(seq_along(group_ids), group_ids)
    group_sizes <- vapply(group_idx, length, integer(1))
    target_F    <- target_f_list$F_value
    formula_internal <- stats::as.formula(paste("outcome ~", deparse(formula[[2]])))

    # consistent means and F
    targets <- aov_targets(target_group_means, target_F, group_sizes,
                           effect_names = target_f_list$effect, levels, factor_type,
                           integer, thresh,
                           ID, factor_mat, group_idx, structure, formula_internal)
    adjusted_group_means <- targets$adjusted_means
    target_F     <- targets$adjusted_F
    numerator_MS <- targets$numerator_MS
    # the F targets must carry information on the within-cell variance
    if (sum(numerator_MS * target_F) <= 0)
      stop("The F targets carry no information on the within-cell variance ",
           "(every effect has F = 0 or equal cell means); nothing to optimise.")

    # candidate initialization --------------------------------------------------

    # integer data can only take the whole numbers inside `range`
    if (integer) range <- c(ceiling(range[1]), floor(range[2]))

    # (1) ballpark within-cell SD implied by the F targets: sqrt(MSE*)
    init_sd <- sqrt(sum(numerator_MS^2) / sum(numerator_MS * target_F))

    rec_template <- rep(NA_real_, length(group_ids))                             # [REC]

    vec_cell <- function(j) {
      n_j  <- group_sizes[j]
      mu_j <- adjusted_group_means[j]
      S_j  <- round(n_j * mu_j)
      sd_range_int <- sd_range_integer(S_j, n_j, x_min = range[1], x_max = range[2],
                                       sample_sd = TRUE)

      # [REC] per-cell proxy: store the cell inside the full candidate
      cell_rec <- if (is.null(rec)) NULL else list(push = function(
    phase, iter, objective, thresh, data, unit = NA_character_,
    kind = "step", info = NULL) {
        full <- rec_template
        full[group_idx[[j]]] <- data[, 1]
        rec$push(paste0("cell_", phase), iter, objective, thresh, full,
                 unit = names(group_idx)[j], kind = kind,
                 info = list(init_sd = init_sd, cell_mean = mu_j))
      })

      if (integer) {
        if (sd_range_int[2] == 0) return(rep(mu_j, n_j))   # constant cell
        # ballpark SD moved into the feasible integer range
        sd_j <- min(init_sd, sd_range_int[2])
        sd_j <- max(sd_j, sd_range_int[1])

        # (4) mean decimals that pin the sum S_j; SD decimals: the highest
        m_prec <- max(2L, floor(log10(n_j)) + 1L)
        mu_in  <- round(mu_j, m_prec)

        #     precision that rsprite2 accepts for this pair
        for (sd_prec in 1:8) {
          ok <- !inherits(try(rsprite2::set_parameters(
            mean = mu_in, sd = round(sd_j, sd_prec), n_obs = n_j,
            min_val = range[1], max_val = range[2],
            m_prec = m_prec, sd_prec = sd_prec, dont_test = TRUE),
            silent = TRUE), "try-error")
          if (ok) break
        }

        vec_args <- list(target_mean = c(y = mu_in),
                         target_sd   = c(y = round(sd_j, sd_prec)),
                         sprite_prec = c(m_prec, sd_prec))
      } else {
        # continuous
        vec_args <- list(target_mean = c(y = mu_j),
                         target_sd   = c(y = init_sd),
                         thresh      = 1e-12,
                         max_iter    = 1000)
      }
      x <- do.call(rec_optim_vec, c(                                             # [REC]
        list(N = n_j, range = range, integer = integer, init_temp = 1e-3,
             cooling_rate = NULL, max_starts = 1, progress_mode = "off",
             rec = cell_rec),                                                    # [REC]
        vec_args))$data$y
      x[sample.int(n_j)] # permute
    }

    current_candidate <- numeric(length(group_ids))
    for (j in seq_along(group_idx)) {
      current_candidate[group_idx[[j]]] <- vec_cell(j)
      rec_template[group_idx[[j]]] <- current_candidate[group_idx[[j]]]          # [REC]
    }

    # former start
    if (FALSE) {
      init_fn <- if (integer) sprite_start_vector else sprite_start_vector_cont
      current_candidate <- numeric(length(group_ids))
      for (j in seq_along(group_idx)) {
        current_candidate[group_idx[[j]]] <- init_fn(
          adjusted_group_means[j], group_sizes[j], range,
          thresh = if (integer) 1 / (2 * group_sizes[j]) else 1e-12
        )
      }
    }

    # objective function
    if (!structure$has_within) {
      # purely between
      objective <- function(x) {
        MSE_b <- compute_MSE_between(x, structure)
        if (MSE_b <= 0) return(Inf)
        comp_F <- numerator_MS / MSE_b
        sqrt(mean((comp_F - target_F)^2))
      }
    } else {
      objective <- function(x) {
        fit <- suppressMessages(
          afex::aov_car(formula = formula_internal,
                        data =  cbind(structure$df, outcome = x)))
        sqrt(mean((fit$anova_table$F - target_F)^2))
      }
    }

    # Move directives: one per error stratum.
    move_directives <- list()
    if (structure$has_between) {
      move_directives[[length(move_directives) + 1L]] <- list(type = "between")
    }
    if (structure$has_within) {
      K <- length(structure$within_levels)
      for (size in seq_len(K)) {
        for (W in utils::combn(K, size, simplify = FALSE)) {
          move_directives[[length(move_directives) + 1L]] <-
            list(type = "within", W = W)
        }
      }
    }
    n_moves <- length(move_directives)

    # SA setup
    if (is.null(cooling_rate)) cooling_rate <- (max_iter - 10) / max_iter
    best_candidate <- current_candidate
    current_error  <- objective(current_candidate)
    if (!is.finite(current_error)) current_error <- Inf
    best_objective     <- current_error
    track_objective    <- numeric(max_iter * max_starts)
    global_iter    <- 0L
    n_within_factors <- sum(factor_type == "within")
    needs_within_interaction <- n_within_factors >= 2 && any(grepl(":", target_f_list$effect))

    temp <- init_temp
    if (!is.null(rec)) rec$push("anneal", 0L, current_error, thresh, current_candidate)  # [REC]

    # progress handler
    handler <- switch(progress_mode,
                      console = list(progressr::handler_txtprogressbar()),
                      shiny   = list(progressr::handler_shiny()),
                      off     = list(progressr::handler_void())
    )
    pb_interval <- max(floor(max_iter / 100), 1)

    # optimization loop
    progressr::with_progress({
      p <- progressr::progressor(steps = ceiling(max_iter * max_starts / pb_interval))
      for (s in seq_len(max_starts)) {
        for (i in seq_len(max_iter)) {
          candidate <- current_candidate
          # candidate modification
          mv <- move_directives[[sample.int(n_moves, 1L)]]
          candidate <- if (mv$type == "between") {
            move_between(current_candidate, integer, structure, range)
          } else {
            move_within_stratum(current_candidate, mv$W, integer, structure, range)
          }
          # candidate evaluation
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
            if (!is.null(rec))                                                   # [REC]
              rec$push("anneal", global_iter + 1L, current_error, thresh, current_candidate)
          }
          # Within-cell permutation escape
          if (integer && i %% 100L == 0L) {
            esc_candidate <- current_candidate
            j <- sample(seq_along(group_idx), 1L)
            idx <- group_idx[[j]]
            n_j <- length(idx)
            n_perm <- 2
            sel <- sample(idx, n_perm)
            esc_candidate[sel] <- sample(esc_candidate[sel])
            esc_error <- objective(esc_candidate)
            if (is.finite(esc_error)) {
              current_candidate <- esc_candidate
              current_error     <- esc_error
              if (esc_error < best_objective) {
                best_objective     <- esc_error
                best_candidate <- esc_candidate
              }
              if (!is.null(rec))                                                 # [REC]
                rec$push("anneal", global_iter + 1L, current_error, thresh, current_candidate)
            }
          }

          temp <- temp * cooling_rate
          global_iter <- global_iter + 1L
          track_objective[global_iter] <- best_objective
          if (global_iter %% pb_interval == 0) p()
          if (is.finite(best_objective) && best_objective < thresh) break
        }
        current_candidate <- best_candidate
        if (is.finite(best_objective) && best_objective < thresh) break
        if (!is.null(rec) && s < max_starts)                                     # [REC]
          rec$push("anneal", global_iter, best_objective, thresh, best_candidate, kind = "restart")
        temp <- init_temp
      }
    }, handlers = handler)

    track_objective <- track_objective[seq_len(global_iter)]
    if (!is.null(rec))                                                           # [REC]
      rec$push("anneal", global_iter, best_objective, thresh, best_candidate, kind = "best")

    # assemble output
    out_data <- data.frame(ID = ID, factor_mat, outcome = best_candidate)

    # return
    new_s2d_aov(
      best_objective       = best_objective,
      data             = out_data,
      inputs           = list(
        N = N, levels = levels,
        target_group_means = target_group_means,
        target_f_list = target_f_list,
        integer = integer, range = range,
        formula = formula, factor_type = factor_type,
        subgroup_sizes = subgroup_sizes,
        thresh = thresh, max_iter = max_iter,
        init_temp = init_temp, cooling_rate = cooling_rate,
        max_starts = max_starts, progress_mode = progress_mode
      ),
      adjusted_targets = list(
        group_means = adjusted_group_means,
        F_values    = target_F
      ),
      track_objective      = track_objective
    )
  }

  # [REC] copy of optim_mlr (fct_optim_mlr.R)
  rec_optim_mlr <- function(
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
    progress_mode = "console",
    rec = NULL                                                                 # [REC]
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
    if (!is.numeric(max_starts) || length(max_starts) != 1 || max_starts < 1)
      stop("`max_starts` must be a single positive integer.")
    if (!(is.null(hill_climbs) ||
          (is.numeric(hill_climbs) && length(hill_climbs) == 1 &&
           hill_climbs >= 0 && hill_climbs == as.integer(hill_climbs))))
      stop("`hill_climbs` must be NULL or a single non-negative integer.")
    if (!is.character(progress_mode) || length(progress_mode) != 1 ||
        !progress_mode %in% c("console", "shiny", "off"))
      stop('`progress_mode` must be "console", "shiny", or "off".')

    # [REC] proxy: tag the marginal phase
    marg_rec <- if (is.null(rec)) NULL else list(push = function(
    phase, iter, objective, thresh, data, unit = NA_character_,
    kind = "step", info = NULL)
      rec$push(paste0("marg_", phase), iter, objective, thresh, data,
               unit = unit, kind = kind, info = info))

    # marginals optimization via optim_vec
    vec_result <- rec_optim_vec(                                                 # [REC]
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
      progress_mode = progress_mode,
      rec           = marg_rec                                                   # [REC]
    )
    sim_data <- vec_result$data

    # design structure
    col_names  <- c(pred_names, dv_name)
    predictors <- as.matrix(sim_data[, pred_names, drop = FALSE])
    outcome    <- sim_data[[dv_name]]
    num_preds  <- ncol(predictors)

    # [REC] full data matrix of a candidate, and proxy for hill climbing
    mlr_snap <- function(cand) {
      m <- cbind(cand, outcome)
      colnames(m) <- col_names
      m
    }
    hill_rec <- if (is.null(rec)) NULL else list(push = function(
    phase, iter, objective, thresh, data, unit = NA_character_,
    kind = "step", info = NULL)
      rec$push(phase, iter, objective, thresh, mlr_snap(data),
               unit = unit, kind = kind, info = info))

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
        # fresh marginals on restarts
        if (FALSE) {
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
                sprite_prec = sprite_prec, thresh = thresh,
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
        if (!is.null(rec) && s == 1L)                                            # [REC]
          rec$push("anneal", 0L, current_error, thresh, mlr_snap(current_candidate))
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
            if (!is.null(rec))                                                   # [REC]
              rec$push("anneal", global_iter + 1L, current_error, thresh,
                       mlr_snap(current_candidate))
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
        if (!is.null(rec) && s < max_starts)                                     # [REC]
          rec$push("anneal", global_iter, best_objective, thresh,
                   mlr_snap(best_candidate), kind = "restart")
        temp <- init_temp
      }

      track_objective       <- track_objective[seq_len(global_iter)]
      track_objective_ratio <- track_objective_ratio[seq_len(global_iter)]
      if (!is.null(rec))                                                         # [REC]
        rec$push("anneal", global_iter, best_objective, thresh,
                 mlr_snap(best_candidate), kind = "best")

      # hill climbing refinement
      if (!is.null(hill_climbs) && hill_climbs > 0) {
        local_opt <- rec_hill_climb(                                             # [REC]
          current_candidate = best_candidate,
          error_function    = error_function,
          N                 = N,
          hill_climbs       = hill_climbs,
          num_preds         = num_preds,
          progressor        = p,
          pb_interval       = pb_interval,
          progress_mode     = progress_mode,
          rec               = hill_rec                                           # [REC]
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
})


# ---- 3. Recording, frame selection and GIF writing ------------------------

# Recorder: stores every pushed state in order of occurrence
new_recorder <- function() {
  e <- new.env()
  e$states <- vector("list", 1024L)
  e$n <- 0L
  e$push <- function(phase, iter, objective, thresh, data,
                     unit = NA_character_, kind = "step", info = NULL) {
    e$n <- e$n + 1L
    if (e$n > length(e$states)) length(e$states) <- 2L * length(e$states)
    e$states[[e$n]] <- list(phase = phase, unit = unit, iter = iter,
                            objective = objective, thresh = thresh,
                            data = data, kind = kind, info = info)
    invisible(NULL)
  }
  e$get <- function() e$states[seq_len(e$n)]
  e
}

# Run a recording copy; optionally compare with the installed package
record_run <- function(label, seed, rec_fun, pkg_fun, args) {
  rec <- new_recorder()
  message("[", label, "] recording run ...")
  set.seed(seed)
  res <- do.call(rec_fun, c(args, list(rec = rec)))
  if (verify_copies) {
    message("[", label, "] verifying against the installed package ...")
    set.seed(seed)
    ref <- do.call(pkg_fun, args)
    if (!identical(res$data, ref$data) ||
        !identical(res$track_objective, ref$track_objective))
      stop("[", label, "] the recording copy diverges from the installed ",
           "stats2data. Update the copy in section 2 to the package version.")
  }
  states <- rec$get()
  message("[", label, "] ", length(states), " states recorded")
  list(result = res, states = states)
}

state_key <- function(states)
  vapply(states, function(s) paste(s$phase, s$unit), character(1))

# Frame selection: evenly spaced within each phase up to its quota; the first
# and last state of every phase/unit and all restart / best states are kept
select_frames <- function(states, quota) {
  phase <- vapply(states, `[[`, character(1), "phase")
  kind  <- vapply(states, `[[`, character(1), "kind")
  key   <- state_key(states)
  keep  <- which(kind != "step")
  for (ph in unique(phase)) {
    idx <- which(phase == ph)
    q <- if (ph %in% names(quota)) quota[[ph]] else Inf
    if (length(idx) > q)
      idx <- idx[unique(round(seq(1, length(idx), length.out = max(q, 2))))]
    keep <- c(keep, idx)
  }
  firsts <- which(!duplicated(key))
  lasts  <- which(!duplicated(key, fromLast = TRUE))
  sort(unique(c(keep, firsts, lasts)))
}

# Log-scale progress from the first objective of a phase/unit to its threshold
progress_frac <- function(o0, o, th) {
  if (!is.finite(o)) return(0)
  if (o <= th || o0 <= th) return(1)
  min(max((log(o0) - log(o)) / (log(o0) - log(th)), 0), 1)
}

fmt_num <- function(x) {
  if (!is.finite(x)) return("\u221e")
  formatC(x, format = "g", digits = 4)
}

open_png <- function(file) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = gif_width, height = gif_height, res = gif_res)
  } else {
    grDevices::png(file, width = gif_width, height = gif_height, res = gif_res)
  }
}

# Objective bar: current objective (left), threshold (right), filled share
bar_plot <- function(st, o0, obj_label) {
  frac <- progress_frac(o0, st$objective, st$thresh)
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = col[["track"]]) +
    annotate("rect", xmin = 0, xmax = max(frac, 1e-6), ymin = 0, ymax = 1,
             fill = col[["main"]]) +
    annotate("text", x = 0, y = 2.1, hjust = 0, vjust = 0.5, size = 3.3,
             colour = col[["ink"]],
             label = sprintf("Objective  %s   %s", obj_label, fmt_num(st$objective))) +
    annotate("text", x = 1, y = 2.1, hjust = 1, vjust = 0.5, size = 3.3,
             colour = col[["muted"]],
             label = sprintf("threshold  %s", fmt_num(st$thresh))) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.8), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(2, 22, 12, 16),
          plot.background = element_rect(fill = "white", colour = NA))
}

draw_frame <- function(p_main, p_bar) {
  grid::grid.newpage()
  lay <- grid::grid.layout(2, 1, heights = grid::unit(c(1, bar_height), c("null", "pt")))
  grid::pushViewport(grid::viewport(layout = lay))
  grid::pushViewport(grid::viewport(layout.pos.row = 1))
  grid::grid.draw(ggplotGrob(p_main))
  grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  grid::grid.draw(ggplotGrob(p_bar))
  grid::popViewport(2)
}

# Render the selected states and write the GIF.
render_gif <- function(states, idx, build_main, obj_label, file, label) {
  key <- state_key(states)
  first_obj <- tapply(vapply(states, `[[`, numeric(1), "objective"), key,
                      function(o) o[1])
  tmp <- tempfile(paste0("frames_", label))
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  files <- character(length(idx))
  for (f in seq_along(idx)) {
    i  <- idx[f]
    st <- states[[i]]
    files[f] <- file.path(tmp, sprintf("f%05d.png", f))
    open_png(files[f])
    draw_frame(build_main(st, i),
               bar_plot(st, first_obj[[key[i]]], obj_label(st)))
    grDevices::dev.off()
    if (f %% 25 == 0) message("[", label, "] rendered ", f, "/", length(idx))
  }

  # holds: repeat frames at the start and at unit / phase / final ends
  phase <- vapply(states[idx], `[[`, character(1), "phase")
  k     <- key[idx]
  n     <- length(idx)
  reps  <- rep(1L, n)
  reps[1] <- reps[1] + hold_start
  unit_end  <- c(k[-1] != k[-n], TRUE)
  phase_end <- c(phase[-1] != phase[-n], TRUE)
  reps <- reps + unit_end * hold_unit_end + phase_end * hold_phase_end
  reps[n] <- reps[n] + hold_final
  seq_files <- rep(files, times = reps)

  out <- file.path(out_dir, file)
  if (requireNamespace("gifski", quietly = TRUE)) {
    gifski::gifski(seq_files, gif_file = out, width = gif_width,
                   height = gif_height, delay = 1 / fps, progress = FALSE)
  } else {
    imgs <- magick::image_read(files)
    magick::image_write(
      magick::image_animate(imgs, delay = round(reps * 100 / fps), optimize = TRUE),
      out)
  }
  message("Saved ", out)
}


# ---- 4. Plot helpers ------------------------------------------------------

theme_anim <- function(base_size = 12, grid_x = FALSE) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(colour = col[["ink"]], face = "bold",
                                   size = rel(1.2), margin = margin(b = 4)),
      plot.subtitle = element_text(colour = col[["muted"]],
                                   margin = margin(b = 12)),
      axis.title    = element_text(colour = col[["muted"]], size = rel(0.9)),
      axis.text     = element_text(colour = col[["muted"]]),
      strip.text    = element_text(colour = col[["ink"]], face = "bold",
                                   hjust = 0, margin = margin(b = 6)),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = if (grid_x)
        element_line(colour = col[["grid"]], linewidth = 0.3) else element_blank(),
      panel.grid.major.y = element_line(colour = col[["grid"]], linewidth = 0.3),
      panel.spacing   = unit(1.4, "lines"),
      legend.position = "top",
      legend.justification = "left",
      legend.title    = element_blank(),
      legend.margin   = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(16, 22, 4, 16)
    )
}

corner_text <- function(box, size = 3.2) {
  grp <- if (is.null(box$panel)) rep(1L, nrow(box)) else as.integer(box$panel)
  box$y <- box$y - box$dy * (stats::ave(seq_len(nrow(box)), grp, FUN = seq_along) - 1)
  roles <- c(header = col[["ink"]], target = col[["target"]], current = col[["main"]])
  layers <- lapply(names(roles), function(r) {
    d <- box[box$role == r, , drop = FALSE]
    if (!nrow(d)) return(NULL)
    geom_text(data = d, aes(x = x, y = y, label = label), hjust = 1, vjust = 1,
              family = "mono", size = size, colour = roles[[r]],
              inherit.aes = FALSE)
  })
  Filter(Negate(is.null), layers)
}

dens_grid <- function(x, bw, from, to, n = 256) {
  d <- stats::density(x, bw = bw, from = from, to = to, n = n)
  data.frame(x = d$x, y = d$y)
}

step_text <- function(st, what) {
  switch(st$kind,
         restart = sprintf("%s  \u00b7  restart from the best candidate", what),
         best    = sprintf("%s  \u00b7  best candidate", what),
         sprintf("%s  \u00b7  %s %s", what,
                 if (grepl("start", st$phase)) "step" else "iteration",
                 format(st$iter, big.mark = ",")))
}


vec_t <- list(N = 60, mean = 5, sd = 1.2, range = c(0, 10), thresh = 1e-3)
vec_args <- list(
  N = vec_t$N, target_mean = c(x = vec_t$mean), target_sd = c(x = vec_t$sd),
  range = vec_t$range, integer = FALSE, thresh = vec_t$thresh,
  max_iter = settings$vec$max_iter, max_starts = settings$vec$max_starts,
  progress_mode = "off")

run_v <- record_run("vec", settings$vec$seed, rec_env$rec_optim_vec,
                    stats2data::optim_vec, vec_args)
st_v  <- run_v$states
idx_v <- select_frames(st_v, frame_quota$vec)

bw_v   <- stats::bw.nrd0(run_v$result$data$x)
ytop_v <- 1.3 * max(vapply(st_v[idx_v], function(s)
  max(dens_grid(s$data[, 1], bw_v, vec_t$range[1], vec_t$range[2])$y), 1))

build_vec <- function(st, i) {
  x <- st$data[, 1]
  d <- dens_grid(x, bw_v, vec_t$range[1], vec_t$range[2])
  box <- data.frame(
    label = c(sprintf("target   M %5.2f   SD %5.2f", vec_t$mean, vec_t$sd),
              sprintf("current  M %5.2f   SD %5.2f", mean(x), stats::sd(x))),
    role = c("target", "current"),
    x = vec_t$range[2] - 0.015 * diff(vec_t$range), y = 0.97 * ytop_v,
    dy = 0.075 * ytop_v)
  what <- if (st$phase == "start")
    "Initialisation  \u00b7  uniform draw around the target mean, mean correction"
  else "Annealing"
  ggplot() +
    geom_area(data = d, aes(x, y), fill = col[["main"]], alpha = 0.22) +
    geom_line(data = d, aes(x, y), colour = col[["main"]], linewidth = 1.1) +
    geom_rug(data = data.frame(x = x), aes(x = x), colour = col[["main"]],
             alpha = 0.55, length = unit(0.03, "npc")) +
    corner_text(box) +
    scale_x_continuous(limits = vec_t$range, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, ytop_v), expand = c(0, 0), labels = NULL) +
    labs(title = sprintf("optim_vec  \u00b7  continuous vector, N = %d", vec_t$N),
         subtitle = step_text(st, what), x = NULL, y = "Density") +
    theme_anim()
}
obj_vec <- function(st)
  if (st$phase == "start") "|M \u2212 M*|" else "|SD \u2212 SD*|"

render_gif(st_v, idx_v, build_vec, obj_vec, "optim_vec_continuous.gif", "vec")


aov_S <- 40
set.seed(10)
gen <- expand.grid(Factor2 = 1:2, ID = 1:aov_S)
gen$Factor1 <- ifelse(gen$ID <= aov_S / 2, 1, 2)
mu_gen  <- c(4, 5, 5, 7)                          # cells 11, 12, 21, 22
u_gen   <- stats::rnorm(aov_S, 0, 1.2)            # subject effects
gen$y   <- mu_gen[(gen$Factor1 - 1) * 2 + gen$Factor2] +
  u_gen[gen$ID] + stats::rnorm(nrow(gen), 0, 1)
gen$Factor1 <- factor(gen$Factor1); gen$Factor2 <- factor(gen$Factor2)
aov_formula <- "~ Factor1 * Factor2 + Error(ID/Factor2)"
fit_gen <- suppressMessages(afex::aov_car(
  stats::as.formula(paste("y", aov_formula)), data = gen))

aov_t <- list(
  means  = round(as.vector(tapply(gen$y, list(gen$Factor2, gen$Factor1), mean)), 2),
  effect = c("Factor1", "Factor2", "Factor1:Factor2"),
  F      = round(fit_gen$anova_table$F, 2),
  range  = c(0, 12),
  thresh = 1e-3
)
aov_args <- list(
  S = aov_S, levels = c(2, 2), target_group_means = aov_t$means,
  target_f_list = list(effect = aov_t$effect, F_value = aov_t$F),
  integer = FALSE, range = aov_t$range, formula = aov_formula,
  factor_type = c("between", "within"), thresh = aov_t$thresh,
  max_iter = settings$aov$max_iter, max_starts = settings$aov$max_starts,
  progress_mode = "off")

run_a <- record_run("aov", settings$aov$seed, rec_env$rec_optim_aov,
                    stats2data::optim_aov, aov_args)
st_a  <- run_a$states
idx_a <- select_frames(st_a, frame_quota$aov)
d_a   <- run_a$result$data                       # design in candidate order

cell_levels <- c("11" = "Group 1 \u00b7 Time 1", "12" = "Group 1 \u00b7 Time 2",
                 "21" = "Group 2 \u00b7 Time 1", "22" = "Group 2 \u00b7 Time 2")
cell_a  <- factor(cell_levels[paste0(d_a$Factor1, d_a$Factor2)],
                  levels = cell_levels)
pal_aov <- stats::setNames(c("#9ECAE1", "#2166AC", "#FDAE6B", "#B2182B"),
                           cell_levels)          # light = Time 1, dark = Time 2
bw_a    <- mean(vapply(split(d_a$outcome, cell_a), stats::bw.nrd0, 1))

dens_aov <- function(x) {
  parts <- lapply(levels(cell_a), function(cl) {
    v <- x[cell_a == cl]
    if (all(is.na(v))) return(NULL)
    cbind(dens_grid(v, bw_a, aov_t$range[1], aov_t$range[2]), cell = cl)
  })
  out <- do.call(rbind, parts)
  out$cell <- factor(out$cell, levels = cell_levels)
  out
}
ytop_a <- 1.35 * max(vapply(st_a[idx_a], function(s) max(dens_aov(s$data)$y), 1))

aov_F <- function(x) {
  d <- d_a; d$outcome <- x
  fit <- suppressMessages(afex::aov_car(
    stats::as.formula(paste("outcome", aov_formula)), data = d))
  fit$anova_table$F
}

build_aov <- function(st, i) {
  d  <- dens_aov(st$data)
  bx <- aov_t$range[2] - 0.015 * diff(aov_t$range)
  if (st$phase == "anneal") {
    Fc  <- aov_F(st$data)
    lab <- c(sprintf("%-8s%8s%8s%8s", "F", "Group", "Time", "GxT"),
             sprintf("%-8s%8.2f%8.2f%8.2f", "target", aov_t$F[1], aov_t$F[2], aov_t$F[3]),
             sprintf("%-8s%8.2f%8.2f%8.2f", "current", Fc[1], Fc[2], Fc[3]))
    what <- "Annealing"
  } else {
    cl  <- cell_levels[[st$unit]]
    v   <- st$data[cell_a == cl]
    lab <- c(cl,
             sprintf("%-8s M %5.2f   SD %5.2f", "target",
                     st$info$cell_mean, st$info$init_sd),
             sprintf("%-8s M %5.2f   SD %5.2f", "current", mean(v), stats::sd(v)))
    what <- if (st$phase == "cell_start")
      sprintf("Initialisation 1/2  \u00b7  uniform draw  \u00b7  %s", cl)
    else sprintf("Initialisation 2/2  \u00b7  optim_vec to the pooled SD  \u00b7  %s", cl)
  }
  box <- data.frame(label = lab, role = c("header", "target", "current"),
                    x = bx, y = 0.97 * ytop_a, dy = 0.07 * ytop_a)
  ggplot() +
    geom_line(data = d, aes(x, y, colour = cell, group = cell), linewidth = 1.1) +
    corner_text(box) +
    scale_colour_manual(values = pal_aov, drop = FALSE) +
    scale_x_continuous(limits = aov_t$range, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, ytop_a), expand = c(0, 0), labels = NULL) +
    guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 2))) +
    labs(title = sprintf("optim_aov  \u00b7  2 \u00d7 2 mixed design, S = %d", aov_S),
         subtitle = step_text(st, what), x = NULL, y = "Density") +
    theme_anim()
}
obj_aov <- function(st) switch(st$phase,
                               cell_start = "|M \u2212 M*| of the cell",
                               cell_fit   = "|SD \u2212 SD*| of the cell",
                               anneal     = "RMSE of F")

render_gif(st_a, idx_a, build_aov, obj_aov, "optim_aov_mixed.gif", "aov")


partial_var <- "X1"   # "X1" or "X2": predictor shown in the partial plot
mlr_N <- 60
set.seed(3)
x1g <- stats::rnorm(mlr_N, 5, 1.5)
x2g <- 0.4 * x1g + stats::rnorm(mlr_N, 3, 1.2)
yg  <- 1 + 0.8 * x1g - 0.6 * x2g + stats::rnorm(mlr_N, 0, 1)
gen_mlr <- data.frame(X1 = x1g, X2 = x2g, Y = yg)
R_gen   <- stats::cor(gen_mlr)

mlr_t <- list(
  mean   = round(colMeans(gen_mlr), 2),
  sd     = round(apply(gen_mlr, 2, stats::sd), 2),
  cor    = round(R_gen[upper.tri(R_gen)], 2),   # r(X1,X2), r(X1,Y), r(X2,Y)
  reg    = round(stats::coef(stats::lm(Y ~ X1 + X2, gen_mlr)), 2),
  range  = c(-5, 15),
  thresh = 1e-3
)
mlr_args <- list(
  N = mlr_N, target_mean = mlr_t$mean, target_sd = mlr_t$sd,
  range = mlr_t$range, integer = FALSE,
  target_cor = mlr_t$cor, target_reg = mlr_t$reg,
  reg_equation = "Y ~ X1 + X2", thresh = mlr_t$thresh,
  max_iter = settings$mlr$max_iter, max_starts = settings$mlr$max_starts,
  hill_climbs = settings$mlr$hill_climbs, progress_mode = "off")

run_m <- record_run("mlr", settings$mlr$seed, rec_env$rec_optim_mlr,
                    stats2data::optim_mlr, mlr_args)
st_m  <- run_m$states
idx_m <- select_frames(st_m, frame_quota$mlr)
vars  <- c("X1", "X2", "Y")
is_init <- function(st) grepl("^marg_", st$phase)

init_idx <- idx_m[vapply(st_m[idx_m], is_init, logical(1))]
bw_m <- vapply(vars, function(v) stats::bw.nrd0(run_m$result$optim_vec$data[[v]]), 1)
xlim_m <- lapply(vars, function(v) {
  r <- range(unlist(lapply(st_m[init_idx], function(s) s$data[, v])), na.rm = TRUE)
  r + c(-1, 1) * 0.02 * diff(r)
})
names(xlim_m) <- vars
dens_m <- function(dat) do.call(rbind, lapply(vars, function(v) {
  x <- dat[, v]
  if (all(is.na(x))) return(NULL)
  cbind(dens_grid(x, bw_m[[v]], xlim_m[[v]][1], xlim_m[[v]][2]), panel = v)
}))
ytop_m <- vapply(vars, function(v) 1.35 * max(vapply(st_m[init_idx], function(s) {
  d <- dens_m(s$data)
  if (is.null(d) || !any(d$panel == v)) 0 else max(d$y[d$panel == v])
}, 1)), 1)
lims_init <- do.call(rbind, lapply(vars, function(v)
  data.frame(panel = v, x = xlim_m[[v]], y = c(0, ytop_m[[v]]))))
lims_init$panel <- factor(lims_init$panel, levels = vars)

build_mlr_init <- function(st) {
  d <- dens_m(st$data)
  if (!is.null(d)) d$panel <- factor(d$panel, levels = vars)
  box <- do.call(rbind, lapply(vars, function(v) {
    x <- st$data[, v]
    lab  <- sprintf("%-8s M %5.2f   SD %5.2f", "target", mlr_t$mean[[v]], mlr_t$sd[[v]])
    role <- "target"
    if (!all(is.na(x))) {
      lab  <- c(lab, sprintf("%-8s M %5.2f   SD %5.2f", "current", mean(x), stats::sd(x)))
      role <- c(role, "current")
    }
    data.frame(panel = factor(v, levels = vars), label = lab, role = role,
               x = xlim_m[[v]][2] - 0.015 * diff(xlim_m[[v]]),
               y = 0.97 * ytop_m[[v]], dy = 0.075 * ytop_m[[v]])
  }))
  what <- if (st$phase == "marg_start")
    sprintf("Initialisation 1/2  \u00b7  uniform draw  \u00b7  %s", st$unit)
  else sprintf("Initialisation 2/2  \u00b7  optim_vec marginal  \u00b7  %s", st$unit)
  p <- ggplot() + geom_blank(data = lims_init, aes(x, y))
  if (!is.null(d))
    p <- p + geom_area(data = d, aes(x, y), fill = col[["main"]], alpha = 0.22) +
    geom_line(data = d, aes(x, y), colour = col[["main"]], linewidth = 1)
  p + corner_text(box, size = 2.9) +
    facet_wrap(~ panel, scales = "free") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = NULL) +
    labs(title = sprintf("optim_mlr  \u00b7  Y ~ X1 + X2, N = %d", mlr_N),
         subtitle = step_text(st, what), x = NULL, y = "Density") +
    theme_anim(base_size = 11)
}

other_var <- setdiff(c("X1", "X2"), partial_var)
panels <- c("Y vs X1", "Y vs X2",
            sprintf("Partial: Y vs %s, adjusted for %s", partial_var, other_var))
r_target <- c(X1 = mlr_t$cor[2], X2 = mlr_t$cor[3])
b_target <- mlr_t$reg[[partial_var]]

scatter_parts <- function(dat) {
  d  <- as.data.frame(dat)
  ex <- stats::resid(stats::lm(d[[partial_var]] ~ d[[other_var]]))
  ey <- stats::resid(stats::lm(d$Y ~ d[[other_var]]))
  list(d = d, ex = ex, ey = ey,
       pts = rbind(data.frame(panel = panels[1], x = d$X1, y = d$Y),
                   data.frame(panel = panels[2], x = d$X2, y = d$Y),
                   data.frame(panel = panels[3], x = ex,   y = ey)))
}
sa_idx  <- setdiff(idx_m, init_idx)
all_pts <- do.call(rbind, lapply(st_m[sa_idx], function(s) scatter_parts(s$data)$pts))
lims_sa <- do.call(rbind, lapply(panels, function(pn) {
  xr <- range(all_pts$x[all_pts$panel == pn])
  yr <- range(all_pts$y[all_pts$panel == pn])
  xr <- xr + c(-1, 1) * 0.04 * diff(xr)
  yr <- c(yr[1] - 0.04 * diff(yr), yr[2] + 0.30 * diff(yr))   # headroom for text
  data.frame(panel = pn, x = xr, y = yr)
}))
lims_sa$panel <- factor(lims_sa$panel, levels = panels)
lim_of <- function(pn) lims_sa[lims_sa$panel == pn, ]

seg <- function(x_rng, slope, x0, y0, pn)
  data.frame(panel = pn, x = x_rng[1], xend = x_rng[2],
             y = y0 + slope * (x_rng[1] - x0), yend = y0 + slope * (x_rng[2] - x0))
tgt_mlr <- rbind(
  seg(lim_of(panels[1])$x, r_target[["X1"]] * mlr_t$sd[["Y"]] / mlr_t$sd[["X1"]],
      mlr_t$mean[["X1"]], mlr_t$mean[["Y"]], panels[1]),
  seg(lim_of(panels[2])$x, r_target[["X2"]] * mlr_t$sd[["Y"]] / mlr_t$sd[["X2"]],
      mlr_t$mean[["X2"]], mlr_t$mean[["Y"]], panels[2]),
  seg(lim_of(panels[3])$x, b_target, 0, 0, panels[3]))
tgt_mlr$panel <- factor(tgt_mlr$panel, levels = panels)

build_mlr_sa <- function(st) {
  sp <- scatter_parts(st$data)
  sp$pts$panel <- factor(sp$pts$panel, levels = panels)
  ols <- function(x, y, pn) {
    b <- stats::coef(stats::lm(y ~ x))
    seg(range(x), b[[2]], 0, b[[1]], pn)
  }
  lines <- rbind(ols(sp$d$X1, sp$d$Y, panels[1]), ols(sp$d$X2, sp$d$Y, panels[2]),
                 ols(sp$ex, sp$ey, panels[3]))
  lines$panel <- factor(lines$panel, levels = panels)
  cur <- c(stats::cor(sp$d$X1, sp$d$Y), stats::cor(sp$d$X2, sp$d$Y),
           stats::coef(stats::lm(sp$ey ~ sp$ex))[[2]])
  tgt <- c(r_target[["X1"]], r_target[["X2"]], b_target)
  sym <- c("r", "r", "b")
  box <- do.call(rbind, lapply(1:3, function(k) {
    L <- lim_of(panels[k])
    data.frame(panel = factor(panels[k], levels = panels),
               label = c(sprintf("%-8s %s %6.2f", "target", sym[k], tgt[k]),
                         sprintf("%-8s %s %6.2f", "current", sym[k], cur[k])),
               role = c("target", "current"),
               x = L$x[2] - 0.02 * diff(L$x), y = L$y[2] - 0.03 * diff(L$y),
               dy = 0.075 * diff(L$y))
  }))
  what <- if (st$phase == "hill") "Hill climbing" else "Annealing"
  ggplot() +
    geom_blank(data = lims_sa, aes(x, y)) +
    geom_segment(data = tgt_mlr, aes(x, y, xend = xend, yend = yend),
                 colour = col[["target"]], linetype = "dashed", linewidth = 0.7) +
    geom_point(data = sp$pts, aes(x, y), colour = col[["main"]],
               alpha = 0.55, size = 1.7, stroke = 0) +
    geom_segment(data = lines, aes(x, y, xend = xend, yend = yend),
                 colour = col[["main"]], linewidth = 1.1) +
    corner_text(box, size = 2.9) +
    facet_wrap(~ panel, scales = "free") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = sprintf("optim_mlr  \u00b7  Y ~ X1 + X2, N = %d", mlr_N),
         subtitle = step_text(st, what), x = NULL, y = NULL) +
    theme_anim(base_size = 11, grid_x = TRUE)
}

build_mlr <- function(st, i) if (is_init(st)) build_mlr_init(st) else build_mlr_sa(st)
obj_mlr <- function(st) switch(st$phase,
                               marg_start = sprintf("|M \u2212 M*| of %s", st$unit),
                               marg_fit   = sprintf("|SD \u2212 SD*| of %s", st$unit),
                               anneal     = "weighted RMSE of r and b",
                               hill       = "weighted RMSE of r and b")

render_gif(st_m, idx_m, build_mlr, obj_mlr, "optim_mlr_partial.gif", "mlr")
