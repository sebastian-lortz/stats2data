#' Optimize simulated data to match ANOVA F-values
#'
#' Uses simulated annealing to generate raw data that reproduce target
#' ANOVA F-statistics and group means under a specified factorial design.
#'
#' @param S Integer. Total number of subjects.
#' @param levels Integer vector. Number of levels per factor.
#' @param target_group_means Numeric vector of length \code{prod(levels)}.
#'   Target cell means in lexicographic sort order: Factor1 slowest, Factor2 fastest, i.e. 11, 12, 21, 22 for a 2x2.
#' @param target_f_list List with components \code{effect} (character) and
#'   \code{F_value} (numeric) of equal length.
#' @param integer Logical. Generate integer-valued data?
#' @param range Numeric vector of length 2. Bounds for individual observations.
#' @param formula Formula or character. ANOVA model formula.
#' @param factor_type Character vector (\code{"between"}/\code{"within"})
#'   matching length of \code{levels}.
#' @param subgroup_sizes Optional numeric vector of between-group sizes
#'   (must sum to \code{S}).
#' @param thresh Numeric. Convergence threshold. Default \code{5e-3}.
#' @param max_iter Integer. Iterations per restart. Default \code{3e4}.
#' @param init_temp Numeric. Initial SA temperature. Default \code{1e-3}.
#' @param cooling_rate Numeric in (0,1) or \code{NULL} (auto). Default \code{NULL}.
#' @param max_starts Integer. Number of restarts. Default \code{3}.
#' @param progress_mode Character: \code{"console"}, \code{"shiny"}, or
#'   \code{"off"}. Default \code{"console"}.
#'
#' @return An object of class \code{stats2data_aov}, a list with components \code{best_objective},
#'   \code{data}, \code{inputs}, \code{adjusted_targets}, and
#'   \code{track_objective}.
#'
#' @examples
#' \donttest{
#' # 2 x 2 between-subjects design, 10 subjects per cell
#' res <- optim_aov(
#'   S                  = 40,
#'   levels             = c(2, 2),
#'   target_group_means = c(5.1, 4.2, 6.3, 5.0),
#'   target_f_list      = list(effect  = c("Factor1", "Factor2", "Factor1:Factor2"),
#'                             F_value = c(5.0, 6.05, 0.2)),
#'   integer            = FALSE,
#'   range              = c(0, 10),
#'   formula            = "outcome ~ Factor1 * Factor2",
#'   factor_type        = c("between", "between"),
#'   progress_mode      = "off"
#' )
#' summary(res)
#' plot_fit(res, standardised = FALSE)
#' }
#'
#' @export
optim_aov <- function(
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
    max_iter = 3e4,
    init_temp = 1e-3,
    cooling_rate = NULL,
    max_starts = 3,
    progress_mode = "console"
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

  # safeguard contrasts
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
  if (integer) range <- c(ceiling(range[1]), floor(range[2]))

  targets <- aov_targets(target_group_means, target_F, group_sizes,
                         effect_names = target_f_list$effect, levels, factor_type,
                         integer, thresh,
                          ID, factor_mat, group_idx, structure, formula_internal,
                         range = range)
  adjusted_group_means <- targets$adjusted_means
  target_F     <- targets$adjusted_F
  numerator_MS <- targets$numerator_MS
  if (sum(numerator_MS * target_F) <= 0)
    stop("The F targets carry no information on the within-cell variance ",
         "(every effect has F = 0 or equal cell means); nothing to optimise.")

  # candidate initialization
  # ballpark within-cell SD
  init_sd <- sqrt(sum(numerator_MS^2) / sum(numerator_MS * target_F))

  vec_cell <- function(j) {
    n_j  <- group_sizes[j]
    mu_j <- adjusted_group_means[j]
     S_j  <- round(n_j * mu_j)
    sd_range_int <- sd_range_integer(S_j, n_j, x_min = range[1], x_max = range[2],
                                     sample_sd = TRUE)

    if (integer) {
      if (sd_range_int[2] == 0) return(rep(mu_j, n_j))
      sd_j <- min(init_sd, sd_range_int[2])
      sd_j <- max(sd_j, sd_range_int[1])

      m_prec <- max(2L, floor(log10(n_j)) + 1L)
      mu_in  <- round(mu_j, m_prec)

      #     precision that rsprite2 accepts for this pair
      for (sd_prec in 1:10) {
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
    x <- do.call(optim_vec, c(
      list(N = n_j, range = range, integer = integer, init_temp = 1e-3,
           cooling_rate = NULL, max_starts = 1, progress_mode = "off"),
      vec_args))$data$y
    x[sample.int(n_j)] # permute
  }

  current_candidate <- numeric(length(group_ids))
  for (j in seq_along(group_idx)) {
    current_candidate[group_idx[[j]]] <- vec_cell(j)
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
      tab   <- fit$anova_table
      F_obs <- tab$F[match(target_f_list$effect, trimws(rownames(tab)))]
      sqrt(mean((F_obs - target_F)^2))
    }
  }

  # Move directives
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
        }
        # escape (integer, within/mixed designs only)
        if (integer && structure$has_within && i %% 100L == 0L) {
          idx <- group_idx[[sample.int(length(group_idx), 1L)]]
          if (length(idx) >= 2L) {
            sel <- idx[sample.int(length(idx), 2L)]
            if (current_candidate[sel[1]] != current_candidate[sel[2]]) {
               esc_candidate <- current_candidate
              esc_candidate[sel] <- esc_candidate[rev(sel)]
              esc_error <- objective(esc_candidate)
              if (is.finite(esc_error)) {
                current_candidate <- esc_candidate
                current_error     <- esc_error
                if (esc_error < best_objective) {
                  best_objective <- esc_error
                  best_candidate <- esc_candidate
                }
              }
            }
          }
        }

        temp <- temp * cooling_rate
        global_iter <- global_iter + 1L
        track_objective[global_iter] <- best_objective
        if (global_iter %% pb_interval == 0) p()
        if (is.finite(best_objective) && best_objective < thresh) break
      }
      current_candidate <- best_candidate
      current_error <- best_objective
      if (is.finite(best_objective) && best_objective < thresh) break
      temp <- init_temp
    }
  }, handlers = handler)

  track_objective <- track_objective[seq_len(global_iter)]

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
