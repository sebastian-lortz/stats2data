#' Internal helper functions for stats2data package
#'
#' @description Utility functions for data handling and calculations
#'
#' @return Various helper outputs
#'
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @importFrom stats aov as.formula coef formula lm model.frame reformulate resid sd setNames terms
#' @importFrom utils combn head str write.csv
#' @noRd
NULL

# count decimals of targets
count_decimals <- function(vec, min_decimals = 0) {
  sapply(as.character(vec), function(x) {
    if (grepl("\\.", x)) {
      parts <- strsplit(x, "\\.", fixed = FALSE)[[1]]
      max(nchar(parts[2]), min_decimals)
    } else {
      min_decimals
    }
  })
}


# extract the positions of terms in the design matrix to match in C++
get_design <- function(candidate, reg_equation, terms_obj) {
 p         <- ncol(candidate)

  main_names <- colnames(candidate)
  if (is.null(main_names)) {
    stop("Candidate predictors must have column names.")
  }

  interaction_names <- c()
  if (p > 1) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        interaction_names <- c(interaction_names, paste0(main_names[i], ":", main_names[j]))
      }
    }
  }

  full_names <- c("(Intercept)", main_names, interaction_names)
  target_names <- c("(Intercept)", labels(terms_obj))

  positions <- vapply(target_names, function(nm) {
    pos <- match(nm, full_names)
    if (is.na(pos) && grepl(":", nm)) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      rev_nm <- paste(parts[2], parts[1], sep = ":")
      pos <- match(rev_nm, full_names)
    }
    if (is.na(pos)) stop("Term not in design: ", nm)
    pos
  }, integer(1))

  return(list(
    target_names = target_names,
    full_names = full_names,
    positions = positions
  ))
}

# heuristic move for continous data in descriptive module
heuristic_move_cont <- function(candidate, target_sd, range) {
  lower_bound <- range[1]
  upper_bound <- range[2]
  n <- length(candidate)
  current_sd <- stats::sd(candidate)
  increaseSD <- (current_sd < target_sd)

  current_min <- min(candidate)
  current_max <- max(candidate)

  dec_idx <- which(candidate > lower_bound)
  if (length(dec_idx) == 0) return(candidate)
  if (increaseSD) {
    dec_try <- dec_idx[candidate[dec_idx] < current_max]
  } else {
    dec_try <- dec_idx[candidate[dec_idx] > current_min]
  }
  if (length(dec_try) > 0) dec_idx <- dec_try
  i_dec <- sample(dec_idx, 1)

  inc_idx <- which(candidate < upper_bound & seq_along(candidate) != i_dec)

  ## --- minimal bidirectional adjustment ---
  if (increaseSD) {
    # Increasing SD: prefer incrementing something larger (push apart)
    inc_try <- inc_idx[candidate[inc_idx] > candidate[i_dec]]
  } else {
    # Decreasing SD: prefer incrementing something smaller (pull together)
    inc_try <- inc_idx[candidate[inc_idx] < candidate[i_dec]]
  }
  if (length(inc_try) > 0) inc_idx <- inc_try

  if (length(inc_idx) == 0) return(candidate)
  i_inc <- sample(inc_idx, 1)

  max_dec <- candidate[i_dec] - lower_bound
  max_inc <- upper_bound - candidate[i_inc]

  if (increaseSD) {
    # Scale cap by sqrt(n) - this is the key fix
    sd_gap <- abs(target_sd - current_sd)
    scaled_cap <- sd_gap * sqrt(n - 1)
    max_delta <- min(max_dec, max_inc, scaled_cap)
  } else {
    thresh <- (candidate[i_dec] - candidate[i_inc]) / 2
    max_delta <- min(max_dec, max_inc, thresh)
  }

  if (max_delta <= 0) return(candidate)

  delta <- runif(1, min = 0, max = max_delta) # continuous
  candidate[i_dec] <- candidate[i_dec] - delta
  candidate[i_inc] <- candidate[i_inc] + delta

  candidate
}


# generate integer candidate vector (modified Sprite)
sprite_start_vector <- function(tMean, n, range, thresh) {
  scaleMin <- range[1]
  scaleMax <- range[2]

  if (scaleMin >= scaleMax) stop("range[1] must be < range[2].")
  if (tMean < scaleMin || tMean > scaleMax) stop("Target mean is outside the allowable range.")
  if (n < 2) stop("n must be >= 2.")

  # random start around the mean
  half_width <- min(tMean - scaleMin, scaleMax - tMean)
  vec <- as.integer(runif(n, min = tMean - half_width, max = tMean + half_width))
  max_loops <- max(10000, n * (scaleMax - scaleMin + 1))


  for (i in seq_len(max_loops)) {
    cMean <- mean(vec)
    if (abs(cMean - tMean) < thresh) break

    delta <- 1 # Maybe this could be improved
    increaseMean <- (cMean < tMean)

    if (increaseMean) {
      can <- which(vec <= (scaleMax - delta))
      if (length(can)) {
        idx <- sample(can, 1)
        vec[idx] <- vec[idx] + delta
      }
      } else {
      can <- which(vec >= (scaleMin + delta))
      if (!length(can) && delta == 2) { delta <- 1; can <- which(vec >= (scaleMin + 1)) }
      if (length(can)) {
        idx <- sample(can, 1)
        vec[idx] <- vec[idx] - delta
      }
      }
  }

  vec
}

sprite_start_vector_cont <- function(tMean, n, range, thresh) {
  scaleMin <- range[1]
  scaleMax <- range[2]

  if (scaleMin >= scaleMax) stop("range[1] must be < range[2].")
  if (tMean < scaleMin || tMean > scaleMax) stop("Target mean is outside the allowable range.")
  if (n < 2) stop("n must be >= 2.")

  half_width <- min(tMean - scaleMin, scaleMax - tMean)
  vec <- runif(n, min = tMean - half_width, max = tMean + half_width)

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
  }

  vec
}

# ANOVA between subject move
move_between <- function(candidate, integer, structure, range) {
  lower_bound <- range[1]
  upper_bound <- range[2]

  bg <- sample(structure$between_group_labels, 1)
  subs <- structure$subjects_in_bgroup[[bg]]
  if (length(subs) < 2) return(candidate)

  pair <- sample(subs, 2)
  idx1 <- structure$subject_indices[[pair[1]]]
  idx2 <- structure$subject_indices[[pair[2]]]

  # Random direction (I could improve this)
  if (runif(1) < 0.5) {
    dec_idx <- idx1; inc_idx <- idx2
  } else {
    dec_idx <- idx2; inc_idx <- idx1
  }

  max_delta <- min(
    min(candidate[dec_idx] - lower_bound),
    min(upper_bound - candidate[inc_idx])
  )

  if (integer) {
    max_delta <- floor(max_delta)
    if (max_delta < 1L) return(candidate)
    delta <- sample.int(max_delta, 1)
  } else {
    if (max_delta <= 0) return(candidate)
    delta <- runif(1, 0, max_delta)
  }

  candidate[dec_idx] <- candidate[dec_idx] - delta
  candidate[inc_idx] <- candidate[inc_idx] + delta

  candidate
}

# ANOVA within paired
move_within_paired <- function(candidate, integer, structure, range) {
  b <- structure$n_within_levels
  if (b < 2) return(candidate)

  lower <- range[1]; upper <- range[2]
  bg   <- sample(structure$between_group_labels, 1)
  subs <- structure$subjects_in_bgroup[[bg]]
  if (length(subs) < 2) return(candidate)

  for (attempt in 1:50) {
    pair <- sample(subs, 2)
    idx1 <- structure$subject_indices[[pair[1]]]
    idx2 <- structure$subject_indices[[pair[2]]]
    cond <- sample.int(b, 2)
    c1 <- cond[1]; c2 <- cond[2]

    # random direction (I could improve this later)
    s1 <- if (runif(1) < 0.5) -1 else 1

    if (s1 == -1) {
      max_delta <- min(
        candidate[idx1[c1]] - lower,
        upper - candidate[idx1[c2]],
        upper - candidate[idx2[c1]],
        candidate[idx2[c2]] - lower
      )
    } else {
      max_delta <- min(
        upper - candidate[idx1[c1]],
        candidate[idx1[c2]] - lower,
        candidate[idx2[c1]] - lower,
        upper - candidate[idx2[c2]]
      )
    }

    if (integer) max_delta <- floor(max_delta)
    if (max_delta >= 1L || (!integer && max_delta > 1e-8)) break
  }

  if (integer && max_delta < 1L) return(candidate)
  if (!integer && max_delta <= 1e-8) return(candidate)

  if (integer) {
    delta <- sample.int(max_delta, 1)
  } else {
    delta <- runif(1, 0, max_delta)
  }

  candidate[idx1[c1]] <- candidate[idx1[c1]] + s1 * delta
  candidate[idx1[c2]] <- candidate[idx1[c2]] - s1 * delta
  candidate[idx2[c1]] <- candidate[idx2[c1]] - s1 * delta
  candidate[idx2[c2]] <- candidate[idx2[c2]] + s1 * delta

  candidate
}

# ANOVA within interaction move
move_within_interaction <- function(candidate, integer, structure, range) {
  within_levels <- structure$within_levels
  if (length(within_levels) < 2) return(candidate)
  if (any(within_levels < 2)) return(candidate)

  lower <- range[1]; upper <- range[2]

  wg_grid <- expand.grid(lapply(within_levels, seq_len))
  wg_grid <- wg_grid[do.call(order, wg_grid), , drop = FALSE]

  bg   <- sample(structure$between_group_labels, 1)
  subs <- structure$subjects_in_bgroup[[bg]]
  if (length(subs) < 2) return(candidate)

  for (attempt in 1:50) {
    l1 <- sort(sample.int(within_levels[1], 2))
    l2 <- sort(sample.int(within_levels[2], 2))

    pos <- c(
      which(wg_grid[,1] == l1[1] & wg_grid[,2] == l2[1]),
      which(wg_grid[,1] == l1[1] & wg_grid[,2] == l2[2]),
      which(wg_grid[,1] == l1[2] & wg_grid[,2] == l2[1]),
      which(wg_grid[,1] == l1[2] & wg_grid[,2] == l2[2])
    )

    pair <- sample(subs, 2)
    idx1 <- structure$subject_indices[[pair[1]]]
    idx2 <- structure$subject_indices[[pair[2]]]

    s1 <- if (runif(1) < 0.5) 1 else -1

    i1_plus  <- idx1[c(pos[1], pos[4])]
    i1_minus <- idx1[c(pos[2], pos[3])]
    i2_plus  <- idx2[c(pos[2], pos[3])]
    i2_minus <- idx2[c(pos[1], pos[4])]

    if (s1 == 1) {
      max_delta <- min(
        min(upper - candidate[i1_plus]),
        min(candidate[i1_minus] - lower),
        min(upper - candidate[i2_plus]),
        min(candidate[i2_minus] - lower)
      )
    } else {
      max_delta <- min(
        min(candidate[i1_plus] - lower),
        min(upper - candidate[i1_minus]),
        min(candidate[i2_plus] - lower),
        min(upper - candidate[i2_minus])
      )
    }

    if (integer) max_delta <- floor(max_delta)
    #cat("Attempt", attempt, "max_delta =", max_delta, "\n")
    if (max_delta >= 1L || (!integer && max_delta > 1e-8)) break
  }

  if (integer && max_delta < 1L) return(candidate)
  if (!integer && max_delta <= 1e-8) return(candidate)

  delta <- if (integer) sample.int(max_delta, 1) else runif(1, 0, max_delta)

  candidate[i1_plus]  <- candidate[i1_plus]  + s1 * delta
  candidate[i1_minus] <- candidate[i1_minus] - s1 * delta
  candidate[i2_plus]  <- candidate[i2_plus]  + s1 * delta
  candidate[i2_minus] <- candidate[i2_minus] - s1 * delta

  candidate
}


# build ANOVA structure
build_aov_structure <- function(N, levels, factor_type, subgroup_sizes = NULL) {

  n_factors <- length(levels)
  if (length(factor_type) != n_factors)
    stop("'levels' and 'factor_type' must have the same length.")

  between_idx <- which(factor_type == "between")
  within_idx  <- which(factor_type == "within")
  has_between <- length(between_idx) > 0L
  has_within  <- length(within_idx)  > 0L

  n_within  <- if (has_within)  prod(levels[within_idx])  else 1L
  n_between <- if (has_between) prod(levels[between_idx]) else 1L

  # subjects per between group
  if (has_between) {
    if (!is.null(subgroup_sizes)) {
      if (length(subgroup_sizes) != n_between)
        stop("subgroup_sizes length must equal ", n_between, " (between-group combinations).")
      spg <- subgroup_sizes
    } else {
      base <- N %/% n_between
      rem  <- N %%  n_between
      spg  <- rep(base, n_between)
      if (rem > 0L) spg[seq_len(rem)] <- spg[seq_len(rem)] + 1L
    }
    N_subjects <- sum(spg)
  } else {
    spg <- N
    N_subjects <- N
  }

  # between group design rows
  if (has_between) {
    bg_grid <- expand.grid(lapply(levels[between_idx], seq_len))
    bg_grid <- bg_grid[do.call(order, bg_grid), , drop = FALSE]
    bg_expanded <- bg_grid[rep(seq_len(n_between), spg), , drop = FALSE]
  }

  # within condition grid
  if (has_within) {
    wg_grid <- expand.grid(lapply(levels[within_idx], seq_len))
    wg_grid <- wg_grid[do.call(order, wg_grid), , drop = FALSE]
  }

  # assemble factor matrix
  n_obs <- N_subjects * n_within
  fmat  <- matrix(NA_integer_, nrow = n_obs, ncol = n_factors)

  if (has_between) {
    for (j in seq_along(between_idx))
      fmat[, between_idx[j]] <- rep(bg_expanded[[j]], each = n_within)
  }
  if (has_within) {
    for (j in seq_along(within_idx))
      fmat[, within_idx[j]] <- rep(wg_grid[[j]], times = N_subjects)
  }

  ID <- rep(seq_len(N_subjects), each = n_within)

  # precompute move indices
  subject_indices <- split(seq_len(n_obs), ID)

  if (has_between) {
    bg_label <- apply(fmat[, between_idx, drop = FALSE], 1, paste, collapse = "_")
    bg_per_subject <- bg_label[seq(1, n_obs, by = n_within)]  # first obs per subject
    names(bg_per_subject) <- seq_len(N_subjects)
  } else {
    bg_per_subject <- setNames(rep("1", N_subjects), seq_len(N_subjects))
  }

  bg_labels        <- unique(bg_per_subject)
  subjects_in_bg   <- split(names(bg_per_subject), bg_per_subject)

  # data frame
  df <- data.frame(ID = ID, fmat)
  colnames(df)[-1] <- paste0("Factor", seq_len(n_factors))
  df[-1] <- lapply(df[-1], as.factor)

  list(
    df                   = df,
    N_subjects           = N_subjects,
    n_within_levels      = n_within,
    n_between_groups     = n_between,
    has_between          = has_between,
    has_within           = has_within,
    within_levels        = if (has_within) levels[within_idx] else integer(0),
    within_idx           = within_idx,                                 # *
    wg_grid              = if (has_within) wg_grid else NULL,          # *
    subject_indices      = subject_indices,
    bg_per_subject       = bg_per_subject,
    between_group_labels = bg_labels,
    subjects_in_bgroup   = subjects_in_bg,
    n_per_bgroup         = sapply(subjects_in_bg, length)
  )
}

# compute Mean Squares for ANOVA
compute_numerator_MS <- function(x, ID, factor_mat, structure, formula,
                                 effect_names) {
  if (structure$has_within) {
    dat <- data.frame(ID = ID, factor_mat, outcome = x)
    suppressMessages({
      fit <- afex::aov_car(
        formula = formula, data = dat,
        factorize = TRUE, type = 3
      )
    })
    tab <- fit$anova_table
    rn  <- trimws(rownames(tab))
    sapply(effect_names, function(e) {
      row <- which(rn == e)
      if (length(row) == 0) stop("Effect '", e, "' not found in ANOVA table.")
      tab[row, "F"] * tab[row, "MSE"]
    })
  } else {
    dat <- data.frame(factor_mat, outcome = x)
    fit <- stats::lm(formula, data = dat)
    tab <- car::Anova(fit, type = 3)
    rn  <- trimws(rownames(tab))
    sapply(effect_names, function(e) {
      row <- which(rn == e)
      if (length(row) == 0) stop("Effect '", e, "' not found in ANOVA table.")
      tab[row, "Sum Sq"] / tab[row, "Df"]
    })
  }
}





# compute MSE between
compute_MSE_between <- function(x, structure) {
  si     <- structure$subject_indices
  sib    <- structure$subjects_in_bgroup
  N_subj <- structure$N_subjects
  n_bg   <- structure$n_between_groups
  b      <- structure$n_within_levels

  subj_means <- sapply(si, function(idx) mean(x[idx]))

  ss <- 0
  for (bg in structure$between_group_labels) {
    subs <- sib[[bg]]
    sm   <- subj_means[subs]
    ss   <- ss + sum((sm - mean(sm))^2)
  }

  b * ss / (N_subj - n_bg)
}

# derive consistent aov targets
aov_targets <- function(target_group_means, target_F, group_sizes,
                        effect_names, levels, factor_type,
                        integer, thresh,
                        ID, factor_mat, group_idx, structure, formula) {

  n_obs <- length(ID)

  # MS from cell means
  ms_from_means <- function(mu) {
    x <- numeric(n_obs)
    for (j in seq_along(group_idx)) {
      idx   <- group_idx[[j]]
      n_j   <- length(idx)
      noise <- (seq_len(n_j) - (n_j + 1) / 2)* 1e-4
      if (j %% 2 == 0) noise <- rev(noise)
      x[idx] <- mu[j] + noise
    }
    ms <- compute_numerator_MS(x, ID, factor_mat, structure, formula, effect_names)
    ms[is.nan(ms)] <- 0
    ms
  }

  # error strata
  within_names <- paste0("Factor", which(factor_type == "within"))
  uses_within  <- sapply(effect_names, function(e)
    any(unlist(strsplit(e, ":")) %in% within_names))

  strata <- list()
  if (any(!uses_within))  strata$between <- which(!uses_within)
  if (any(uses_within)) {
    within_composition <- sapply(effect_names[uses_within], function(e) {
      p <- intersect(unlist(strsplit(e, ":")), within_names)
      paste(p[order(as.integer(sub("Factor", "", p)))], collapse = ":")
    })
    for (key in unique(within_composition)) {
      strata[[key]] <- which(uses_within)[within_composition == key]
    }
  }

  # how well can the optimizer hit target_F given these means?
  score_mu <- function(mu) {
    ms <- ms_from_means(mu)

    # for effects sharing an error term, the best the optimizer can do
    # is find MSE* that minimises sum((ms/MSE - F)^2) within each stratum
    f_dev <- 0
    for (s in strata) {
      if (length(s) < 2L) next
      # optimal MSE
      MSE_star <- sum(ms[s]^2) / sum(ms[s] * target_F[s])
      implied_F <- ms[s] / MSE_star
      f_dev <- f_dev + sum((implied_F - target_F[s])^2)
    }

    # combined: mean displacement + best-case F residual
    mean_dev <- sum((mu - target_group_means)^2)
    mean_dev + f_dev
  }

  if (integer) {
    # enumerate achievable means
    cands <- lapply(seq_along(target_group_means), function(k) {
      n_k <- group_sizes[k]; mu_k <- target_group_means[k]
      lo  <- ceiling((mu_k - thresh) * n_k)
      hi  <- floor((mu_k + thresh) * n_k)
      if (lo > hi) {
        warning("Cell ", k, ": no integer mean within thresh.")
        return(round(mu_k * n_k) / n_k)
      }
      (lo:hi) / n_k
    })
    grid <- expand.grid(cands)
    if (nrow(grid) > 200) {
      grid <- grid[round(seq(1, nrow(grid), length.out = 200)),]
    }
    best_score <- Inf
    best_means <- target_group_means
    for (r in seq_len(nrow(grid))) {
      mu <- as.numeric(grid[r, ])
      sc <- score_mu(mu)
      if (sc < best_score) {
        best_score <- sc
        best_means <- mu
      }
    }
    adjusted_means <- best_means
    numerator_MS   <- ms_from_means(best_means)

  } else {
    # continuous: optimise within thresh
    lower <- target_group_means - thresh
    upper <- target_group_means + thresh
    opt <- optim(
      par    = target_group_means,
      fn     = score_mu,
      method = "L-BFGS-B",
      lower  = lower,
      upper  = upper
    )
    adjusted_means <- opt$par
    numerator_MS   <- ms_from_means(opt$par)
  }

  names(numerator_MS) <- effect_names

  list(
    adjusted_means     = adjusted_means,
    adjusted_F         = target_F,
    numerator_MS       = numerator_MS,
    between_effect_idx = if (any(!uses_within)) which(!uses_within) else integer(0),
    within_effect_idx  = if (any(uses_within))  which(uses_within)  else integer(0)
  )
}


# re order the target cor according to input order
remap_target_cor <- function(target_cor, sim_data, vars_new) {
  vars_old <- colnames(sim_data)
  p_old    <- length(vars_old)
  if (length(target_cor) != p_old*(p_old-1)/2) {
    stop("target_cor length (", length(target_cor),
         ") does not match ncol(sim_data) = ", p_old)
  }

  mat_old <- matrix(NA_real_, p_old, p_old,
                    dimnames = list(vars_old, vars_old))
  mat_old[upper.tri(mat_old)] <- target_cor
  mat_old[lower.tri(mat_old)] <- t(mat_old)[lower.tri(mat_old)]
  diag(mat_old) <- 1

  if (!all(vars_new %in% vars_old)) {
    stop("Some vars_new not found in sim_data: ",
         paste(setdiff(vars_new, vars_old), collapse = ", "))
  }
  mat_new <- mat_old[vars_new, vars_new]

  as.vector(mat_new[upper.tri(mat_new)])
}

move_within_stratum <- function(candidate, W, integer, structure, range) {
  if (!structure$has_within) return(candidate)
  wl <- structure$within_levels
  K  <- length(wl)
  if (length(W) < 1L || any(W < 1L | W > K)) return(candidate)
  if (any(wl[W] < 2L)) return(candidate)

  lower <- range[1]; upper <- range[2]
  wg    <- structure$wg_grid
  bg    <- sample(structure$between_group_labels, 1)
  subs  <- structure$subjects_in_bgroup[[bg]]
  if (length(subs) < 2L) return(candidate)

  for (attempt in seq_len(50L)) {
    # 1. choose two levels per factor in W; keep all levels for j not in W
    chosen <- vector("list", K)
    for (j in seq_len(K)) {
      chosen[[j]] <- if (j %in% W) sort(sample.int(wl[j], 2L)) else seq_len(wl[j])
    }

    # 2. sub-cube cells (positions in wg_grid order)
    in_sub <- rep(TRUE, nrow(wg))
    for (j in seq_len(K)) in_sub <- in_sub & (wg[, j] %in% chosen[[j]])
    sub_pos <- which(in_sub)

    # 3. signed contrast pattern over the sub-cube
    sign_c <- rep(1L, length(sub_pos))
    for (j in W) {
      sign_c <- sign_c * ifelse(wg[sub_pos, j] == chosen[[j]][1], 1L, -1L)
    }

    # 4. two subjects from the same between-group
    pair <- sample(subs, 2)
    idx1 <- structure$subject_indices[[pair[1]]][sub_pos]
    idx2 <- structure$subject_indices[[pair[2]]][sub_pos]

    # 5. headroom in candidate; subject 1 receives +sign*delta, subject 2 -sign*delta
    head1 <- ifelse(sign_c == 1L, upper - candidate[idx1], candidate[idx1] - lower)
    head2 <- ifelse(sign_c == 1L, candidate[idx2] - lower, upper - candidate[idx2])
    max_delta <- min(head1, head2)

    if (integer) max_delta <- floor(max_delta)
    if (max_delta >= 1L || (!integer && max_delta > 1e-8)) break
  }
  if (integer && max_delta < 1L) return(candidate)
  if (!integer && max_delta <= 1e-8) return(candidate)

  delta <- if (integer) sample.int(max_delta, 1L) else stats::runif(1, 0, max_delta)

  candidate[idx1] <- candidate[idx1] + sign_c * delta
  candidate[idx2] <- candidate[idx2] - sign_c * delta
  candidate
}

