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

# search move for continous data in descriptive module
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

  if (increaseSD) {
    inc_try <- inc_idx[candidate[inc_idx] > candidate[i_dec]]
  } else {
    inc_try <- inc_idx[candidate[inc_idx] < candidate[i_dec]]
  }
  if (length(inc_try) > 0) inc_idx <- inc_try

  if (length(inc_idx) == 0) return(candidate)
  i_inc <- sample(inc_idx, 1)

   max_dec <- candidate[i_dec] - lower_bound
  max_inc <- upper_bound - candidate[i_inc]

  if (increaseSD) {
    sd_gap <- abs(target_sd - current_sd)
     scaled_cap <- sd_gap * sqrt(n - 1)
    max_delta <- min(max_dec, max_inc, scaled_cap)
  } else {
     thresh <- (candidate[i_dec] - candidate[i_inc]) / 2
    max_delta <- min(max_dec, max_inc, thresh)
  }

  if (max_delta <= 0) return(candidate)

  delta <- stats::runif(1, min = 0, max = max_delta) # continuous
   candidate[i_dec] <- candidate[i_dec] - delta
  candidate[i_inc] <- candidate[i_inc] + delta

  candidate
}


# integer sd range
sd_range_integer <- function(S, n, x_min, x_max, sample_sd = TRUE) {
  S <- round(S)
  if (S < n * x_min || S > n * x_max) stop("`S` is not attainable in [x_min, x_max].")
  to_sd <- function(Q) {
    ss <- Q - S^2 / n
    if (sample_sd) sqrt(ss / (n - 1)) else sqrt(ss / n)
  }
   a <- S %/% n
   b <- S - n * a
  Q_min <- (n - b) * a^2 + b * (a + 1)^2
  if (x_max == x_min) return(c(min = 0, max = 0))
  D <- x_max - x_min
   E <- S - n * x_min
   k <- E %/% D
  r <- E %% D
  Q_max <- k * x_max^2 + (r > 0) * (x_min + r)^2 + (n - k - (r > 0)) * x_min^2
  c(min = to_sd(Q_min), max = to_sd(Q_max))
}


# generate continuous candidate vector
sprite_start_vector_cont <- function(tMean, n, range, thresh) {
  scaleMin <- range[1]
  scaleMax <- range[2]

  if (scaleMin >= scaleMax) stop("range[1] must be < range[2].")
  if (tMean < scaleMin || tMean > scaleMax) stop("Target mean is outside the allowable range.")
  if (n < 2) stop("n must be >= 2.")

  half_width <- min(tMean - scaleMin, scaleMax - tMean)
  vec <- stats::runif(n, min = tMean - half_width, max = tMean + half_width)

  W <- scaleMax - scaleMin
   step_floor <- max(n * thresh, .Machine$double.eps)
  max_loops  <- min(ceiling(W * n / step_floor) * 100, 1e7)

   iter <- 0L
  cMean <- mean(vec)

  while (abs(cMean - tMean) > thresh && iter < max_loops) {
    iter <- iter + 1L
    cMean <- mean(vec)
     if (abs(cMean - tMean) < thresh) break

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

  # random direction (i could improve this)
  if (stats::runif(1) < 0.5) {
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
    delta <- stats::runif(1, 0, max_delta)
  }

  candidate[dec_idx] <- candidate[dec_idx] - delta
   candidate[inc_idx] <- candidate[inc_idx] + delta

  candidate
}

# ANOVA within stratum move
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
    chosen <- vector("list", K)
    for (j in seq_len(K)) {
      chosen[[j]] <- if (j %in% W) sort(sample.int(wl[j], 2L)) else seq_len(wl[j])
    }

    in_sub <- rep(TRUE, nrow(wg))
    for (j in seq_len(K)) in_sub <- in_sub & (wg[, j] %in% chosen[[j]])
     sub_pos <- which(in_sub)

     sign_c <- rep(1L, length(sub_pos))
    for (j in W) {
      sign_c <- sign_c * ifelse(wg[sub_pos, j] == chosen[[j]][1], 1L, -1L)
    }

    pair <- sample(subs, 2)
    idx1 <- structure$subject_indices[[pair[1]]][sub_pos]
     idx2 <- structure$subject_indices[[pair[2]]][sub_pos]

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

# compute consistent aov targets
aov_targets <- function(target_group_means, target_F, group_sizes,
                        effect_names, levels, factor_type,
                        integer, thresh,
                        ID, factor_mat, group_idx, structure, formula,
                        range = c(-Inf, Inf)) {

  n_obs <- length(ID)

  # safety margin inside the thresh
  marg <- if (thresh > 0) {
     pmin(thresh * 1e-6 + 1e-9 * pmax(1, abs(target_group_means)), thresh / 2)
  } else {
    rep(0, length(target_group_means))
  }

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

  # error components
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

  # scoring
  # integer
  score_mu_int <- function(mu) {
    ms  <- ms_from_means(mu)
    s   <- strata$between
    if (length(s) < 2L) return(0)
    S   <- round(mu * group_sizes)
     C   <- sum(S^2 / group_sizes)
    dfe <- n_obs - length(group_sizes)
    parity <- sum(S) %% 2
    M0  <- sum(ms[s]^2) / sum(ms[s] * target_F[s])
    if (!is.finite(M0) || M0 <= 0) return(sum(target_F[s]^2))
    Q0  <- M0 * dfe + C
     Qs  <- floor(Q0) + (-3:3)
    Qs  <- Qs[Qs %% 2 == parity]
    Ms  <- (Qs - C) / dfe
     Ms  <- Ms[Ms > 0]
    if (!length(Ms)) return(sum(target_F[s]^2))
    min(sapply(Ms, function(M) sum((ms[s] / M - target_F[s])^2)))
  }

  # continuous
  score_mu <- function(mu) {
    ms <- ms_from_means(mu)
    f_dev <- 0
    for (s in strata) {
      if (length(s) < 2L) next
      den <- sum(ms[s]^2)
       implied_F <- if (den > 0) ms[s] * sum(ms[s] * target_F[s]) / den else 0 * ms[s]
      f_dev <- f_dev + sum((implied_F - target_F[s])^2)
    }

    f_dev
  }

  if (integer) {

    score_cand <- if (!structure$has_within) score_mu_int else score_mu
    # enumerate achievable means
    cands <- lapply(seq_along(target_group_means), function(k) {
      n_k <- group_sizes[k]; mu_k <- target_group_means[k]
      tol <- 1e-9 * max(1, abs(mu_k) * n_k)
       lo  <- ceiling((mu_k - thresh) * n_k - tol)
      hi  <- floor((mu_k + thresh) * n_k + tol)
      lo  <- max(lo, ceiling(range[1] * n_k - tol))
       hi  <- min(hi, floor(range[2] * n_k + tol))
      if (lo > hi) {
        warning("Cell ", k, ": no integer mean within thresh.")
        return(round(mu_k * n_k) / n_k)
      }
      (lo:hi) / n_k
    })
    grid <- expand.grid(cands)
    if (nrow(grid) > 200) {
      grid <- grid[round(seq(1, nrow(grid), length.out = 500)),]
    }
    best_score <- Inf
     best_md    <- Inf
    best_means <- target_group_means
    for (r in seq_len(nrow(grid))) {
      mu <- as.numeric(grid[r, ])
       sc <- score_cand(mu)
      md <- sum((mu - target_group_means)^2)
      if (sc < best_score || (sc == best_score && md < best_md)) {
        best_score <- sc
         best_md    <- md
        best_means <- mu
      }
    }
    adjusted_means <- best_means
    numerator_MS   <- ms_from_means(best_means)

  } else {
    lower <- pmax(target_group_means - (thresh - marg), range[1])
    upper <- pmin(target_group_means + (thresh - marg), range[2])
     lower <- pmin(lower, upper)
    free  <- upper > lower
    par   <- pmin(pmax(target_group_means, lower), upper)
    if (any(free)) {
      opt <- stats::optim(
        par    = par[free],
        fn     = function(p) { mu <- par; mu[free] <- p; score_mu(mu) },
        method = "L-BFGS-B",
        lower  = lower[free],
         upper  = upper[free],
        control = list(ndeps = rep(max(thresh, 1e-8) * 1e-4, sum(free)))
      )
      par[free] <- opt$par
    }
    adjusted_means <- par
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

# stats2data: internal checks for SPRITE input precision
# number of decimals of each value (NA for missing values)
n_decimals <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (is.na(v)) return(NA_integer_)
    for (d in 0:10) if (abs(v - round(v, d)) < 1e-9) return(as.integer(d))
    10L
  }, integer(1), USE.NAMES = FALSE)
}

# input check
check_sprite_prec <- function(target_mean, target_sd, integer, range, sprite_prec) {
  n_var <- length(target_mean)
  if (is.null(sprite_prec) || n_var == 0L) return(invisible(TRUE))
  integer <- rep_len(as.logical(integer), n_var)
  rng     <- if (is.matrix(range)) range else matrix(range, nrow = 2, ncol = n_var)
  sprite  <- integer & (rng[2, ] - rng[1, ] > 1)
  if (!any(sprite)) return(invisible(TRUE))

  vars <- names(target_mean)
  d_mean <- n_decimals(target_mean)
  d_sd   <- n_decimals(target_sd)
  bad_mean <- which(sprite & !is.na(d_mean) & d_mean > sprite_prec[1])
  bad_sd   <- which(sprite & !is.na(d_sd)   & d_sd   > sprite_prec[2])
  if (length(bad_mean) == 0L && length(bad_sd) == 0L) return(invisible(TRUE))

  describe <- function(i, x, what) {
    if (length(i)) paste0(what, " of ", paste0("'", vars[i], "' (", x[i], ")", collapse = ", "))
  }
  needed <- c(max(sprite_prec[1], d_mean[sprite], na.rm = TRUE),
              max(sprite_prec[2], d_sd[sprite],   na.rm = TRUE))
  stop(
    "`sprite_prec = c(", sprite_prec[1], ", ", sprite_prec[2], ")` has fewer decimals than the ",
    paste(c(describe(bad_mean, target_mean, "mean"), describe(bad_sd, target_sd, "SD")),
          collapse = " and the "),
    ". SPRITE does not round the targets: set `sprite_prec = c(", needed[1], ", ", needed[2],
    ")` or round the targets to the reported decimals.",
    call. = FALSE
  )
}
