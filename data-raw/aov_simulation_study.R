### Simulation Study from the manuscript
# HPC
# 41 hours
# 108 cores
# 40GB

#### Configs ####
options(repos = c(CRAN = "https://ftp.belnet.be/mirror/CRAN/"))
HPC <- TRUE

# Install package
if (HPC) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  deps <- remotes::dev_package_deps("/home4/p310779/stats2data", dependencies = TRUE)
  missing <- deps$package[!sapply(deps$package, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)

  system("rm -f /home4/p310779/stats2data/src/*.o /home4/p310779/stats2data/src/*.so")
  install.packages("/home4/p310779/stats2data", repos = NULL, type = "source")
}

# set root_dir
root_dir <- if (HPC) "/home4/p310779/stats2data" else "/Users/lortz/Desktop/PhD/Research/simdata/stats2data"

save_dir <- file.path(root_dir, "data-raw", "results")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

seed     <- 310779

# libs
library(stats2data)
library(ggplot2)
library(patchwork)
library(sn)
library(dplyr)
library(future)
library(future.apply)
library(afex)
library(progressr)
library(tidyr)

# backend
if (HPC) {
  plan(multisession, workers = availableCores()-5)
} else {
  plan(multisession, workers = 4L)
}


#### ANOVA Module ####
# Data generating function

data_gen <- function(type, pars) {
  switch(type,

         normal   = do.call(rsn, pars),
         uniform  = do.call(runif, pars),
         t = do.call(rt, pars),
         binomial = do.call(rbinom, pars),
         exp      = do.call(rexp, pars),
         gamma    = do.call(rgamma, pars),
         weibull     = do.call(rweibull, pars),
         chisq    = do.call(rchisq, pars),
         lnorm    = do.call(rlnorm, pars),
         stop("Unknown distribution type")
  )
}

# pars conditions
pars_conditions <- function(N) {
  list(
    normal = list(
      standard = list(n = N, xi = 0, omega = 1, alpha = 0),
      extreme1 = list(n = N, xi = 0, omega = 5, alpha = -5),
      extreme2 = list(n = N, xi = 100, omega = 10, alpha = 10)
    ),
    uniform = list(
      standard = list(n = N, min = 0, max = 10),
      extreme1 = list(n = N, min = -100, max = 100),
      extreme2 = list(n = N, min = 0, max = 1)
    ),
    t = list(
      standard = list(n = N, df = 10),
      extreme1 = list(n = N, df = 3),
      extreme2 = list(n = N, df = 50)
    ),
    binomial = list(
      standard = list(n = N, size = 10, prob = 0.5),
      extreme1 = list(n = N, size = 1000, prob = 0.01),
      extreme2 = list(n = N, size = 1000, prob = 0.99)
    ),
    exp = list(
      standard = list(n = N, rate = 1),
      extreme1 = list(n = N, rate = 0.1),
      extreme2 = list(n = N, rate = 0.01)
    ),
    gamma = list(
      standard = list(n = N, shape = 3, rate = 0.5),
      extreme1 = list(n = N, shape = 1, rate = 0.2),
      extreme2 = list(n = N, shape = 10, rate = 1)
    ),
    weibull = list(
      standard = list(n = N, shape = 2, scale = 5),
      extreme1 = list(n = N, shape = 0.5, scale = 1),
      extreme2 = list(n = N, shape = 5, scale = 10)
    ),
    chisq = list(
      standard = list(n = N, df = 5),
      extreme1 = list(n = N, df = 1),
      extreme2 = list(n = N, df = 100)
    ),
    lnorm = list(
      standard = list(n = N, meanlog = 0, sdlog = 1),
      extreme1 = list(n = N, meanlog = 0, sdlog = 2),
      extreme2 = list(n = N, meanlog = 5, sdlog = 0.1)
    )
  )
}

# Three fixed designs
aov_designs <- list(
  between = list(levels = c(2, 2), factor_type = c("between", "between"),
                 formula_afex  = as.formula("outcome ~ Factor1 * Factor2 + Error(ID)"),
                 formula_optim = as.formula("outcome ~ Factor1 * Factor2")),
  within  = list(levels = c(2, 2), factor_type = c("within", "within"),
                 formula_afex  = as.formula("outcome ~ 1 + Error(ID / (Factor1 * Factor2))"),
                 formula_optim = as.formula("outcome ~ 1 + Error(ID / (Factor1 * Factor2))")),
  mixed   = list(levels = c(2, 2), factor_type = c("between", "within"),
                 formula_afex  = as.formula("outcome ~ Factor1 + Error(ID / Factor2)"),
                 formula_optim = as.formula("outcome ~ Factor1 + Error(ID / Factor2)"))
)

# Sample distribution conditions (like lm_conditions)
aov_conditions <- function(S, n.cond, seed) {
  set.seed(seed)
  pars <- pars_conditions(S)
  dist_names <- names(pars)
  #cond_names <- c("standard", "extreme1", "extreme2")
  cond_names <- c("standard")

  make_one <- function() {
    cell_dists <- sample(dist_names, 4, replace = TRUE)
    cell_conds <- sample(cond_names, 4, replace = TRUE)

    cell_pars  <- lapply(seq_len(4), function(k)
      pars[[cell_dists[k]]][[cell_conds[k]]])

    pilot_sds <- sapply(1:4, function(k) {
      p <- cell_pars[[k]]
      p$n <- S
      sd(data_gen(cell_dists[k], p))
    })
    error_sd <- mean(pilot_sds)
    grand_mean <- round(runif(1, 5, 20), 2)
    # target_F   <- runif(3, 0.5, 10)
    target_F   <- runif(3, 0.1, 0.4)   # small to large effects
    list(
      cell_dists = cell_dists,
      cell_conds = cell_conds,
      cell_pars  = cell_pars,
      error_sd   = error_sd,
      grand_mean = grand_mean,
      target_F   = target_F
    )
  }
  setNames(
    replicate(n.cond, make_one(), simplify = FALSE),
    paste0("cond", 1:n.cond)
  )
}

# Generate data: for each condition × each design
sim_aov_data <- function(S, conds, icc = 0.3) {
  N_PER_CELL <- 75L
  CELL_F1    <- c(1, 1, 2, 2)
  CELL_F2    <- c(1, 2, 1, 2)

  setNames(lapply(names(conds), function(cond_name) {
    spec <- conds[[cond_name]]

    # mean-centred cell errors (preserves target cell means exactly)
    gen_cell_errors <- function(k, n) {
      p <- spec$cell_pars[[k]]; p$n <- n
      e <- data_gen(spec$cell_dists[k], p)
      e - mean(e)
    }

    # cell means from Cohen's f (design-agnostic). Effects in outcome units.
    eff <- spec$target_F * spec$error_sd
    cell_means <- spec$grand_mean + c(
      -eff[1] - eff[2] + eff[3],
      -eff[1] + eff[2] - eff[3],
      eff[1] - eff[2] - eff[3],
      eff[1] + eff[2] + eff[3]
    )

    # subject random-intercept SD calibrated to target ICC
    subject_sd <- sqrt(icc / (1 - icc)) * spec$error_sd

    setNames(lapply(names(aov_designs), function(des_name) {
      design <- aov_designs[[des_name]]
      ftype  <- design$factor_type
      has_w  <- any(ftype == "within")

      # for each cell k, identify the between-group (b) and within-group (w).
      # works for 2x2 between, within, and mixed (B = F1, W = F2)
      bg_of <- if (all(ftype == "between"))   function(k) k
      else if (all(ftype == "within")) function(k) 1L
      else                              function(k) CELL_F1[k]

      n_bgs <- if (all(ftype == "between")) 4L
      else if (all(ftype == "within")) 1L
      else 2L

      # allocate unique subject IDs per between-group; draw subject effects
      # once per subject (zero for purely between designs)
      subj_offset      <- 0L
      subjects_in_bg   <- vector("list", n_bgs)
      subj_effects_bg  <- vector("list", n_bgs)
      for (b in seq_len(n_bgs)) {
        subjects_in_bg[[b]]  <- subj_offset + seq_len(N_PER_CELL)
        subj_effects_bg[[b]] <- if (has_w) rnorm(N_PER_CELL, 0, subject_sd)
        else        rep(0, N_PER_CELL)
        subj_offset <- subj_offset + N_PER_CELL
      }

      dat <- do.call(rbind, lapply(1:4, function(k) {
        b <- bg_of(k)
        y <- cell_means[k] + subj_effects_bg[[b]] + gen_cell_errors(k, N_PER_CELL)
        data.frame(ID      = subjects_in_bg[[b]],
                   Factor1 = factor(CELL_F1[k]),
                   Factor2 = factor(CELL_F2[k]),
                   outcome = y)
      }))
      dat$ID <- factor(dat$ID)
      list(data = dat, design = design, cell_dists = spec$cell_dists)
    }), names(aov_designs))
  }), names(conds))
}

# extract aov targets
aov_extract_targets <- function(data_list, dec) {
  lapply(data_list, function(cond_designs) {
    lapply(cond_designs, function(x) {
      dat    <- x$data
      design <- x$design

      cell_order <- list(F1 = c(1, 1, 2, 2), F2 = c(1, 2, 1, 2))

      extract <- function(d) {
        cell_means <- sapply(1:4, function(k) {
          round(mean(d$outcome[d$Factor1 == cell_order$F1[k] &
                                 d$Factor2 == cell_order$F2[k]]), dec)
        })
        fit    <- afex::aov_car(design$formula_afex, data = d)
        f_vals <- round(fit$anova_table$F, dec)
        rng    <- c(floor(min(d$outcome) * 10^dec) / 10^dec,
                    ceiling(max(d$outcome) * 10^dec) / 10^dec)

        if (any(design$factor_type == "between")) {
          first_obs <- d[!duplicated(d$ID), ]
          b_names   <- c("Factor1", "Factor2")[design$factor_type == "between"]
          b_levels  <- design$levels[design$factor_type == "between"]
          bg_grid   <- expand.grid(lapply(b_levels, seq_len))
          bg_grid   <- bg_grid[do.call(order, bg_grid), , drop = FALSE]
          subgroup_sizes <- as.integer(sapply(1:nrow(bg_grid), function(r) {
            idx <- rep(TRUE, nrow(first_obs))
            for (j in seq_along(b_names)) idx <- idx & first_obs[[b_names[j]]] == bg_grid[r, j]
            sum(idx)
          }))
        } else {
          subgroup_sizes <- NULL
        }

        list(S = length(unique(d$ID)), levels = design$levels,
             subgroup_sizes = subgroup_sizes, factor_type = design$factor_type,
             group_means = cell_means, f_vals = f_vals,
             effect_names = rownames(fit$anova_table),
             formula = design$formula_optim, range = rng)
      }

      cont <- extract(dat)

      dat_int <- dat
      dat_int$outcome <- round(dat_int$outcome)
      degenerate <- any(sapply(1:4, function(k) {
        vals <- dat_int$outcome[dat_int$Factor1 == cell_order$F1[k] &
                                  dat_int$Factor2 == cell_order$F2[k]]
        length(unique(vals)) < 2
      }))
      int <- if (degenerate) NULL else extract(dat_int)

      list(cont = cont, int = int)
    })
  })
}

# optim aov
apply_optim_aov <- function(targets, tol) {
  lapply(targets, function(cond_targets) {
    lapply(cond_targets, function(targ) {
      run_one <- function(t, is_int) {
        res <- tryCatch(
          optim_aov(S = t$S, levels = t$levels,
                    target_group_means = t$group_means,
                    subgroup_sizes = t$subgroup_sizes,
                    target_f_list = list(effect = t$effect_names, F = t$f_vals),
                    integer = is_int, range = t$range, formula = t$formula,
                    factor_type = t$factor_type, thresh = tol,
                    progress_mode = "console"),
          error = function(e) list(best_error = Inf, status = "infeasible",
                                   message = conditionMessage(e))
        )
        if (!is.null(res$track_error)) {
          n_iter <- length(res$track_error)
          res$diag <- list(
            n_iter              = n_iter,
            err_at_init         = res$track_error[1],
            err_at_iter_100     = if (n_iter >= 100)  res$track_error[100]  else NA_real_,
            err_at_iter_1000    = if (n_iter >= 1000) res$track_error[1000] else NA_real_,
            err_final           = res$best_error,
            improvement_frac    = if (res$track_error[1] > 0)
              1 - res$best_error / res$track_error[1]
            else NA_real_,
            stalled_steps       = sum(diff(res$track_error) == 0),
            strict_improvements = sum(diff(res$track_error) < 0),
            adjusted_means      = res$adjusted_targets$group_means,
            target_means        = t$group_means,
            adjusted_F          = res$adjusted_targets$F_values,
            target_F            = t$f_vals,
            max_mean_drift      = max(abs(res$adjusted_targets$group_means - t$group_means))
          )
          res$track_error <- NULL
        }
        res
      }
      cont <- run_one(targ$cont, FALSE)
      int  <- if (is.null(targ$int)) list(best_error = NA, status = "degenerate")
      else run_one(targ$int, TRUE)
      list(cont = cont, int = int)
    })
  })
}
# check convergence aov
check_aov_conv <- function(res, tol) {
  do.call(rbind, lapply(names(res), function(cond) {
    do.call(rbind, lapply(names(res[[cond]]), function(des) {
      cont_err <- res[[cond]][[des]]$cont$best_error
      int_err  <- res[[cond]][[des]]$int$best_error
      data.frame(condition = cond, design = des,
                 cont_err = cont_err,
                 cont_conv = is.finite(cont_err) && cont_err < tol,
                 int_err = int_err,
                 int_conv = if (is.na(int_err)) NA else is.finite(int_err) && int_err < tol)
    }))
  }))
}

# check aov means
check_aov_means <- function(aov_mc, tol = 1e-4) {
  do.call(rbind, lapply(seq_along(aov_mc$full_reps), function(r) {
    rep <- aov_mc$full_reps[[r]]
    do.call(rbind, lapply(names(rep$results), function(cond) {
      do.call(rbind, lapply(names(rep$results[[cond]]), function(des) {
        do.call(rbind, lapply(c("cont", "int"), function(type) {
          res <- rep$results[[cond]][[des]][[type]]
          targ <- rep$targets[[cond]][[des]][[type]]
          if (is.null(res$data) || is.null(targ)) return(NULL)

          dat <- res$data
          target_means <- targ$group_means

          obs_means <- sapply(1:4, function(k) {
            f1 <- c(1, 1, 2, 2)[k]
            f2 <- c(1, 2, 1, 2)[k]
            mean(dat$outcome[dat$Factor1 == f1 & dat$Factor2 == f2])
          })

          max_dev <- max(abs(obs_means - target_means))

          data.frame(
            replication = r, condition = cond, design = des, type = type,
            max_mean_dev = max_dev, pass = max_dev < tol
          )
        }))
      }))
    }))
  }))
}

# aov simulation parallel
sim_optim_aov_mc <- function(S, n.cond, tol, dec, R, seed, keep_full = 1L) {
  conds      <- aov_conditions(S, n.cond, seed = seed)
  cond_names <- names(conds)
  des_names  <- names(aov_designs)
  n_cond     <- length(cond_names)
  n_des      <- length(des_names)

  dat  <- sim_aov_data(S, conds)
  targ <- aov_extract_targets(dat, dec)

  mean_tgt_F <- numeric(n_cond * n_des)
  k <- 0L
  for (cond in cond_names) {
    for (des in des_names) {
      k <- k + 1L
      tf <- targ[[cond]][[des]]$cont$f_vals
      mean_tgt_F[k] <- mean(tf, na.rm = TRUE)
    }
  }

  results <- future_lapply(seq_len(R), function(r) {
    res <- apply_optim_aov(targ, tol)

    cont_err  <- numeric(n_cond * n_des)
    int_err   <- numeric(n_cond * n_des)
    cont_diag <- vector("list", n_cond * n_des)
    int_diag  <- vector("list", n_cond * n_des)

    k <- 0L
    for (cond in cond_names) {
      for (des in des_names) {
        k <- k + 1L
        ce <- res[[cond]][[des]]$cont$best_error
        ie <- res[[cond]][[des]]$int$best_error
        cont_err[k] <- if (is.finite(ce)) ce else NA_real_
        int_err[k]  <- if (!is.na(ie) && is.finite(ie)) ie else NA_real_
        cont_diag[[k]] <- res[[cond]][[des]]$cont$diag
        int_diag[[k]]  <- res[[cond]][[des]]$int$diag
      }
    }

    out <- list(cont_err = cont_err, int_err = int_err,
                cont_diag = cont_diag, int_diag = int_diag)
    if (r <= keep_full) {
      out$full <- list(data = dat, targets = targ, results = res,
                       convergence = check_aov_conv(res, tol))
    }
    out
  }, future.seed = seed)

  cont_err_mat <- do.call(rbind, lapply(results, `[[`, "cont_err"))
  int_err_mat  <- do.call(rbind, lapply(results, `[[`, "int_err"))

  # Diagnostics: list of length R, each a list of length n_cond*n_des
  cont_diag_list <- lapply(results, `[[`, "cont_diag")
  int_diag_list  <- lapply(results, `[[`, "int_diag")

  col_labels <- expand.grid(design = des_names, condition = cond_names,
                            stringsAsFactors = FALSE)[, 2:1]

  list(
    conditions = conds,
    col_labels = col_labels,
    cont_err   = cont_err_mat,
    int_err    = int_err_mat,
    cont_diag  = cont_diag_list,
    int_diag   = int_diag_list,
    mean_tgt_F = mean_tgt_F,
    full_reps  = Filter(Negate(is.null), lapply(results, `[[`, "full"))
  )
}

# extract aov
extract_mc_errors_aov <- function(mc_res) {
  R      <- nrow(mc_res$cont_err)
  n_cols <- ncol(mc_res$cont_err)
  labels <- mc_res$col_labels

  cond_dists <- vapply(mc_res$conditions, function(spec) {
    paste(spec$cell_dists, collapse = ", ")
  }, character(1))

  cont_err   <- as.vector(t(mc_res$cont_err))
  int_err    <- as.vector(t(mc_res$int_err))
  mean_tgt_F <- rep(mc_res$mean_tgt_F, times = R)

  data.frame(
    replication    = rep(seq_len(R), each = n_cols),
    condition      = rep(labels$condition, times = R),
    design         = rep(labels$design, times = R),
    cell_dists     = cond_dists[rep(labels$condition, times = R)],
    mean_target_F  = mean_tgt_F,
    cont_error     = cont_err,
    cont_rel_error = cont_err / mean_tgt_F,
    int_error      = int_err,
    int_rel_error  = int_err / mean_tgt_F,
    stringsAsFactors = FALSE
  )
}

extract_diag_aov <- function(mc_res, type = c("cont", "int")) {
  type <- match.arg(type)
  diag_list <- mc_res[[paste0(type, "_diag")]]   # length R
  labels    <- mc_res$col_labels
  R         <- length(diag_list)
  n_cells   <- nrow(labels)

  rows <- vector("list", R * n_cells)
  k <- 0L
  for (r in seq_len(R)) {
    cell_diags <- diag_list[[r]]                  # length n_cells
    for (c in seq_len(n_cells)) {
      k <- k + 1L
      d <- cell_diags[[c]]
      if (is.null(d)) {
        rows[[k]] <- data.frame(
          replication = r, condition = labels$condition[c],
          design = labels$design[c], data_type = type,
          n_iter = NA_integer_, err_at_init = NA_real_,
          err_at_iter_100 = NA_real_, err_at_iter_1000 = NA_real_,
          err_final = NA_real_, improvement_frac = NA_real_,
          stalled_steps = NA_integer_, strict_improvements = NA_integer_,
          max_mean_drift = NA_real_
        )
      } else {
        rows[[k]] <- data.frame(
          replication = r, condition = labels$condition[c],
          design = labels$design[c], data_type = type,
          n_iter           = d$n_iter,
          err_at_init      = d$err_at_init,
          err_at_iter_100  = d$err_at_iter_100,
          err_at_iter_1000 = d$err_at_iter_1000,
          err_final        = d$err_final,
          improvement_frac = d$improvement_frac,
          stalled_steps    = d$stalled_steps,
          strict_improvements = d$strict_improvements,
          max_mean_drift   = d$max_mean_drift
        )
      }
    }
  }
  do.call(rbind, rows)
}

# plot example
plot_aov_example <- function(aov_mc, replication = 1, condition = "cond1",
                             design = "between", type = c("cont", "int"),
                             geom = c("histogram", "density"), bins = NULL) {
  type <- match.arg(type)
  geom <- match.arg(geom)

  rep   <- aov_mc$full_reps[[replication]]
  orig  <- rep$data[[condition]][[design]]$data
  res   <- rep$results[[condition]][[design]][[type]]

  if (is.null(res$data)) {
    message("No ", type, " result for ", condition, " / ", design)
    return(invisible(NULL))
  }

  rec <- res$data
  if (type == "int") orig$outcome <- round(orig$outcome)
  rec$Factor1 <- factor(rec$Factor1)
  rec$Factor2 <- factor(rec$Factor2)

  # Sanity check: print cell means side by side
  targ <- rep$targets[[condition]][[design]][[type]]
  cat("Target means: ", round(targ$group_means, 3), "\n")
  cat("Orig means:   ", sapply(1:4, function(k)
    round(mean(orig$outcome[orig$Factor1 == c(1,1,2,2)[k] &
                              orig$Factor2 == c(1,2,1,2)[k]]), 3)), "\n")
  cat("Rec means:    ", sapply(1:4, function(k)
    round(mean(rec$outcome[rec$Factor1 == c(1,1,2,2)[k] &
                             rec$Factor2 == c(1,2,1,2)[k]]), 3)), "\n")

  make_plot <- function(idx_orig, idx_rec, title) {
    df <- rbind(
      data.frame(value = orig$outcome[idx_orig], source = "Original"),
      data.frame(value = rec$outcome[idx_rec],   source = "Simulation")
    )
    p <- ggplot(df, aes(x = value, fill = source))
    if (geom == "histogram") {
      if (is.null(bins)) {
        bins <- min(30, round(max(10, length(unique(df$value[df$source == "Original"])) / 3)))
      }
      p <- p + geom_histogram(bins = bins, alpha = 0.7, position = "identity",
                              colour = "white", linewidth = 0.2)
    } else {
      p <- p + geom_density(alpha = 0.5, linewidth = 0.4)
    }
    p + scale_fill_manual(values = c("Original" = "grey25", "Simulation" = "grey75")) +
      labs(title = title, x = NULL, y = NULL, fill = NULL) +
      jtools::theme_apa() +
      theme(legend.position = "none")
  }

  plots <- list(
    make_plot(orig$Factor1 == 1 & orig$Factor2 == 1,
              rec$Factor1 == 1 & rec$Factor2 == 1, "Factor1=1, Factor2=1"),
    make_plot(orig$Factor1 == 1 & orig$Factor2 == 2,
              rec$Factor1 == 1 & rec$Factor2 == 2, "Factor1=1, Factor2=2"),
    make_plot(orig$Factor1 == 2 & orig$Factor2 == 1,
              rec$Factor1 == 2 & rec$Factor2 == 1, "Factor1=2, Factor2=1"),
    make_plot(orig$Factor1 == 2 & orig$Factor2 == 2,
              rec$Factor1 == 2 & rec$Factor2 == 2, "Factor1=2, Factor2=2"),
    make_plot(orig$Factor1 == 1, rec$Factor1 == 1, "Factor1=1 marginal"),
    make_plot(orig$Factor1 == 2, rec$Factor1 == 2, "Factor1=2 marginal"),
    make_plot(orig$Factor2 == 1, rec$Factor2 == 1, "Factor2=1 marginal"),
    make_plot(orig$Factor2 == 2, rec$Factor2 == 2, "Factor2=2 marginal")
  )

  plots[[8]] <- plots[[8]] +
    theme(legend.position = "bottom", legend.direction = "horizontal")

  wrap_plots(plots, ncol = 2)
}

# Simulate


#### Run Simulation ####
cat("Simulation starting...\n\n")

# AOV
aov_mc_res <- sim_optim_aov_mc(S = 300, n.cond = 100, tol = 0.005, dec = 2, R = 100, seed = seed, keep_full = 0)
saveRDS(aov_mc_res, file.path(save_dir, "aov_mc_res"))
cat("AOV module done!\n\n")

#mean_check <- check_aov_means(aov_mc)
#mean_check[mean_check$pass, ]

#aov_mc$replications[[1]]$targets$cond10$mixed$cont$f_vals


#### Results ####


if (!HPC) {
  options(scipen = 50)

  aov_mc_res  <- readRDS(file.path(save_dir, "aov_mc_res-2"))
  aov_results <- extract_mc_errors_aov(aov_mc_res)
  diag_cont <- extract_diag_aov(aov_mc_res, "cont")
  diag_int  <- extract_diag_aov(aov_mc_res, "int")
  diag_all  <- rbind(diag_cont, diag_int)

  bad_int = diag_int[diag_int$err_final > .1,]
  bad_cont = diag_int[diag_cont$err_final > .005,]
  bad_idx = unique(bad_cont$condition)
  bad_results = aov_results[aov_results$int_error > .1,]

  conds      <- aov_conditions(S = 300, n.cond = 100, seed = 310779)
  cond_names <- names(conds)
  des_names  <- names(aov_designs)
  n_cond     <- length(cond_names)
  n_des      <- length(des_names)

  dat  <- sim_aov_data(S = 300, conds)
  targ <- aov_extract_targets(dat, dec = 2)
  bad_targ = targ[names(targ) %in% bad_idx]

  bad_targ$cond18$within$cont
  bad_targ$cond26$within$cont
  bad_targ$cond40$within$cont
  bad_targ$cond82$within$cont
  targ_test = bad_targ$cond26$within$int


    bad_results %>%
      filter(design == "within") %>%
    group_by(condition) %>%
    summarise(mu = mean(int_error),
              sd = sd(int_error),
              min = min(int_error),
              max = max(int_error))
  # F range
  range(aov_results$mean_target_F, na.rm = TRUE)
  sum(is.na(aov_results$int_error))

  # Theme + palette ---------------------------------------------------------
  theme_sim <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
      theme(
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = base_size + 1,
                                        hjust = 0),
        plot.subtitle    = element_text(size = base_size - 1, color = "gray30"),
        plot.tag         = element_text(face = "bold", size = base_size + 2),
        axis.title       = element_text(color = "black"),
        axis.text        = element_text(color = "black"),
        strip.background = element_rect(fill = "gray90", color = "gray50"),
        strip.text       = element_text(face = "bold"),
        legend.position  = "bottom",
        legend.title     = element_blank()
      )
  }

  type_pal  <- c("Continuous" = "grey75", "Integer" = "grey25")
  EPS_FLOOR <- 1e-8
  TOL_ABS   <- 0.005
  TOL_REL   <- 0.01

  # Long format -------------------------------------------------------------
  long_df <- aov_results %>%
    select(condition, design, replication, mean_target_F,
           cont_error, cont_rel_error, int_error, int_rel_error) %>%
    pivot_longer(
      cols      = c(cont_error, cont_rel_error, int_error, int_rel_error),
      names_to  = "metric",
      values_to = "value"
    ) %>%
    mutate(
      data_type  = factor(ifelse(grepl("cont", metric), "Continuous", "Integer"),
                          levels = c("Continuous", "Integer")),
      error_type = factor(ifelse(grepl("rel",  metric), "Relative", "Absolute"),
                          levels = c("Absolute", "Relative")),
      design     = factor(design, levels = c("between", "within", "mixed")),
      value_disp = pmax(value, EPS_FLOOR)
    )

  # Panel A: ECDF of absolute errors
  ecdf_df <- long_df %>% filter(error_type == "Absolute", !is.na(value))

  panel_A <- ggplot(ecdf_df, aes(x = value_disp, colour = data_type)) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    geom_vline(xintercept = TOL_ABS, linetype = "dashed",
               colour = "grey40", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = type_pal) +
    facet_wrap(~ design, ncol = 3) +
    coord_cartesian(xlim = c(0, 0.1)) +
    labs(title = "Panel A",
         x = "Objective Value",
         y = "Cumulative Fraction of Replications") +
    jtools::theme_apa() +
    theme(legend.position = "bottom")

  # Panel B: Variance partition
  partition_aov <- function(err, des, cond) {
    keep <- !is.na(err)
    df   <- data.frame(x = err[keep],
                       d = factor(des[keep]),
                       c = factor(cond[keep]))
    fit  <- stats::aov(x ~ d * c, data = df)
    tab  <- summary(fit)[[1]]
    ss   <- setNames(tab[, "Sum Sq"], trimws(rownames(tab)))
    total <- sum(ss)
    c(Designs       = unname(ss["d"]         / total),
      Conditions    = unname(ss["c"]         / total),
      `Design x Condition` = unname(ss["d:c"]     / total),
      Replications  = unname(ss["Residuals"] / total))
  }

  vp_cont <- partition_aov(aov_results$cont_error,
                           aov_results$design, aov_results$condition)
  vp_int  <- partition_aov(aov_results$int_error,
                           aov_results$design, aov_results$condition)

  var_tbl <- tibble(
    data_type = factor(rep(c("Continuous", "Integer"), each = 4),
                       levels = c("Continuous", "Integer")),
    source    = factor(rep(names(vp_cont), 2),
                       levels = c("Replications", "Design x Condition",
                                  "Conditions", "Designs")),
    prop      = c(vp_cont, vp_int)
  ) %>%
    mutate(label = ifelse(prop >= 0.03,
                          sprintf("%.0f%%", 100 * prop), ""))

  panel_B <- ggplot(var_tbl, aes(x = data_type, y = prop, fill = source)) +
    geom_col(width = 0.55, colour = "white", linewidth = 0.3) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              colour = "white", fontface = "bold", size = 3.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = c("Replications"  = "gray80",
                                 "Design x Condition" = "gray65",
                                 "Conditions"    = "gray40",
                                 "Designs"       = "gray10")) +
    labs(title = "Panel B", x = NULL, y = "Share of Total Variance") +
    jtools::theme_apa() +
    theme(legend.position = "bottom")

  # Panel C: Caterpillar plot
  cond_summary <- aov_results %>%
    group_by(condition, design) %>%
    summarise(
      cont_med = median(cont_error, na.rm = TRUE),
      cont_q05 = quantile(cont_error, 0.1, na.rm = TRUE),
      cont_q95 = quantile(cont_error, 0.9, na.rm = TRUE),
      int_med  = median(int_error,  na.rm = TRUE),
      int_q05  = quantile(int_error, 0.1, na.rm = TRUE),
      int_q95  = quantile(int_error, 0.9, na.rm = TRUE),
      .groups  = "drop"
    )

  cat_df <- bind_rows(
    cond_summary %>% transmute(condition, design, data_type = "Continuous",
                               med = cont_med, q05 = cont_q05, q95 = cont_q95),
    cond_summary %>% transmute(condition, design, data_type = "Integer",
                               med = int_med,  q05 = int_q05,  q95 = int_q95)
  ) %>%
    mutate(data_type = factor(data_type, levels = c("Continuous", "Integer")),
           design    = factor(design,    levels = c("between", "within", "mixed"))) %>%
    group_by(data_type, design) %>%
    arrange(med, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  panel_C <- ggplot(cat_df, aes(x = rank, colour = data_type)) +
    geom_linerange(aes(ymin = pmax(q05, EPS_FLOOR),
                       ymax = pmax(q95, EPS_FLOOR)),
                   linewidth = 0.25, alpha = 0.55) +
    geom_point(aes(y = pmax(med, EPS_FLOOR)),
               size = 0.55, alpha = 0.9) +
    geom_hline(yintercept = TOL_ABS, linetype = "dashed",
               colour = "grey40", linewidth = 0.3) +
    scale_colour_manual(values = type_pal, guide = "none") +
    facet_grid(data_type ~ design) +
    coord_cartesian(ylim = c(0, 0.1)) +
    labs(title = "Panel C",
         x = "Condition (Ranked by Median)",
         y = "Objective Value") +
    jtools::theme_apa()

  # Panel D: Median error vs difficulty proxy (mean_target_F) --------------
  diag_df <- aov_results %>%
    group_by(condition, design, mean_target_F) %>%
    summarise(cont_med = median(cont_error, na.rm = TRUE),
              int_med  = median(int_error,  na.rm = TRUE),
              .groups  = "drop") %>%
    pivot_longer(c(cont_med, int_med),
                 names_to = "data_type", values_to = "med") %>%
    mutate(data_type = factor(ifelse(data_type == "cont_med",
                                     "Continuous", "Integer"),
                              levels = c("Continuous", "Integer")),
           design    = factor(design, levels = c("between", "within", "mixed")),
           med_disp  = pmax(med, EPS_FLOOR))

  panel_D <- ggplot(diag_df, aes(x = mean_target_F, y = med_disp,
                                 colour = data_type)) +
    geom_point(alpha = 0.35, size = 0.9) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.7,
                fill = "gray80", alpha = 0.4) +
    #scale_y_log10() +
    scale_colour_manual(values = type_pal) +
    facet_wrap(~ design, ncol = 3) +
    labs(title = "Panel D",
         x = "Mean Target F",
         y = "Median Objective Value") +
    jtools::theme_apa() +
    theme(legend.position = "bottom")

  # Manuscript table — design x data_type
  conv_cond <- aov_results %>%
    group_by(condition, design) %>%
    summarise(cont.conv = any(cont_error < TOL_ABS, na.rm = TRUE),
              int.conv  = any(int_error  < TOL_ABS, na.rm = TRUE),
              .groups   = "drop")

  conv_cond %>%
    group_by(design) %>%
    summarise(
      cont_any_conv_pct = mean(cont.conv) * 100,
      int_any_conv_pct  = mean(int.conv,  na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>% print()

  # Assemble main figure
  fig_main_aov <- (panel_A | panel_B) / (panel_C | panel_D) +
    plot_layout(heights = c(1, 1.1))

  if (FALSE) {
    ggsave("data-raw/plots/sim_aov.pdf", fig_main_aov,
           width = 260, height = 220, units = "mm",
           bg = "white", dpi = 300)
    print(var_tbl)
  }
}


# manuscript table
aov_results %>%
  group_by(design) %>%
  summarise(
    med_cont      = median(cont_error[is.finite(cont_error)]),
    med_int       = median(int_error[is.finite(int_error)], na.rm = TRUE),
    cont_succ_rate = mean(is.finite(cont_error) & cont_rel_error < 0.01) * 100,
    int_succ_rate  = mean(is.finite(int_error) & int_rel_error < 0.01, na.rm = TRUE) * 100,
    cont_conv_rate = mean(is.finite(cont_error) & cont_error < 0.005) * 100,
    int_conv_rate  = mean(is.finite(int_error) & int_error < 0.005, na.rm = TRUE) * 100,
    .groups = "drop"
  )




#### Example ####
gen_cell_errors
aov_exp <- sim_optim_aov_mc(S = 300, n.cond = 1, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 1)
aov_exp$conditions$cond1$cell_dists
aov_plot = plot_aov_example(aov_exp, design = "mixed", type = "int", geom = "histogram")
ggsave("data-raw/plots/aov.example.pdf", aov_plot,
       width = 260, height = 300, units = "mm",
       bg = "white", dpi = 300)



#### Testing ####
TOL_ABS <- 0.005

# 1. Integer (condition, design) pairs with at least one non-converged run --
non_conv_int <- aov_results %>%
  filter(!is.na(int_error)) %>%
  group_by(condition, design) %>%
  summarise(
    n_runs    = n(),
    n_fail    = sum(int_error >= TOL_ABS),
    fail_rate = n_fail / n_runs,
    med_err   = median(int_error),
    max_err   = max(int_error),
    .groups   = "drop"
  ) %>%
  filter(n_fail > 0) %>%
  arrange(design, desc(fail_rate))

cat(sprintf("Non-converged integer (cond, design) pairs: %d / %d\n",
            nrow(non_conv_int),
            n_distinct(aov_results$condition) * n_distinct(aov_results$design)))
print(non_conv_int)

# 2. F-granularity probe ----------------------------------------------------
# Take the integer-rounded simulation data as a baseline near the target,
# apply +/-1 to a sample of observations, record |dF| per effect.
# If min|dF| > TOL_ABS, the integer F-grid is fundamentally too coarse and
# convergence within thresh is mathematically impossible -- not an
# optimizer failure.
probe_granularity <- function(cond_name, des_name, n_probe = 40) {
  t <- targ[[cond_name]][[des_name]]$int
  if (is.null(t)) return(NULL)
  d <- dat[[cond_name]][[des_name]]$data
  d$outcome <- round(d$outcome)
  d$ID <- factor(d$ID)
  d$Factor1 <- factor(d$Factor1); d$Factor2 <- factor(d$Factor2)

  base <- suppressMessages(afex::aov_car(t$formula, data = d,
                                         factorize = FALSE))$anova_table$F
  idx  <- sample.int(nrow(d), min(n_probe, nrow(d)))

  dF <- do.call(rbind, lapply(idx, function(i) {
    do.call(rbind, lapply(c(-1L, 1L), function(s) {
      d2 <- d; d2$outcome[i] <- d2$outcome[i] + s
      fit <- suppressMessages(afex::aov_car(t$formula, data = d2,
                                            factorize = FALSE))
      abs(fit$anova_table$F - base)
    }))
  }))

  data.frame(
    condition = cond_name, design = des_name,
    effect    = t$effect_names,
    target_F  = t$f_vals,
    min_dF    = apply(dF, 2, min),
    p10_dF    = apply(dF, 2, quantile, 0.10),
    median_dF = apply(dF, 2, median),
    coarse_for_tol = apply(dF, 2, min) > TOL_ABS,
    stringsAsFactors = FALSE
  )
}

set.seed(seed)
gran_results <- do.call(rbind, lapply(seq_len(nrow(non_conv_int)), function(i) {
  probe_granularity(non_conv_int$condition[i], non_conv_int$design[i])
}))

# 3. Per-design summary: is the grid too coarse?
cat("\nGrid-resolution summary (by design):\n")
gran_results %>%
  group_by(design) %>%
  summarise(
    n_effect_probes      = n(),
    frac_coarse_for_tol  = mean(coarse_for_tol),
    median_min_dF        = median(min_dF),
    median_median_dF     = median(median_dF),
    .groups              = "drop"
  ) %>% print()

# 4. Per-condition: did failure correlate with grid coarseness?
cond_diag <- gran_results %>%
  group_by(condition, design) %>%
  summarise(any_effect_coarse = any(coarse_for_tol),
            min_min_dF        = min(min_dF),
            .groups           = "drop") %>%
  left_join(non_conv_int, by = c("condition", "design")) %>%
  arrange(design, desc(fail_rate))

cat("\nPer-condition diagnosis:\n")
print(cond_diag, n = Inf)

cat("\nCorrespondence between coarseness and failure:\n")
cond_diag %>%
  group_by(design) %>%
  summarise(
    n_failed         = n(),
    n_grid_coarse    = sum(any_effect_coarse, na.rm = TRUE),
    pct_explained_by_grid = mean(any_effect_coarse, na.rm = TRUE) * 100,
    median_fail_rate_when_coarse  = median(fail_rate[any_effect_coarse],   na.rm = TRUE),
    median_fail_rate_when_finer   = median(fail_rate[!any_effect_coarse], na.rm = TRUE),
    .groups          = "drop"
  ) %>% print()
