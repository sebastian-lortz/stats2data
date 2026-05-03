### Simulation Study from the manuscript
# HPC results
# 4.5 hours
# 108 cores
# 150GB

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
  plan(multisession, workers = 6L)
}


#### Descriptives Module ####
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


# generate data for all distributions and parameter conditions
sim_data <- function(N) {

  pars <- pars_conditions(N)

  out <- unlist(
    lapply(names(pars), function(dist) {

      setNames(
        lapply(names(pars[[dist]]), function(cond) {
          data_gen(dist, pars[[dist]][[cond]])
        }),
        paste(dist, names(pars[[dist]]), sep = "_")
      )

    }),
    recursive = FALSE
  )

  out
}


# get targets
uni_targets <- function(data, dec) {
  nms <- names(data)
  out <- vector("list", length(nms))
  names(out) <- nms
  for (nm in nms) {
    x <- data[[nm]]
    cont <- data.frame(
      mean = round(mean(x), dec),
      sd   = round(sd(x), dec),
      min  = round(min(x), dec),
      max  = round(max(x), dec)
    )
    temp <- round(x)
    if (length(unique(temp)) < 2) {
      cat("Degenerate integer case:", nm, "\n")
      int <- NULL
    } else {
      int <- data.frame(
        mean = round(mean(temp), dec),
        sd   = round(sd(temp), dec),
        min  = min(temp),
        max  = max(temp)
      )
    }
    out[[nm]] <- list(cont = cont, int = int)
  }
  out
}


# apply optim_vec
apply_optim_vec <- function(N, targets, tol, dec) {
  lapply(targets, function(x) {
    optim_vec_cont <- optim_vec(
      N = N,
      target_mean = setNames(x$cont$mean, "vec"),
      target_sd   = x$cont$sd,
      range       = c(x$cont$min, x$cont$max),
      thresh   = tol,
      integer     = FALSE,
      sprite_prec = c(dec, dec)
    )
    optim_vec_cont$track_error <- NULL
    optim_vec_cont$inputs      <- NULL

    if (is.null(x$int)) {
      optim_vec_int <- list(best_error = list(NA), status = "degenerate")
    } else {
      optim_vec_int <- optim_vec(
        N = N,
        target_mean = setNames(x$int$mean, "vec"),
        target_sd   = x$int$sd,
        range       = c(x$int$min, x$int$max),
        thresh   = tol,
        integer     = TRUE,
        sprite_prec = c(dec, dec)
      )
      optim_vec_int$track_error <- NULL
      optim_vec_int$inputs      <- NULL
    }
    list(cont = optim_vec_cont, int = optim_vec_int)
  })
}

# check convergence
check_vec_conv <- function(res, tol) {
  do.call(rbind, lapply(res, function(x) {
    cont_err <- x$cont$best_error[[1]]
    int_err  <- x$int$best_error[[1]]
    data.frame(
      cont_err  = cont_err,
      cont_conv = cont_err < tol,
      int_err   = int_err,
      int_conv  = if (is.na(int_err)) NA else int_err < tol
    )
  }))
}

# optim_vec simulation
sim_optim_vec <- function(N, tol, dec) {
  temp.dat <- sim_data(N)
  temp.targets <- uni_targets(temp.dat, dec)
  temp.res <- apply_optim_vec(N, temp.targets, tol, dec)
  check_vec_conv(temp.res, tol)
}

# MC parallelized
sim_optim_vec_mc <- function(N, tol, dec, R, seed, keep_full = 1L) {
  set.seed(seed)

  results <- future_lapply(seq_len(R), function(r) {
    dat     <- sim_data(N)
    targets <- uni_targets(dat, dec)
    res     <- apply_optim_vec(N, targets, tol, dec)
    conv    <- check_vec_conv(res, tol)

    # --- achieved mean & sd from simulated data ---
    safe_stat <- function(node, fn) {
      v <- node$data$vec
      if (is.null(v)) NA_real_ else fn(v)
    }
    cont_mean_ach <- vapply(res, function(x) safe_stat(x$cont, mean), numeric(1))
    cont_sd_ach   <- vapply(res, function(x) safe_stat(x$cont, sd),   numeric(1))
    int_mean_ach  <- vapply(res, function(x) {
      if (identical(x$int$status, "degenerate")) NA_real_ else safe_stat(x$int, mean)
    }, numeric(1))
    int_sd_ach    <- vapply(res, function(x) {
      if (identical(x$int$status, "degenerate")) NA_real_ else safe_stat(x$int, sd)
    }, numeric(1))

    # targets
    target_mean     <- vapply(targets, function(x) x$cont$mean, numeric(1))
    target_sd       <- vapply(targets, function(x) x$cont$sd,   numeric(1))
    int_target_mean <- vapply(targets, function(x) if (is.null(x$int)) NA_real_ else x$int$mean, numeric(1))
    int_target_sd   <- vapply(targets, function(x) if (is.null(x$int)) NA_real_ else x$int$sd,   numeric(1))

    out <- list(
      cont_err         = conv$cont_err,
      int_err          = conv$int_err,
      target_mean      = target_mean,
      target_sd        = target_sd,
      int_target_mean  = int_target_mean,
      int_target_sd    = int_target_sd,
      cont_mean_ach    = cont_mean_ach,
      cont_sd_ach      = cont_sd_ach,
      int_mean_ach     = int_mean_ach,
      int_sd_ach       = int_sd_ach
    )
    if (r <= keep_full) {
      out$full <- list(data = dat, targets = targets, results = res)
    }
    out
  }, future.seed = seed)

  # bind matrices
  dist_names <- names(sim_data(N))
  bind_mat <- function(field) {
    m <- do.call(rbind, lapply(results, `[[`, field))
    colnames(m) <- dist_names
    m
  }
  fields <- c("cont_err", "int_err",
              "target_mean", "target_sd",
              "int_target_mean", "int_target_sd",
              "cont_mean_ach", "cont_sd_ach",
              "int_mean_ach",  "int_sd_ach")

  out <- setNames(lapply(fields, bind_mat), fields)
  out$full_reps <- Filter(Negate(is.null), lapply(results, `[[`, "full"))
  out
}


# extract results
extract_mc_errors_vec <- function(mc_res) {
  R <- nrow(mc_res$cont_err)
  dist_names <- colnames(mc_res$cont_err)
  n_dist <- length(dist_names)
  vec <- function(x) as.vector(t(x))

  cont_err  <- vec(mc_res$cont_err)
  int_err   <- vec(mc_res$int_err)

  t_mean    <- vec(mc_res$target_mean)
  t_sd      <- vec(mc_res$target_sd)
  it_mean   <- vec(mc_res$int_target_mean)
  it_sd     <- vec(mc_res$int_target_sd)

  c_mean    <- vec(mc_res$cont_mean_ach)
  c_sd      <- vec(mc_res$cont_sd_ach)
  i_mean    <- vec(mc_res$int_mean_ach)
  i_sd      <- vec(mc_res$int_sd_ach)

  cont_mean_err <- abs(c_mean - t_mean)
  cont_sd_err   <- abs(c_sd   - t_sd)
  int_mean_err  <- abs(i_mean - it_mean)
  int_sd_err    <- abs(i_sd   - it_sd)

  data.frame(
    replication      = rep(seq_len(R), each = n_dist),
    distribution     = rep(dist_names, R),

    # targets
    target_mean      = t_mean,
    target_sd        = t_sd,
    int_target_mean  = it_mean,
    int_target_sd    = it_sd,

    # optimizer's combined loss
    cont_error       = cont_err,
    cont_rel_error   = cont_err / t_sd,
    int_error        = int_err,
    int_rel_error    = ifelse(is.na(int_err), NA, int_err / it_sd),

    # achieved
    cont_mean_ach    = c_mean,
    cont_sd_ach      = c_sd,
    int_mean_ach     = i_mean,
    int_sd_ach       = i_sd,

    # mean recovery diagnostic
    cont_mean_err    = cont_mean_err,
    int_mean_err     = int_mean_err,
    # sd recovery diagnostic
    cont_sd_err      = cont_sd_err,
    int_sd_err       = int_sd_err
  )
}

# plot example
plot_vec_example <- function(mc_res, rep_index = 1, filter = "extreme1",
                             type = c("cont", "int"),
                             geom = c("histogram", "density"), bins = NULL) {
  type <- match.arg(type)
  geom <- match.arg(geom)

  if (rep_index > length(mc_res$full_reps))
    stop("Replication ", rep_index, " not stored. Only ",
         length(mc_res$full_reps), " full replication(s) kept.")

  rep <- mc_res$full_reps[[rep_index]]

  dist_names <- c(
    normal   = "Normal",    uniform = "Uniform",  t        = "t",
    binomial = "Binomial",  exp     = "Exponential",
    gamma    = "Gamma",     weibull = "Weibull",
    chisq    = "Chi-squared", lnorm = "Log-normal"
  )

  all_dists <- names(rep$results)
  if (!is.null(filter)) {
    all_dists <- all_dists[grepl(filter, all_dists)]
  }

  is_int <- type == "int"

  plots <- lapply(all_dists, function(dist) {
    orig_vec <- rep$data[[dist]]
    if (is_int) orig_vec <- round(orig_vec)

    res <- rep$results[[dist]][[type]]
    if (is.null(res) || identical(res$status, "degenerate")) return(NULL)

    rec_vec <- res$data$vec

    dist_key   <- sub("_.*", "", dist)
    dist_label <- dist_names[dist_key]

    df <- data.frame(
      value  = c(orig_vec, rec_vec),
      source = rep(c("Original", "Simulation"), each = length(orig_vec))
    )

    p <- ggplot(df, aes(x = value, fill = source))
    if (geom == "histogram") {
      b <- if (is.null(bins)) min(30, round(max(10, length(unique(orig_vec)) / 3))) else bins
      p <- p + geom_histogram(bins = b, alpha = 0.7, position = "identity",
                              colour = "white", linewidth = 0.2)
    } else {
      p <- p + geom_density(alpha = 0.5, linewidth = 0.4)
    }

    p + scale_fill_manual(values = c("Original" = "grey25", "Simulation" = "grey75")) +
      labs(title = dist_label, x = NULL, y = NULL, fill = NULL) +
      jtools::theme_apa() +
      theme(legend.position = "none")
  })

  plots <- Filter(Negate(is.null), plots)

  plots[[length(plots)]] <- plots[[length(plots)]] +
    theme(legend.position = "bottom", legend.direction = "horizontal")

  wrap_plots(plots, ncol = 3)
}


#### MLR Module ####

# Predictors drawn from existing pars_conditions distributions
lm_conditions <- function(N, n.cond, seed) {
  set.seed(seed)
  pars <- pars_conditions(N)
  dist_names <- names(pars)
  #cond_names <- c("standard", "extreme1", "extreme2")
  cond_names <- c("standard")

  # sample 3 predictors x condition level for each of 3 LM conditions
  make_one <- function() {
    sampled_dists <- sample(dist_names, 3)
    sampled_conds <- sample(cond_names, 3, replace = TRUE)
    x_specs <- setNames(lapply(seq_len(3), function(i) {
      list(type = sampled_dists[i],
           pars = pars[[sampled_dists[i]]][[sampled_conds[i]]])
    }), paste0("x", 1:3))
    betas <- c(
      round(runif(5, -1, 1), 1)
    )
    x1 <- data_gen(x_specs$x1$type, x_specs$x1$pars)
    x2 <- data_gen(x_specs$x2$type, x_specs$x2$pars)
    x3 <- data_gen(x_specs$x3$type, x_specs$x3$pars)

    cont_ok <- all(c(sd(x1), sd(x2), sd(x3)) >= 0.1)
    int_ok  <- all(sapply(list(x1, x2, x3), function(v) length(unique(round(v))) >= 2))

    yhat <- betas[1] + betas[2]*x1 + betas[3]*x2 + betas[4]*x3 + betas[5]*x1*x2
    var_yhat <- var(yhat)
    target_r2 <- runif(1, 0.05, 0.5)
    sigma <- sqrt(var_yhat * (1 - target_r2) / target_r2)
    list(x_specs = x_specs, betas = betas, sigma = round(sigma, 2),
         target_r2 = target_r2, cont_ok = cont_ok, int_ok = int_ok)
  }

  conditions <- setNames(
    replicate(n.cond, make_one(), simplify = FALSE),
    paste0("cond", 1:n.cond)
  )
  conditions
}

# generate LM datasets — reuses data_gen
sim_mlr_data <- function(N, conds) {
  setNames(lapply(names(conds), function(cond) {
    spec <- conds[[cond]]
    x1 <- data_gen(spec$x_specs$x1$type, spec$x_specs$x1$pars)
    x2 <- data_gen(spec$x_specs$x2$type, spec$x_specs$x2$pars)
    x3 <- data_gen(spec$x_specs$x3$type, spec$x_specs$x3$pars)
    yhat <- spec$betas[1] +
      spec$betas[2] * x1 +
      spec$betas[3] * x2 +
      spec$betas[4] * x3 +
      spec$betas[5] * x1 * x2
    y <- yhat + rnorm(N, 0, spec$sigma) # I actually do make normality assumption here.
    R2 <- var(yhat) / var(y)
    list(
      data = data.frame(x1 = x1, x2 = x2, x3 = x3, y = y),
      R2   = R2
    )
  }), names(conds))
}


# extract targets
lm_targets <- function(data_list, dec) {
  lapply(data_list, function(x) {
    dat <- x$data
    vars <- c("x1", "x2", "x3", "y")

    extract <- function(d) {
      fit <- lm(y ~ x1 + x2 + x3 + x1:x2, data = d)
      s   <- summary(fit)
      t_mean <- setNames(sapply(vars, function(v) round(mean(d[[v]]), dec)), vars)
      t_sd   <- setNames(sapply(vars, function(v) round(sd(d[[v]]), dec)), vars)
      t_min  <- sapply(vars, function(v) round(min(d[[v]]), dec))
      t_max  <- sapply(vars, function(v) round(max(d[[v]]), dec))
      t_range <- rbind(t_min, t_max)
      cor_mat <- cor(d[, vars])
      t_cor   <- round(cor_mat[upper.tri(cor_mat)], dec)
      t_reg   <- round(coef(fit), dec)
      t_se    <- round(s$coefficients[, "Std. Error"], dec)
      list(N = nrow(d), mean = t_mean, sd = t_sd, range = t_range,
           cor = t_cor, reg = unname(t_reg), se = unname(t_se))
    }

    cont <- extract(dat)

    dat_int <- as.data.frame(lapply(dat, round))
    # check for degenerate columns
    if (any(sapply(dat_int, function(v) length(unique(v)) < 2))) {
      int <- NULL
    } else {
      int <- extract(dat_int)
    }

    list(cont = cont, int = int)
  })
}

# apply optim_mlr
apply_optim_mlr <- function(targets, tol, dec, conditions = NULL) {
  lapply(names(targets), function(cond) {
    targ <- targets[[cond]]

    skip_cont <- if (!is.null(conditions)) !conditions[[cond]]$cont_ok else FALSE
    skip_int  <- if (!is.null(conditions)) !conditions[[cond]]$int_ok else FALSE

    run_one <- function(t, is_int) {
      out <- tryCatch(
        optim_mlr(
          N             = t$N,
          target_mean   = t$mean,
          target_sd     = t$sd,
          range         = t$range,
          integer       = is_int,
          sprite_prec   = c(dec, dec),
          target_cor    = t$cor,
          target_reg    = t$reg,
          target_se     = t$se,
          reg_equation  = "y ~ x1 + x2 + x3 + x1:x2",
          thresh     = tol,
          progress_mode = "off"
        ),
        error = function(e) list(best_error = Inf, status = "infeasible",
                                 message = conditionMessage(e))
      )
      # === MEMORY FIX: drop heavy diagnostics before accumulating ===
      out$track_error       <- NULL
      out$track_error_ratio <- NULL
      out$inputs            <- NULL
      out$optim_vec         <- NULL
      out$data              <- NULL
      out
    }

    cont <- if (skip_cont) {
      list(best_error = NA, status = "degenerate")
    } else {
      run_one(targ$cont, FALSE)
    }

    int <- if (is.null(targ$int) || skip_int) {
      list(best_error = NA, status = "degenerate")
    } else {
      run_one(targ$int, TRUE)
    }

    list(cont = cont, int = int)
  }) |> setNames(names(targets))
}

# check convergence
check_mlr_conv <- function(res, tol) {
  do.call(rbind, lapply(names(res), function(cond) {
    cont_err <- res[[cond]]$cont$best_error
    int_err  <- res[[cond]]$int$best_error
    data.frame(
      condition = cond,
      cont_err  = cont_err,
      cont_conv = is.finite(cont_err) && cont_err < tol,
      int_err   = int_err,
      int_conv  = if (is.na(int_err)) NA else is.finite(int_err) && int_err < tol
    )
  }))
}

# parallel simulation
sim_optim_mlr_mc <- function(N, n.cond, tol, dec, R, seed, keep_full = 1L) {
  conds      <- lm_conditions(N, n.cond, seed = seed)
  cond_names <- names(conds)
  n_cond     <- length(cond_names)

  dat         <- sim_mlr_data(N, conds)
  targ        <- lm_targets(dat, dec)
  R2s         <- vapply(dat, function(x) x$R2, numeric(1))
  mean_target <- vapply(targ, function(t) {
    mean(c(abs(t$cont$cor), abs(t$cont$reg)), na.rm = TRUE)
  }, numeric(1))

  results <- future_lapply(seq_len(R), function(r) {
    res  <- apply_optim_mlr(targ, tol, dec, conditions = conds)
    conv <- check_mlr_conv(res, tol)

    cont_err <- vapply(res, function(x) {
      e <- x$cont$best_error; if (is.finite(e)) e else NA_real_
    }, numeric(1))
    int_err  <- vapply(res, function(x) {
      e <- x$int$best_error; if (!is.na(e) && is.finite(e)) e else NA_real_
    }, numeric(1))

    out <- list(cont_err = cont_err, int_err = int_err)
    if (r <= keep_full) {
      out$full <- list(data = dat, targets = targ, results = res,
                       convergence = conv, R2 = R2s)
    }
    out
  }, future.seed = seed)

  cont_err_mat <- do.call(rbind, lapply(results, `[[`, "cont_err"))
  int_err_mat  <- do.call(rbind, lapply(results, `[[`, "int_err"))
  colnames(cont_err_mat) <- colnames(int_err_mat) <- cond_names

  list(
    conditions  = conds,
    cont_err    = cont_err_mat,
    int_err     = int_err_mat,
    R2          = R2s,           # now a length-n_cond vector, not R × n_cond matrix
    mean_target = mean_target,   # same
    full_reps   = Filter(Negate(is.null), lapply(results, `[[`, "full"))
  )
}

# extract mlr results
extract_mc_errors_mlr <- function(mc_res) {
  cond_names <- colnames(mc_res$cont_err)
  R <- nrow(mc_res$cont_err)
  n_cond <- length(cond_names)

  cond_dists <- vapply(mc_res$conditions, function(spec) {
    paste(vapply(spec$x_specs, function(s) s$type, character(1)), collapse = ", ")
  }, character(1))

  target_r2 <- vapply(mc_res$conditions, function(spec) spec$target_r2, numeric(1))

  # Vectorised construction — no row-by-row rbind
  rep_idx  <- rep(seq_len(R), each = n_cond)
  cond_idx <- rep(seq_len(n_cond), times = R)

  cont_err    <- as.vector(t(mc_res$cont_err))
  int_err     <- as.vector(t(mc_res$int_err))
  r2        <- rep(mc_res$R2,          times = R)   # length R * n_cond, same ordering
  mean_targ <- rep(mc_res$mean_target, times = R)

  data.frame(
    replication    = rep_idx,
    condition      = cond_names[cond_idx],
    pred_dists     = cond_dists[cond_idx],
    R2             = r2,
    target_r2      = target_r2[cond_idx],
    mean_target    = mean_targ,
    cont_error     = cont_err,
    cont_rel_error = cont_err / mean_targ,
    int_error      = int_err,
    int_rel_error  = int_err / mean_targ,
    stringsAsFactors = FALSE
  )
}

# plot example
plot_mlr_example <- function(lm_mc, replication = 1, condition = "cond1",
                             type = c("cont", "int"),
                             geom = c("point", "density"), bins = NULL) {
  type <- match.arg(type)
  geom <- match.arg(geom)
  rep  <- lm_mc$full_reps[[replication]]
  res  <- rep$results[[condition]][[type]]
  targ <- rep$targets[[condition]][[type]]
  orig_df <- rep$data[[condition]]$data
  if (type == "int") orig_df <- as.data.frame(lapply(orig_df, round))
  rec_df <- as.data.frame(res$data)
  vars <- names(targ$mean)
  pred_names <- setdiff(vars, "y")

  # partial residuals
  partial_resid <- function(dat, focal_var) {
    other_vars <- setdiff(pred_names, focal_var)
    frm_y <- as.formula(paste("y ~", paste(other_vars, collapse = " + ")))
    frm_x <- as.formula(paste(focal_var, "~", paste(other_vars, collapse = " + ")))
    data.frame(x_partial = residuals(lm(frm_x, data = dat)),
               y_partial = residuals(lm(frm_y, data = dat)))
  }

  # partial regression plots
  partial_plots <- lapply(pred_names, function(v) {
    pr_orig <- partial_resid(orig_df, v)
    pr_orig$source <- "Original"
    pr_rec <- partial_resid(rec_df, v)
    pr_rec$source <- "Simulation"
    pr <- rbind(pr_orig, pr_rec)
    p <- ggplot(pr, aes(x = x_partial, y = y_partial, colour = source))
    if (geom == "point") {
      p <- p +
        geom_point(alpha = 0.7, size = 1.2, shape = 16) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 0.7)
    } else {
      p <- p +
        geom_density_2d(alpha = 0.6, linewidth = 0.4) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 0.7)
    }
    p + scale_colour_manual(values = c("Original" = "grey25", "Simulation" = "grey65")) +
      labs(title = paste0(v, " | Others"), x = paste0(v, " partial"), y = "y partial", colour = NULL) +
      jtools::theme_apa() +
      theme(legend.position = "none")
  })

  # marginal distribution overlays
  marginal_plots <- lapply(vars, function(v) {
    df <- rbind(
      data.frame(value = orig_df[[v]], source = "Original"),
      data.frame(value = rec_df[[v]], source = "Simulation")
    )
    b <- if (is.null(bins)) min(30, round(max(10, length(unique(orig_df[[v]])) / 3))) else bins
    ggplot(df, aes(x = value, fill = source)) +
      geom_histogram(bins = b, alpha = 0.7, position = "identity",
                     colour = "white", linewidth = 0.2) +
      scale_fill_manual(values = c("Original" = "grey25", "Simulation" = "grey75")) +
      labs(title = paste0(v, " Marginal"), x = NULL, y = NULL, fill = NULL) +
      jtools::theme_apa() +
      theme(legend.position = "none")
  })

  marginal_plots[[length(marginal_plots)]] <- marginal_plots[[length(marginal_plots)]] +
    theme(legend.position = "bottom", legend.direction = "horizontal")

  # diagnostics
  frm <- as.formula("y ~ x1 + x2 + x3 + x1:x2")
  cat("Target coefs:", targ$reg, "\n")
  cat("Obs coefs:   ", round(coef(lm(frm, data = rec_df)), 3), "\n")
  cat("Target cor:  ", targ$cor, "\n")
  cat("Obs cor:     ", round(cor(rec_df[, vars])[upper.tri(cor(rec_df[, vars]))], 3), "\n")

  # layout: row 1 = partial regression, row 2 = marginals
  (wrap_plots(partial_plots, ncol = 3) /
      wrap_plots(marginal_plots, ncol = 4))
}


#### Run Simulation ####
cat("Simulation starting...\n\n")

# VEC
if (FALSE) {
vec_mc_res <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 10000, seed = seed, keep_full = 0)
saveRDS(vec_mc_res, file.path(save_dir, "vec_mc_res"))
cat("VEC module done!\n\n")
rm(vec_mc_res)
}
# MLR
lm_mc_res <- sim_optim_mlr_mc(N = 300, n.cond = 500, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 0)
saveRDS(lm_mc_res, file.path(save_dir, "lm_mc_res_std"))
cat("MLR module done!\n\n")
rm(lm_mc_res)

#mean_check <- check_aov_means(aov_mc)
#mean_check[mean_check$pass, ]

#aov_mc$replications[[1]]$targets$cond10$mixed$cont$f_vals



#### VEC Results ####
if (!HPC) {
  options(scipen = 50)

  ## vec module ##
  vec_mc_res <- readRDS(file.path(save_dir,"vec_mc_res"))

  vec_results <- extract_mc_errors_vec(vec_mc_res)

  # manuscript table
  vec_results %>%
    summarise(
      cont_conv_rate  = mean(cont_error < 0.005, na.rm = TRUE) * 100,
      int_conv_rate   = mean(int_error < 0.005, na.rm = TRUE) * 100
    )
  which(!is.numeric(vec_results$cont_error))
  nrow(vec_results)
  # no NA

  idx = which(vec_results$int_error > .005)
  vec_results[idx,]
  # t_extreme2 -> rounding/ floating number issue

  idx = which(vec_results$cont_error > .005)
  nrow(vec_results[idx,])
  vec_results[idx,]
  # all lnorm_extreme1 -> could converge if runtime longer
  vec_results[order(vec_results$target_sd, decreasing = TRUE),]
  plot(vec_results$target_sd, vec_results$cont_error,
       col = "black", pch = 16,
       xlab = "target_sd", ylab = "cont_error")
  points(vec_results$target_sd[idx],
         vec_results$cont_error[idx],
         col = "red", pch = 16)
  # vec module would converge basically in all runs

}

#### MLR Results ####
if (!HPC) {

  lm_mc_res <- readRDS(file.path(save_dir,"lm_mc_res"))
  lm_results <- extract_mc_errors_mlr(lm_mc_res)

  conv_cond = lm_results %>%
    group_by(condition) %>%
    summarize(cont.conv = (min(cont_error) < .005),
              int.conv = (min(int_error) < .005))

    sum(conv_cond$cont.conv)/500 *100
    # 80.8% converged at least once
    sum(conv_cond$int.conv)/500 *100
    # 69.0% converged at least once

  # Theme + palette
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

  type_pal <- c("Continuous" = "grey75", "Integer" = "grey25")

  # Plot floor
  EPS_FLOOR <- 1e-8
  TOL_ABS   <- 0.005
  TOL_REL   <- 0.01


  #Long data frame

  long_df <- lm_results %>%
    select(condition, replication, R2,
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
      value_disp = pmax(value, EPS_FLOOR)
    )


  # ECDF of relative errors
  ecdf_df <- long_df %>% filter(error_type == "Absolute", !is.na(value))

  panel_A <- ggplot(ecdf_df, aes(x = value_disp, colour = data_type)) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    #scale_x_log10(labels = function(x) ifelse(x < 1e-3, scales::scientific(x), x)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = type_pal) +
    labs(
      title = "Panel A",
      x        = "Objective Value",
      y        = "Cumulative Fraction of Replications"
    ) +
    xlim(c(0,.1))+
    theme_sim() +
    jtools::theme_apa() +
    theme(legend.position = "bottom")

  # Variance partition on log10(error)

  partition_one <- function(err_vec, condition) {
    x <- err_vec[!is.na(err_vec)]
    g <- condition[!is.na(err_vec)]

    agg <- tibble(g, x) %>%
      group_by(g) %>%
      summarise(m = mean(x), v = var(x), n = dplyr::n(), .groups = "drop")

    total_var   <- var(x)
    between_var <- var(agg$m)
    within_var  <- mean(agg$v, na.rm = TRUE)

    s <- total_var / (between_var + within_var)
    c(between = between_var * s, within = within_var * s, total = total_var)
  }

  vp_cont <- partition_one(lm_results$cont_error, lm_results$condition)
  vp_int  <- partition_one(lm_results$int_error,  lm_results$condition)

  var_tbl <- tibble(
    data_type = factor(c("Continuous", "Continuous", "Integer", "Integer"),
                       levels = c("Continuous", "Integer")),
    source    = factor(c("Conditions", "Replications",
                         "Conditions", "Replications"),
                       levels = c("Replications",
                                  "Conditions")),
    variance  = c(vp_cont["between"], vp_cont["within"],
                  vp_int["between"],  vp_int["within"])
  ) %>%
    group_by(data_type) %>%
    mutate(prop = variance / sum(variance),
           label = sprintf("%.0f%%", 100 * prop)) %>%
    ungroup()

  panel_B <- ggplot(var_tbl,
                    aes(x = data_type, y = prop, fill = source)) +
    geom_col(width = 0.55, colour = "white", linewidth = 0.3) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              colour = "white", fontface = "bold", size = 3.4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = c("Conditions"             = "gray25",
                                 "Replications" = "gray75")) +
    labs(
      title = "Panel B",
      x        = NULL,
      y        = "Share of Total Variance"
    ) +
    theme_sim() +
    jtools::theme_apa() +
    theme(legend.position = "bottom")


  # Caterpillar plot --------------------------------------
  # Condition-level uncertainty: median +/- IQR, conditions ranked.
  # Ranked within each panel; we are NOT inviting a cross-panel positional
  # comparison.

  cond_summary <- lm_results %>%
    group_by(condition) %>%
    summarise(
      cont_med = median(cont_error, na.rm = TRUE),
      cont_q05 = quantile(cont_error, 0.1, na.rm = TRUE),
      cont_q95 = quantile(cont_error, 0.9, na.rm = TRUE),
      int_med  = median(int_error,  na.rm = TRUE),
      int_q05  = quantile(int_error, 0.1, na.rm = TRUE),
      int_q95  = quantile(int_error, 0.9, na.rm = TRUE),
      .groups  = "drop"
    )
  sum(cond_summary$cont_med <.01)/500
  sum(cond_summary$int_med <.01)/500

  cat_df <- bind_rows(
    cond_summary %>%
      transmute(condition,
                data_type = "Continuous",
                med = cont_med, q05 = cont_q05, q95 = cont_q95),
    cond_summary %>%
      transmute(condition,
                data_type = "Integer",
                med = int_med, q05 = int_q05, q95 = int_q95)
  ) %>%
    mutate(data_type = factor(data_type, levels = c("Continuous", "Integer"))) %>%
    group_by(data_type) %>%
    arrange(med, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  panel_C <- ggplot(cat_df, aes(x = rank, colour = data_type)) +
    geom_linerange(aes(ymin = pmax(q05, EPS_FLOOR),
                       ymax = pmax(q95, EPS_FLOOR)),
                   linewidth = 0.25, alpha = 0.55) +
    geom_point(aes(y = pmax(med, EPS_FLOOR)),
               size = 0.55, alpha = 0.9) +
    #geom_hline(yintercept = .005, linetype = "dashed") +
    #scale_y_log10() +
    scale_colour_manual(values = type_pal, guide = "none") +
    facet_wrap(~ data_type, ncol = 2) +
    labs(
      title    = "Panel C",
      x        = "Condition (Ranked by Median)",
      y        = "Objective Value"
    ) +
    ylim(c(0,.1)) +
    theme_sim() +
    jtools::theme_apa()


  # Diagnostic vs target difficulty

  diag_df <- lm_results %>%
    group_by(condition, R2) %>%
    summarise(cont_med = median(cont_error, na.rm = TRUE),
              int_med  = median(int_error,  na.rm = TRUE),
              .groups  = "drop") %>%
    pivot_longer(c(cont_med, int_med),
                 names_to = "data_type", values_to = "med") %>%
    mutate(data_type = factor(ifelse(data_type == "cont_med",
                                     "Continuous", "Integer"),
                              levels = c("Continuous", "Integer")),
           med_disp = pmax(med, EPS_FLOOR))

  panel_D <- ggplot(diag_df, aes(x = R2, y = med_disp, colour = data_type)) +
    geom_point(alpha = 0.35, size = 0.9) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.7,
                fill = "gray80", alpha = 0.4) +
    #scale_y_log10() +
    scale_colour_manual(values = type_pal) +
    labs(
      title    = "Panel D",
      x        = expression(Estimated~R^2),
      y        = "Median Objective Value"
    ) +
    theme_sim() +
    jtools::theme_apa() +
    theme(legend.position = "bottom")

  # Assemble main figure

  fig_main <- (panel_A | panel_B) / (panel_C | panel_D) +
    plot_layout(heights = c(1, 1))

  # save
  if (FALSE) {
    ggsave("data-raw/plots/sim_mlr.pdf",  fig_main, width = 240, height = 200,
           units = "mm", bg = "white", dpi = 300)

    # Print variance partition
    print(var_tbl)
  }

}

#### Example ####
# VEC
vec_exp <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 1)
vec.plot    <- plot_vec_example(vec_exp, rep_index = 1, filter = "extreme1", type = "cont")
ggsave(
  filename = "data-raw/plots/vec.example.pdf",
  plot     = vec.plot,
  width    = 300,
  height   = 300,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

# MLR
mlr_exp <- sim_optim_mlr_mc(N = 300, n.cond = 1, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 1)
mlr_plot = plot_mlr_example(mlr_exp, type = "cont", geom = "point")
ggsave("data-raw/plots/mlr.example.pdf", mlr_plot,
       width = 300, height = 200, units = "mm",
       bg = "white", dpi = 300)


#### VEC for lognormal with higher budget ####

sim_optim_vec_lnorm_e1 <- function(N          = 300,
                                   tol        = 0.005,
                                   dec        = 2,
                                   R          = 3,
                                   seed       = 310779,
                                   max_iter   = 2e5) {

  set.seed(seed)

  results <- future_lapply(seq_len(R), function(r) {
    # Reproduce the original RNG stream: call sim_data() in full so the
    # lnorm_extreme1 sample matches the one used in vec_mc_res.
    dat     <- sim_data(N)
    targets <- uni_targets(dat, dec)
    t_cont  <- targets[["lnorm_extreme1"]]$cont

    res <- optim_vec(
      N           = N,
      target_mean = setNames(t_cont$mean, "vec"),
      target_sd   = t_cont$sd,
      range       = c(t_cont$min, t_cont$max),
      thresh   = tol,
      integer     = FALSE,
      sprite_prec = c(dec, dec),
      max_iter    = max_iter
      )

    list(
      cont_err      = res$best_error[[1]],
      target_mean   = t_cont$mean,
      target_sd     = t_cont$sd,
      cont_mean_ach = mean(res$data$vec),
      cont_sd_ach   = sd(res$data$vec)
    )
  }, future.seed = seed)

  out <- data.frame(
    replication   = seq_len(R),
    target_mean   = vapply(results, `[[`, numeric(1), "target_mean"),
    target_sd     = vapply(results, `[[`, numeric(1), "target_sd"),
    cont_error    = vapply(results, `[[`, numeric(1), "cont_err"),
    cont_mean_ach = vapply(results, `[[`, numeric(1), "cont_mean_ach"),
    cont_sd_ach   = vapply(results, `[[`, numeric(1), "cont_sd_ach")
  )
  out$cont_conv     <- out$cont_error    < tol
  out$cont_mean_err <- abs(out$cont_mean_ach - out$target_mean)
  out$cont_sd_err   <- abs(out$cont_sd_ach   - out$target_sd)
  out
}

start_time = Sys.time()
lnorm_e1_res <- sim_optim_vec_lnorm_e1(
  N          = 300,
  tol        = 0.005,
  dec        = 2,
  R          = 10000,
  seed       = seed,
  max_iter   = 1e5
)
end_time = Sys.time()
end_time - start_time
sum(lnorm_e1_res$cont_conv)

lnorm_e1_res_high <- sim_optim_vec_lnorm_e1(
  N          = 300,
  tol        = 0.005,
  dec        = 2,
  R          = 10000,
  seed       = seed,
  max_iter   = 5e5
)
saveRDS(lnorm_e1_res_high, file.path(save_dir, "vec_high_budget"))

summary_tbl <- data.frame(
  R                = nrow(lnorm_e1_res_high),
  conv_rate_pct    = mean(lnorm_e1_res_high$cont_conv)         * 100,
  n_failed         = sum(!lnorm_e1_res_high$cont_conv),
  max_error        = max(lnorm_e1_res_high$cont_error),
  median_error     = median(lnorm_e1_res_high$cont_error),
  mean_within_tol  = mean(lnorm_e1_res_high$cont_mean_err < 0.005) * 100,
  sd_within_tol    = mean(lnorm_e1_res_high$cont_sd_err   < 0.005) * 100
)
print(summary_tbl)
# all runs converged!

