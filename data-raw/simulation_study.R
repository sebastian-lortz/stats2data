### Simulation Study from the manuscript
# HPC results
# 6 hours
# 38 cores
# 15GB

#### Configs ####
options(repos = c(CRAN = "https://ftp.belnet.be/mirror/CRAN/"))
HPC <- FALSE
cov_check = TRUE
run_vec <- TRUE
run_mlr <- TRUE

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
library(PearsonDS)
library(moments)
library(ggnewscale)

# backend
if (HPC) {
  plan(multisession, workers = availableCores()-5)
} else {
  plan(multisession, workers = 6L)
}



#### Descriptives Module ####
# Data generation

# sample moment plane
sample_moments <- function(C, skew_max = 2, exkurt_max = 7, buffer = 0.05,
                           mu_min = 0, mu_max = 0,
                           sigma_min = 3, sigma_max = 3) {
  # Rejection sampling
  out <- data.frame(skew = numeric(0), exkurt = numeric(0))
  while (nrow(out) < C) {
    n_try <- 2 * (C - nrow(out))
    s <- runif(n_try, -skew_max, skew_max)
    k <- runif(n_try, -2 , exkurt_max)
    keep <- k > s^2 - 2 + buffer
    out <- rbind(out, data.frame(skew = s[keep], exkurt = k[keep]))
  }
  out <- out[seq_len(C), ]
  out$mu    <- runif(C, mu_min,    mu_max)
  out$sigma <- runif(C, sigma_min, sigma_max)
  out
}

# sample univariate data
sample_data <- function(moments, N) {
  mapply(function(skew, exkurt, mu, sigma) {
      rpearson(N, moments = c(mean     = mu,
                              variance = sigma^2,
                              skewness = skew,
                              kurtosis = exkurt + 3))},
  skew   = moments$skew,
  exkurt = moments$exkurt,
  mu     = moments$mu,
  sigma  = moments$sigma,
  SIMPLIFY = FALSE)
}


# uni targets
uni_targets <- function(samples, dec) {
  list(
  cont = do.call(rbind, lapply(samples, function(x) {
    data.frame(
      mean   = round(mean(x),dec),
      sd     = round(sd(x),dec),
      min    = round(min(x),dec),
      max    = round(max(x),dec),
      range  = round(max(x),dec) - round(min(x),dec),
      skew   = moments::skewness(x),
      exkurt = moments::kurtosis(x) - 3
    )
  })),
  int = do.call(rbind, lapply(samples, function(x) {
    xi  <- round(x)
    uni <- length(unique(xi))
    data.frame(
      mean   = round(mean(xi),dec),
      sd     = round(sd(xi),dec),
      min    = min(xi),
      max    = max(xi),
      range  = max(xi) - min(xi),
      uni    = uni,
      skew   = if (uni < 2) NA_real_ else moments::skewness(xi),
      exkurt = if (uni < 2) NA_real_ else moments::kurtosis(xi) - 3
    )
  }))
)}

# Coverage check
if (cov_check) {
C <- 5000     # number of target conditions
N <- 300     # sample size per condition
true_moments <- sample_moments(C)
samples      <- sample_data(true_moments, N)
targets      <- uni_targets(samples, dec = 2)

# Plots
p_cont <- ggplot(
  pivot_longer(targets$cont, everything(),
               names_to = "var", values_to = "value"),
  aes(x = value)
) +
  geom_histogram(bins = 30) +
  facet_wrap(~ var, scales = "free", ncol = 4) +
  labs(title = "Continuous targets")

p_int <- ggplot(
  pivot_longer(targets$int, everything(),
               names_to = "var", values_to = "value"), aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ var, scales = "free", ncol = 4) +
  labs(title = "Integer (rounded) targets")

p_cont
p_int

table(targets$int$uni)


### kurtosis plot

plot_cf <- function(moments, skew = "skew", exkurt = "exkurt", intensity = 0.5, bound = TRUE) {
  # moments: data frame with columns `skew` (signed) and `exkurt` (excess)
  if (bound) {
    xmax = 4
    ymax = 10
  } else {
    xmax <- max(4,  ceiling(max(moments[[skew]]^2,      na.rm = TRUE)))
    ymax <- max(10, ceiling(max(moments[[exkurt]] + 3,  na.rm = TRUE)))
  }
  # Theoretical points (non-excess kurtosis, squared skewness) ---------------
  pts <- data.frame(
    dist  = c("normal", "uniform", "exponential", "logistic", "chi-sq(df=4)"),
    skew2 = c(0, 0, 4, 0, 2),
    kurt  = c(3, 1.8, 9, 4.2, 6)
  )

  # Gamma line (chi-square lies on this same line)
  gam <- data.frame(skew2 = seq(0, xmax, length.out = 200))
  gam$kurt <- 3 + 1.5 * gam$skew2

  # Lognormal curve, parametric in sigma
  s  <- seq(0.01, 1.2, length.out = 200)
  w  <- exp(s^2)
  ln <- data.frame(skew2 = ((w + 2) * sqrt(w - 1))^2,
                   kurt  = w^4 + 2*w^3 + 3*w^2 - 3)

  ggplot() +
    geom_line(data = gam, aes(skew2, kurt)) +
    geom_line(data = ln,  aes(skew2, kurt), linetype = "dashed") +
    geom_point(data = moments, aes(.data[[skew]]^2, .data[[exkurt]] + 3),
               size = 0.4, alpha = intensity, colour = "steelblue") +
    geom_point(data = pts, aes(skew2, kurt, shape = dist),
               size = 3, fill = "white", stroke = 0.6) +
    scale_shape_manual(values = c("normal" = 21, "uniform" = 22,
                                  "exponential" = 23, "logistic" = 24,
                                  "chi-sq(df=4)" = 25)) +
    scale_y_reverse(limits = c(ymax, 0)) +
    scale_x_continuous(limits = c(0, xmax)) +
    labs(title = "Cullen and Frey graph",
         x = "square of skewness", y = "kurtosis") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black"))
}

# Usage
plot_cf(true_moments)

}

# apply optim_vec
apply_optim_vec <- function(N, targets, tol, dec) {
  cont_out = int_out = list()
  for (i in 1:nrow(targets$cont)) {
    optim_vec_cont <- optim_vec(
      N = N,
      target_mean = setNames(targets$cont$mean[i], "vec"),
      target_sd   = targets$cont$sd[i],
      range       = c(targets$cont$min[i], targets$cont$max[i]),
      thresh   = tol,
      integer     = FALSE,
      sprite_prec = c(dec, dec)
    )
    optim_vec_cont$track_error = NULL
    optim_vec_cont$inputs      = NULL
    cont_out[[i]] = optim_vec_cont
    if (is.na(targets$int$skew[i]) || targets$int$uni[i] < 2) {
      optim_vec_int <- list(best_error = list(NA), status = "degenerate")
    } else {
      optim_vec_int <- optim_vec(
        N = N,
        target_mean = setNames(targets$int$mean[i], "vec"),
        target_sd   = targets$int$sd[i],
        range       = c(targets$int$min[i], targets$int$max[i]),
        thresh   = tol,
        integer     = TRUE,
        sprite_prec = c(dec, dec)
      )
      optim_vec_int$track_error <- NULL
      optim_vec_int$inputs      <- NULL
    }
    int_out[[i]] = optim_vec_int
  }
  list(cont = cont_out, int = int_out)
}

# check convergence
check_vec_conv <- function(res, tol) {
  n = nrow(res[[1]])
  int = sum(res[[1]]$int_err < tol) / n
  cont = sum(res[[1]]$cont_err < tol) / n
  data.frame(
    cont_conv = cont,
    int_conv  = int
  )
}

# distances
quantile_distance <- function(x, y, probs = seq(.01, .99, .01), standardize = TRUE) {
  if (standardize) {
    x <- (x - mean(x)) / sd(x)
    y <- (y - mean(y)) / sd(y)
  }

  qx <- quantile(x, probs)
  qy <- quantile(y, probs)

  sqrt(mean((qx - qy)^2))
}
ecdf_distance <- function(x, y) {

  vals <- sort(unique(c(x, y)))

  Fx <- ecdf(x)(vals)
  Fy <- ecdf(y)(vals)

  sqrt(mean((Fx - Fy)^2))
}

# MC parallelized
sim_optim_vec_mc <- function(N, tol, dec, R, seed, keep_full = 0) {
  set.seed(seed)
  results <- future_lapply(seq_len(R), function(r) {
    tm      <- sample_moments(1)
    samp    <- sample_data(tm, N)
    int_samp <- round(samp[[1]])
    targets <- uni_targets(samp, dec = dec)
    res     <- apply_optim_vec(N = N, targets = targets, tol = tol, dec = dec)

    cv <- res$cont[[1]]$data$vec
    iv <- res$int[[1]]$data$vec

    # hellinger
    cont_dist = quantile_distance(cv, samp[[1]])
    int_dist = ecdf_distance(iv, int_samp)

    row <- data.frame(
      rep              = r,
      # true Pearson moments (what we asked rpearson for)
      true_mu          = tm$mu,
      true_sigma       = tm$sigma,
      true_skew        = tm$skew,
      true_exkurt      = tm$exkurt,
      # continuous: targets (what optim was asked to hit)
      cont_target_mean = targets$cont$mean,
      cont_target_sd   = targets$cont$sd,
      cont_target_min  = targets$cont$min,
      cont_target_max  = targets$cont$max,
      cont_target_range = targets$cont$max-targets$cont$min,
      cont_target_skew = targets$cont$skew,
      cont_target_exkurt = targets$cont$exkurt,
      # continuous: achieved (from optim output)
      cont_ach_mean    = mean(cv),
      cont_ach_sd      = sd(cv),
      cont_ach_min     = min(cv),
      cont_ach_max     = max(cv),
      cont_ach_range   = max(cv) - min(cv),
      cont_err         = unlist(res$cont[[1]]$best_error),
      cont_ach_skew    = moments::skewness(cv),
      cont_ach_exkurt  = moments::kurtosis(cv) - 3,
      cont_ach_dist    = cont_dist,
      # integer: targets
      int_target_mean  = targets$int$mean,
      int_target_sd    = targets$int$sd,
      int_target_min   = targets$int$min,
      int_target_max   = targets$int$max,
      int_target_range = targets$int$max-targets$int$min,
      int_target_uni   = targets$int$uni,
      int_target_skew  = targets$int$skew,
      int_target_exkurt = targets$int$exkurt,
      # integer: achieved
      int_ach_mean     = mean(iv),
      int_ach_sd       = sd(iv),
      int_ach_min      = min(iv),
      int_ach_max      = max(iv),
      int_ach_range    = max(iv) - min(iv),
      int_err          = unlist(res$int[[1]]$best_error),
      int_ach_uni      = length(unique(iv)),
      int_ach_skew     = moments::skewness(iv),
      int_ach_exkurt   = moments::kurtosis(iv) - 3,
      int_ach_dist    = int_dist
    )

    full <- if (r <= keep_full) {
      list(true_moments = tm, samples = samp, targets = targets, results = res)
    } else NULL

    list(row = row, full = full)
  }, future.seed = TRUE)

  list(
    summary = do.call(rbind, lapply(results, `[[`, "row")),
    full_reps = Filter(Negate(is.null), lapply(results, `[[`, "full"))
  )
}


# plot example
plot_vec_example <- function(mc_res, rep_index = 1,
                             type = c("cont", "int"),
                             geom = c("histogram", "density"), legend = FALSE, bins = NULL) {
  type <- match.arg(type)
  geom <- match.arg(geom)

  if (length(mc_res$full_reps) == 0L || rep_index > length(mc_res$full_reps))
    stop("Replication ", rep_index, " not stored. Only ",
         length(mc_res$full_reps), " full replication(s) kept.")

  rep <- mc_res$full_reps[[rep_index]]

  orig_vec <- rep$samples[[1]]
  if (type == "int") orig_vec <- round(orig_vec)

  res <- rep$results[[type]][[1]]
  if (is.null(res) || identical(res$status, "degenerate"))
    stop("The ", type, " example is degenerate; nothing to plot.", call. = FALSE)
  rec_vec <- res$data$vec

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

  p +
    scale_fill_manual(values = c("Original" = "grey80", "Simulation" = "grey30")) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_sim() + jtools::theme_apa() +
    theme(legend.position = if (legend) "bottom" else "none",
        legend.direction = "horizontal")
}


#### MLR Module ####

sim_mlr_data <- function(N) {
  tm = sample_moments(3)
  x_mat = sapply(seq_len(nrow(tm)), function(i) {unlist(sample_data(tm[i,], N))})
    betas = runif(5, -1, 1)
    yhat = betas[1] + betas[2]*x_mat[,1] + betas[3]*x_mat[,2] + betas[4]*x_mat[,3] + betas[5]*x_mat[,1]*x_mat[,2]
    #yhat = yhat / sd(yhat)*3
    sd_yhat = sd(yhat)
    true_r2 <- runif(1, 0.05, 0.5)
    sigma <- sqrt(sd_yhat^2 * (1 - true_r2) / true_r2)
    y = yhat + rnorm(N, 0, sigma)
    dat = data.frame(x1 = x_mat[,1], x2 = x_mat[,2], x3 = x_mat[,3], y = y)
 list(
   data = dat,
   true_R2   = true_r2,
   true_betas = betas,
   true_sigma = sigma,
   true_moments = tm)
}
# d = sim_mlr_data(N = 300)$data

# extract targets
mlr_targets <- function(d, dec) {

    extract = function(d) {
      N = nrow(d)
      fit <- summary(lm(y ~ x1 + x2 + x3 + x1:x2, data = d))
      t_mean = round(colMeans(d), dec)
      t_sd = round(apply(d, 2, sd), dec)
      t_min = round(apply(d, 2, min), dec)
      t_max = round(apply(d, 2, max), dec)
      t_range = rbind(t_min, t_max)
      skew   = apply(d, 2, moments::skewness)
      exkurt = apply(d, 2, moments::kurtosis) - 3
      cor_mat = cor(d)
      t_cor = round(cor_mat[upper.tri(cor_mat)], dec)
      coef = round(coef(fit), dec)
      t_reg = coef[, "Estimate"]
      t_se = coef[, "Std. Error"]

    list(N = N, mean = t_mean, sd = t_sd, min = t_min, max = t_max, range = t_range,
         skew = skew, exkurt = exkurt, cor = t_cor, reg = t_reg, se = t_se)
    }
  int_d = as.data.frame(lapply(d, round))
  uni = c(apply(int_d, 2, function(col) length(unique(col))))
  if (any(uni < 2)) {
    warning("Some integer-cast predictors have fewer than 2 unique values; skewness and kurtosis targets will be NA.")
  }
  t_int = extract(int_d)
  t_cont = extract(d)
  t_int$uni = uni

  list(
  cont =  t_cont,
  int = t_int)
}

# apply optim_mlr
apply_optim_mlr <- function(targets, tol, dec, keep = FALSE) {

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
        error = function(e) list(best_error = Inf, status_mlr = "infeasible",
                                 message = conditionMessage(e))
      )
      if(any(out$optim_vec$best_error > tol)) {
        out$status_vec = "fail"
      } else {
        out$status_vec = "ok"
      }
      if (!keep) {
      out$track_error       <- NULL
      out$track_error_ratio <- NULL
      out$inputs            <- NULL
      out$optim_vec         <- NULL
      out$data              <- NULL
      }
      out
    }

    cont = run_one(targets$cont, FALSE)
    int <- if (any(targets$int$uni < 2)) {
      list(best_error = NA, status = "degenerate")
    } else {
      run_one(targets$int, TRUE)
    }

  list(cont = cont, int = int)
}

# MLR MC parallelized
sim_optim_mlr_mc <- function(N = 300, n.cond, tol, dec, R, seed,
                             keep_full = 0) {

  set.seed(seed)
  grid = list()
  for (i in 1:n.cond) {
    sim_dat = sim_mlr_data(N)
    dat = sim_dat$data
    true_R2 = sim_dat$true_R2
    true_betas = sim_dat$true_betas
    true_sigma = sim_dat$true_sigma
    true_moments = sim_dat$true_moments
    mlr_targ = mlr_targets(dat, dec)
    grid[[i]] = list(data = dat, targets = mlr_targ, true_R2 = true_R2,
                     true_betas = true_betas, true_sigma = true_sigma,
                     true_moments = true_moments)
  }

  target_grid = lapply(seq_len(n.cond), function(x) {grid[[x]]$targets})

  out_cont = out_int = vec_cont = vec_int = list()
  for (i in 1:n.cond) {
    i_t = target_grid[[i]]

    results = future_lapply(1:R, function(r) {
      res  <- apply_optim_mlr(i_t, tol, dec, keep = FALSE)
      cont_err <- res$cont$best_error
      int_err  <- res$int$best_error
      cont_vec = res$cont$status
      int_vec = res$int$status
      list(cont_err = cont_err, int_err = int_err, cont_vec = cont_vec, int_vec = int_vec)
    }, future.seed = TRUE)

    out_cont[[i]] = do.call(rbind, lapply(results, `[[`, "cont_err"))
    out_int[[i]]  = do.call(rbind, lapply(results, `[[`, "int_err"))
    vec_cont[[i]] = do.call(rbind, lapply(results, `[[`, "cont_vec"))
    vec_int[[i]]  = do.call(rbind, lapply(results, `[[`, "int_vec"))
    cat("Progress: ", i/n.cond," complete \n")
  }

  data         = lapply(grid, '[[', "data")
  true_R2      = sapply(grid, '[[', "true_R2")
  true_sigma   = sapply(grid, '[[', "true_sigma")
  true_betas   = t(sapply(grid, '[[', "true_betas"))
  true_moments = lapply(grid, '[[', "true_moments")
  int_target   = lapply(target_grid, '[[', "int")
  cont_target  = lapply(target_grid, '[[', "cont")

  combined_int <- lapply(names(int_target[[1]]), function(nm) {
    do.call(rbind, lapply(int_target, `[[`, nm))
  })
  combined_cont <- lapply(names(cont_target[[1]]), function(nm) {
    do.call(rbind, lapply(cont_target, `[[`, nm))
  })
  names(combined_int)  = names(int_target[[1]])
  names(combined_cont) = names(cont_target[[1]])

  cont_err = do.call(cbind, out_cont)
  int_err  = do.call(cbind, out_int)
  vec_cont = do.call(cbind, vec_cont)
  vec_int  = do.call(cbind, vec_int)

  cond_names = sprintf("cond_%03d", seq_len(n.cond))
  colnames(cont_err) = cond_names
  colnames(int_err)  = cond_names
  colnames(vec_cont) = cond_names
  colnames(vec_int)  = cond_names
  combined_cont$range <- NULL
  combined_int$range <- NULL

  full_reps <- NULL
  if (keep_full > 0L) {
    keep_full <- min(keep_full, R)
    full_reps <- lapply(seq_len(keep_full), function(r) {
      res_r <- lapply(target_grid, function(t) apply_optim_mlr(t, tol, dec, keep = TRUE))
      names(res_r) <- cond_names
      list(
        data    = setNames(grid,        cond_names),
        targets = setNames(target_grid, cond_names),
        results = res_r
      )
    })
  }

  list(
    data         = data,
    true_R2      = true_R2,
    true_sigma   = true_sigma,
    true_betas   = true_betas,
    true_moments = true_moments,
    int_target   = combined_int,
    cont_target  = combined_cont,
    cont_err     = cont_err,
    int_err      = int_err,
    cont_vec     = vec_cont,
    int_vec      = vec_int,
    full_reps    = full_reps
  )
}

# plot example
plot_mlr_example <- function(lm_mc, replication = 1, condition = NULL,
                             type = c("cont", "int"),
                             geom = c("point", "density"), bins = NULL) {
  type <- match.arg(type)
  geom <- match.arg(geom)

  rep <- lm_mc$full_reps[[replication]]
  if (is.null(rep))
    stop("No `full_reps` stored. Re-run sim_optim_mlr_mc(..., keep_full >= ",
         replication, ").", call. = FALSE)
  if (is.null(condition)) condition <- names(rep$results)[1]

  res  <- rep$results[[condition]][[type]]
  targ <- rep$targets[[condition]][[type]]
  orig_df <- rep$data[[condition]]$data
  if (is.null(res) || identical(res$status, "degenerate"))
    stop("The ", type, " example for ", condition,
         " is degenerate; nothing to plot.", call. = FALSE)
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
      p <- p +
        geom_point(alpha = 0.7, size = 1.2, shape = 16) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 0.7)
    p + scale_colour_manual(values = c("Original" = "grey80", "Simulation" = "grey30")) +
      labs(title = paste0(v, " | Others"), x = paste0(v, " partial"),
           y = "y partial", colour = NULL) +
      theme_sim() +
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
    layer <- if (geom == "point") {
      geom_histogram(bins = b, alpha = 0.7, position = "identity",
                     colour = "white", linewidth = 0.2)
    } else {
      geom_density(alpha = 0.5, linewidth = 0.4)
    }

    ggplot(df, aes(x = value, fill = source)) +
      layer +
      scale_fill_manual(values = c("Original" = "grey80", "Simulation" = "grey30")) +
      labs(title = paste0(v, " Marginal"), x = NULL, y = NULL, fill = NULL) +
      theme_sim() +
      jtools::theme_apa() +
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank())
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



extract_mc_errors_mlr <- function(mc_res) {

  req  <- c("cont_err", "int_err", "cont_vec", "int_vec", "true_R2")
  miss <- req[!req %in% names(mc_res)]
  if (length(miss) > 0L)
    stop("`mc_res` is missing required component(s): ",
         paste(miss, collapse = ", "), ".", call. = FALSE)

  cont_err <- as.matrix(mc_res$cont_err)   # rows = replications, cols = conditions
  int_err  <- as.matrix(mc_res$int_err)
  cont_vec <- as.matrix(mc_res$cont_vec)   # vector-module convergence status
  int_vec  <- as.matrix(mc_res$int_vec)
  if (!identical(dim(cont_err), dim(int_err)))
    stop("`cont_err` and `int_err` must share the same dimensions.", call. = FALSE)
  if (!identical(dim(cont_vec), dim(cont_err)) ||
      !identical(dim(int_vec),  dim(cont_err)))
    stop("`cont_vec`/`int_vec` must share the dimensions of `cont_err`.", call. = FALSE)

  n_rep  <- nrow(cont_err)
  n_cond <- ncol(cont_err)

  cond_names <- colnames(cont_err)
  if (is.null(cond_names)) cond_names <- sprintf("cond_%03d", seq_len(n_cond))
  if (length(mc_res$true_R2) != n_cond)
    stop("length(true_R2) (", length(mc_res$true_R2),
         ") must equal the number of conditions (", n_cond, ").", call. = FALSE)

  # condition index aligned to the column-major unrolling of the matrices
  cond_idx <- rep(seq_len(n_cond), each = n_rep)

  out <- data.frame(
    condition   = factor(cond_names[cond_idx], levels = cond_names),
    replication = rep(seq_len(n_rep), times = n_cond),
    R2          = rep(as.numeric(mc_res$true_R2), each = n_rep),
    cont_error  = as.vector(cont_err),
    int_error   = as.vector(int_err),
    cont_vec    = as.vector(cont_vec),
    int_vec     = as.vector(int_vec),
    stringsAsFactors = FALSE
  )

  out[, c("condition", "replication", "R2",
          "cont_error", "int_error",
          "cont_vec", "int_vec")]
}

#### Plotting ####
source("/Users/lortz/Desktop/PhD/Research/simdata/stats2data/data-raw/moment_plane_graph.R")

# build a moment plane on the exiting plot
make_heat <- function(skew, exkurt, dist) {
  data.frame(skewness2 = skew^2, kurtosis = exkurt + 3, dist = dist) |>
    filter(is.finite(skewness2), is.finite(kurtosis), is.finite(dist),
           skewness2 <= 4, skewness2 > .001, kurtosis >= 1, kurtosis <= 10)   # stay inside the panel
}

plot_cf_heat <- function(heat, discrete, col_mid, limits = NULL) {
  cullen_frey_template(discrete = discrete, base_size = 11) +
    ggnewscale::new_scale_fill() +
    stat_summary_2d(
      data        = heat,
      mapping     = aes(x = skewness2, y = kurtosis, z = dist),
      fun         = mean,
      bins        = 50,
      inherit.aes = FALSE,
      alpha       = 0.7
    ) +
    scale_fill_gradient2(
      low = "#009E73",
      mid = "#E69F00",
      high = "#CC79A7",
      midpoint = col_mid,
      limits   = limits          # <- the only addition
    )
}

#### Run Simulation ####
cat("Simulation starting...\n\n")

# VEC
if (run_vec) {
st = Sys.time()
vec_mc_res <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 1e5, seed = seed, keep_full = 0)
saveRDS(vec_mc_res, file.path(save_dir, "vec_mc_res"))
#check_vec_conv(vec_mc_res, tol = 0.005)
en = Sys.time()
cat("Time diff: ", en-st, "\n\n")

cat("VEC module done!\n\n")
rm(vec_mc_res)
}

# MLR
if (run_mlr) {
st = Sys.time()
lm_mc_res <- sim_optim_mlr_mc(N = 300, n.cond = 500, tol = 0.005, dec = 2, R = 100, seed = seed)
saveRDS(lm_mc_res, file.path(save_dir, "lm_mc_res"))
en = Sys.time()
cat("Time diff: ", en-st,"\n\n")

cat("MLR module done!\n\n")
rm(lm_mc_res)
}
stop("COMPLETED SIMULATION")

#### VEC Results ####
if (!HPC) {
  options(scipen = 50)

  ## vec module ##
  vec_mc_res <- readRDS(file.path(save_dir,"vec_mc_res"))
  d <- vec_mc_res$summary

  max(vec_mc_res$summary$int_err) < .005
  max(vec_mc_res$summary$cont_err) < .005
  # all converged!

  #### empty plane ####
  empty_plane = cullen_frey_template(FALSE, base_size = 11)+
    theme(legend.position = "right")
  ggsave(
    filename = "data-raw/plots/empty_plane.pdf",
    plot     = empty_plane,
    width    = 150,
    height   = 150,
    units    = "mm",
    bg       = "white",
    dpi = 300
  )

  #### overlay moments ####
  # helper: build the two-group overlay frame on the template's axes
  make_overlay <- function(true_skew, true_exkurt, ach_skew, ach_exkurt) {
    bind_rows(
      data.frame(skewness2 = true_skew^2, kurtosis = true_exkurt + 3, moments = "True"),
      data.frame(skewness2 = ach_skew^2,  kurtosis = ach_exkurt + 3,  moments = "Achieved")
    ) |>
      filter(is.finite(skewness2), is.finite(kurtosis)) |>   # drop degenerate / NA
      mutate(moments = factor(moments, levels = c("True", "Achieved")))
  }

  # helper: overlay on the template
  plot_cf_overlay <- function(overlay, discrete) {
    cullen_frey_template(discrete = discrete, base_size = 11) +
      geom_point(
        data        = overlay,
        mapping     = aes(x = skewness2, y = kurtosis, colour = moments),
        inherit.aes = FALSE,
        size = 0.5, alpha = 0.1, na.rm = TRUE,
        show.legend = FALSE
      ) +
      scale_colour_manual(
        values = c("True" = "grey80", "Achieved" = "grey30")
      )
  }
  overlay_cont <- make_overlay(d$true_skew, d$true_exkurt,
                               d$cont_ach_skew, d$cont_ach_exkurt)
  p_cont <- plot_cf_overlay(overlay_cont, discrete = FALSE) +
    theme(legend.position = "right")

  overlay_int <- make_overlay(d$int_target_skew, d$int_target_exkurt,
                              d$int_ach_skew, d$int_ach_exkurt)
  p_int <- plot_cf_overlay(overlay_int, discrete = TRUE) +
    theme(legend.position = "right")

  comb_plane = p_cont + p_int
  comb_plane

  ggsave(
    filename = "data-raw/plots/comb_plane.pdf",
    plot     = comb_plane,
    width    = 300,
    height   = 150,
    units    = "mm",
    bg       = "white",
    dpi = 300
  )

  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # ---- Continuous: standardized quantile distance, at the true Pearson moments ----
  heat_cont <- make_heat(d$true_skew, d$true_exkurt, d$cont_ach_dist)
  ph_cont   <- plot_cf_heat(heat_cont, discrete = FALSE, col_mid = .2) +
    theme(legend.position = "right")

  # ---- Integer: ECDF distance, at the rounded-sample target moments ----
  heat_int  <- make_heat(d$int_target_skew, d$int_target_exkurt, d$int_ach_dist)
  ph_int    <- plot_cf_heat(heat_int, discrete = TRUE, col_mid = .05) +
    theme(legend.position = "right")

  comb_heat_vec = ph_cont + ph_int

  ggsave(
    filename = "data-raw/plots/comb_heat_vec.pdf",
    plot     = comb_heat_vec,
    width    = 300,
    height   = 150,
    units    = "mm",
    bg       = "white",
    dpi = 300
  )


}

#### MLR Results ####
if (!HPC) {

  lm_mc_res <- readRDS(file.path(save_dir,"lm_mc_res"))
  lm_results <- extract_mc_errors_mlr(lm_mc_res)
  conv_cond = lm_results %>%
    group_by(condition) %>%
    summarize(cont.conv = mean(min(cont_error) < .005),
              int.conv = mean(min(int_error) < .005),
              cont.vec = mean(cont_vec == "ok"),
              int.vec  = mean(int_vec == "ok"))

  any(lm_results$cont_vec != "ok")
  any(lm_results$int_vec != "ok")
  # vec converged in all runs!

  # Overall performance
  round(mean(lm_mc_res$cont_err < .005)*100)
  round(mean(lm_mc_res$int_err < .005)*100)


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

  cont_err_med <- apply(lm_mc_res$cont_err, 2, mean, na.rm = TRUE)
  int_err_med  <- apply(lm_mc_res$int_err,  2, mean, na.rm = TRUE)

  rng <- range(c(heat_cont_mlr$dist, heat_int_mlr$dist), na.rm = TRUE)
  # pass limits = rng into scale_fill_gradient2() in plot_cf_heat()

  heat_cont_mlr <- make_heat(
    skew   = as.vector(lm_mc_res$cont_target$skew),
    exkurt = as.vector(lm_mc_res$cont_target$exkurt),
    dist   = rep(cont_err_med, times = ncol(lm_mc_res$cont_target$skew))
  )
  heat_int_mlr <- make_heat(
    skew   = as.vector(lm_mc_res$int_target$skew),
    exkurt = as.vector(lm_mc_res$int_target$exkurt),
    dist   = rep(int_err_med, times = ncol(lm_mc_res$int_target$skew))
  )

  ph_cont_mlr <- plot_cf_heat(heat_cont_mlr, discrete = FALSE, col_mid = 0.007, limits = rng) +
    theme(legend.position = "right")
  ph_int_mlr  <- plot_cf_heat(heat_int_mlr,  discrete = TRUE,  col_mid = 0.007, limits = rng) +
    theme(legend.position = "right")

  comb_heat_mlr <- ph_cont_mlr + ph_int_mlr
  comb_heat_mlr

  ggsave(
    filename = "data-raw/plots/comb_heat_mlr.pdf",
    plot     = comb_heat_mlr,
    width    = 300,
    height   = 150,
    units    = "mm",
    bg       = "white",
    dpi = 300
  )

  type_pal <- c("Continuous" = "grey30", "Integer" = "grey30")

  # Plot floor
  EPS_FLOOR <- 1e-8
  TOL_ABS   <- 0.005
  TOL_REL   <- 0.01

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

  # Caterpillar plot --------------------------------------
  # Condition-level uncertainty: median +/- IQR, conditions ranked.
  # Ranked within each panel; we are NOT inviting a cross-panel positional
  # comparison.

  cond_summary <- lm_results %>%
    group_by(condition) %>%
    summarise(
      cont_med = median(cont_error, na.rm = TRUE),
      cont_q05 = quantile(cont_error, 0.0, na.rm = TRUE),
      cont_q95 = quantile(cont_error, 1, na.rm = TRUE),
      int_med  = median(int_error,  na.rm = TRUE),
      int_q05  = quantile(int_error, 0.0, na.rm = TRUE),
      int_q95  = quantile(int_error, 1, na.rm = TRUE),
      .groups  = "drop"
    )

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

  panel_A <- ggplot(cat_df, aes(x = rank, colour = data_type)) +
    geom_linerange(aes(ymin = pmax(q05, EPS_FLOOR),
                       ymax = pmax(q95, EPS_FLOOR)),
                   linewidth = 0.25, alpha = 0.55) +
    geom_point(aes(y = pmax(med, EPS_FLOOR)),
               size = 0.55, alpha = 0.9) +
    geom_hline(yintercept = .005, linetype = "dashed") +
    #scale_y_log10() +
    scale_colour_manual(values = type_pal, guide = "none") +
    facet_wrap(~ data_type, ncol = 2) +
    labs(
      title    = "",
      x        = "Condition Ranked by Median",
      y        = ""
    ) +
    theme_sim() +
    jtools::theme_apa()


  # Diagnostic vs target difficulty
  # Same layout/form as Panel C, but conditions placed by R2 on the x-axis.
  diag_df <- lm_results %>%
    group_by(condition, R2) %>%
    summarise(cont_med = median(cont_error, na.rm = TRUE),
              cont_q05 = quantile(cont_error, 0.0, na.rm = TRUE),
              cont_q95 = quantile(cont_error, 1,   na.rm = TRUE),
              int_med  = median(int_error,  na.rm = TRUE),
              int_q05  = quantile(int_error, 0.0, na.rm = TRUE),
              int_q95  = quantile(int_error, 1,   na.rm = TRUE),
              .groups  = "drop")

  diag_cat <- bind_rows(
    diag_df %>%
      transmute(condition, R2,
                data_type = "Continuous",
                med = cont_med, q05 = cont_q05, q95 = cont_q95),
    diag_df %>%
      transmute(condition, R2,
                data_type = "Integer",
                med = int_med, q05 = int_q05, q95 = int_q95)
  ) %>%
    mutate(data_type = factor(data_type, levels = c("Continuous", "Integer")))

  panel_B <- ggplot(diag_cat, aes(x = R2, colour = data_type)) +
    geom_linerange(aes(ymin = pmax(q05, EPS_FLOOR),
                       ymax = pmax(q95, EPS_FLOOR)),
                   linewidth = 0.25, alpha = 0.55) +
    geom_point(aes(y = pmax(med, EPS_FLOOR)),
               size = 0.55, alpha = 0.9) +
    geom_hline(yintercept = .005, linetype = "dashed") +
    #scale_y_log10() +
    scale_colour_manual(values = type_pal, guide = "none") +
    facet_wrap(~ data_type, ncol = 2) +
    labs(
      title    = "",
      x        = expression("Estimated" ~ R^2 ~ "of Condition"),
      y        = ""
    ) +
    theme_sim() +
    jtools::theme_apa()

  # Assemble main figure
  fig_main <- (panel_A / panel_B) +
    plot_layout(heights = c(1, 1))

  # one shared axis title for the whole column
  fig_main <- wrap_elements(fig_main) +
    labs(tag = "Weighted Root Mean Square Error") +
    theme(plot.tag = element_text(angle = 90),
          plot.tag.position = "left")

    # save
    ggsave("data-raw/plots/sim_mlr.pdf",  fig_main, width = 300, height = 200,
           units = "mm", bg = "white", dpi = 300)

    # Print variance partition
    print(var_tbl)

}

#### Example ####
# VEC
vec_exp1 <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 1, seed = seed + 1, keep_full = 1)
vec_exp2 <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 1, seed = seed + 2, keep_full = 1)
vec_exp3 <- sim_optim_vec_mc(N = 300, tol = 0.005, dec = 2, R = 1, seed = seed + 3, keep_full = 1)

p1 <- plot_vec_example(vec_exp1, type = "cont", geom = "density", bins = 50)
p2 <- plot_vec_example(vec_exp2, type = "cont", geom = "density", bins = 50)
p3 <- plot_vec_example(vec_exp3, type = "cont", geom = "density", bins = 50, legend = TRUE)

vec_example <- (p1 | p2 | p3)

ggsave(
  filename = "data-raw/plots/vec_example.pdf",
  plot     = vec_example,
  width    = 300,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

# MLR
mlr_exp <- sim_optim_mlr_mc(N = 300, n.cond = 1, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 1)

mlr_example = plot_mlr_example(mlr_exp, type = "cont", geom = "dens")
ggsave("data-raw/plots/mlr_example.pdf", mlr_example,
       width = 300, height = 200, units = "mm",
       bg = "white", dpi = 300)

