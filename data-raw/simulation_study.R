### Simulation Study from the manuscript

#### Configs ####
options(repos = c(CRAN = "https://ftp.belnet.be/mirror/CRAN/"))
HPC <- FALSE

root_dir <- if (HPC) {
  "/home4/p310779/nds3"
} else {
  "/Users/lortz/Desktop/PhD/Research/simdata/nds3"
}

save_dir <- file.path(root_dir, "data-raw", "results")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

seed     <- 310779

# pkg functions
Rcpp::sourceCpp(file.path(root_dir, "src", "helpers.cpp"))
invisible(lapply(
  list.files(file.path(root_dir, "R"), full.names = TRUE),
  source
))

# libs
library(nds3)
library(ggplot2)
library(patchwork)
library(sn)
library(dplyr)
library(future)
library(future.apply)
library(afex)
library(progressr)

required_pkgs <- c("ggplot2", "patchwork", "sn", "dplyr",
                   "future", "future.apply", "afex", "progressr")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) stop("Missing packages: ", paste(missing, collapse = ", "))

# backend
if (HPC) {
  plan(multisession, workers = availableCores()-5)
} else {
  plan(multisession, workers = 4L)
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
      tolerance   = tol,
      integer     = FALSE,
      sprite_prec = c(dec, dec)
    )
    if (is.null(x$int)) {
      optim_vec_int <- list(best_error = list(NA), status = "degenerate")
    } else {
      optim_vec_int <- optim_vec(
        N = N,
        target_mean = setNames(x$int$mean, "vec"),
        target_sd   = x$int$sd,
        range       = c(x$int$min, x$int$max),
        tolerance   = tol,
        integer     = TRUE,
        sprite_prec = c(dec, dec)
      )
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
  summaries <- future_lapply(seq_len(R), function(r) {
    dat     <- sim_data(N)
    targets <- uni_targets(dat, dec)
    res     <- apply_optim_vec(N, targets, tol, dec)
    conv    <- check_vec_conv(res, tol)

    out <- list(
      summary = list(
        convergence = conv,
        targets     = lapply(targets, function(x) {
          list(cont = x$cont, int = x$int)
        })
      )
    )

    if (r <= keep_full) {
      out$full <- list(
        data = dat, targets = targets,
        results = res, convergence = conv
      )
    }

    out
  }, future.seed = seed)

  list(
    summaries = lapply(summaries, `[[`, "summary"),
    full_reps = Filter(Negate(is.null), lapply(summaries, `[[`, "full"))
  )
}

# extract errors
extract_mc_errors_vec <- function(mc_res) {
  do.call(rbind, lapply(seq_along(mc_res$summaries), function(r) {
    s <- mc_res$summaries[[r]]
    conv <- s$convergence
    dist_names <- rownames(conv)

    do.call(rbind, lapply(seq_along(dist_names), function(i) {
      dist      <- dist_names[i]
      target_sd <- s$targets[[dist]]$cont$sd
      cont_err  <- conv$cont_err[i]
      int_err   <- conv$int_err[i]

      data.frame(
        replication    = r,
        distribution   = dist,
        target_sd      = target_sd,
        cont_error     = cont_err,
        cont_rel_error = cont_err / target_sd,
        int_error      = int_err,
        int_rel_error  = if (is.na(int_err)) NA else int_err / target_sd
      )
    }))
  }))
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
  #cond_names <- c("standard", "extreme1")
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
      round(runif(1, -10, 10), 1),
      round(runif(3, -5, 5), 2),
      round(runif(1, -2, 2), 2)
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
    y <- yhat + rnorm(N, 0, spec$sigma)
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
      tryCatch(
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
          tolerance     = tol,
          progress_mode = "console"
        ),
        error = function(e) list(best_error = Inf, status = "infeasible",
                                 message = conditionMessage(e))
      )
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

# MC MLR parallelized
sim_optim_mlr_mc <- function(N, n.cond, tol, dec, R, seed, keep_full = 1L) {
  conds <- lm_conditions(N, n.cond, seed = seed)

  results <- future_lapply(seq_len(R), function(r) {
    dat  <- sim_mlr_data(N, conds)
    targ <- lm_targets(dat, dec)
    res  <- apply_optim_mlr(targ, tol, dec, conditions = conds)
    conv <- check_mlr_conv(res, tol)
    R2s  <- sapply(dat, function(x) x$R2)

    out <- list(
      summary = list(
        convergence = conv,
        R2 = R2s,
        targets = lapply(targ, function(t) {
          list(cont = t$cont[c("cor", "reg", "se")],
               int  = if (!is.null(t$int)) t$int[c("cor", "reg", "se")] else NULL)
        }),
        errors = lapply(res, function(x) {
          list(
            cont_error = x$cont$best_error,
            int_error  = x$int$best_error,
            cont_ratio = if (!is.null(x$cont$track_error_ratio))
              tail(x$cont$track_error_ratio, 1) else NA_real_,
            int_ratio  = if (!is.null(x$int$track_error_ratio))
              tail(x$int$track_error_ratio, 1) else NA_real_
          )
        })
      )
    )

    if (r <= keep_full) {
      out$full <- list(
        data = dat, targets = targ, results = res,
        convergence = conv, R2 = R2s
      )
    }

    out
  }, future.seed = seed)

  list(
    conditions = conds,
    summaries  = lapply(results, `[[`, "summary"),
    full_reps  = Filter(Negate(is.null), lapply(results, `[[`, "full"))
  )
}

extract_mc_errors_mlr <- function(mc_res) {
  cond_dists <- sapply(mc_res$conditions, function(spec) {
    paste(sapply(spec$x_specs, function(s) s$type), collapse = ", ")
  })

  do.call(rbind, lapply(seq_along(mc_res$summaries), function(r) {
    s <- mc_res$summaries[[r]]
    do.call(rbind, lapply(names(s$errors), function(cond) {
      err  <- s$errors[[cond]]
      targ <- s$targets[[cond]]$cont
      mean_target <- mean(c(abs(targ$cor), abs(targ$reg)), na.rm = TRUE)

      cont_err <- err$cont_error
      int_err  <- err$int_error

      data.frame(
        replication    = r,
        condition      = cond,
        pred_dists     = cond_dists[cond],
        R2             = s$R2[cond],
        target_r2      = mc_res$conditions[[cond]]$target_r2,
        mean_target    = mean_target,
        cont_error     = if (is.finite(cont_err)) cont_err else NA_real_,
        cont_rel_error = if (is.finite(cont_err)) cont_err / mean_target else NA_real_,
        cont_ratio     = err$cont_ratio,
        int_error      = if (!is.na(int_err) && is.finite(int_err)) int_err else NA_real_,
        int_rel_error  = if (!is.na(int_err) && is.finite(int_err)) int_err / mean_target else NA_real_,
        int_ratio      = err$int_ratio
      )
    }))
  }))
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


#### ANOVA Module ####

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

  make_one <- function() {
    cell_dists <- sample(dist_names, 4, replace = TRUE)
    cell_pars  <- lapply(cell_dists, function(d) pars[[d]][["standard"]])

    pilot_sds <- sapply(1:4, function(k) {
      p <- cell_pars[[k]]
      p$n <- S
      sd(data_gen(cell_dists[k], p))
    })
    error_sd <- mean(pilot_sds)

    grand_mean <- round(runif(1, 5, 20), 2)
    target_F   <- runif(3, 0.5, 10)

    list(
      cell_dists = cell_dists,
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
sim_aov_data <- function(S, conds) {
  pars <- pars_conditions(S)

  setNames(lapply(names(conds), function(cond_name) {
    spec <- conds[[cond_name]]

    gen_cell_errors <- function(k, n) {
      p <- spec$cell_pars[[k]]
      p$n <- n
      e <- data_gen(spec$cell_dists[k], p)
      e - mean(e)
    }

    setNames(lapply(names(aov_designs), function(des_name) {
      design <- aov_designs[[des_name]]

      # equalize total observations
      if (all(design$factor_type == "between")) {
        S_design   <- S
        n_per_cell <- S %/% 4
      } else if (all(design$factor_type == "within")) {
        S_design   <- S %/% 4
        n_per_cell <- S_design
      } else {
        S_design   <- S %/% 2
        n_per_cell <- S_design %/% 2
      }

      if (all(design$factor_type == "between")) {
        denom <- n_per_cell
      } else if (all(design$factor_type == "within")) {
        denom <- S_design
      } else {
        denom <- n_per_cell
      }

      eff_F1  <- round(sqrt(spec$target_F[1] * spec$error_sd^2 / denom) * sample(c(-1, 1), 1), 2)
      eff_F2  <- round(sqrt(spec$target_F[2] * spec$error_sd^2 / denom) * sample(c(-1, 1), 1), 2)
      eff_int <- round(sqrt(spec$target_F[3] * spec$error_sd^2 / denom) * sample(c(-1, 1), 1), 2)

      cell_means <- spec$grand_mean + c(
        -eff_F1 - eff_F2 + eff_int,
        -eff_F1 + eff_F2 - eff_int,
        +eff_F1 - eff_F2 - eff_int,
        +eff_F1 + eff_F2 + eff_int
      )

      n1 <- round(S_design * 0.6)
      n2 <- S_design - n1

      if (all(design$factor_type == "between")) {
        cell_sizes <- c(round(n1 * 0.6), n1 - round(n1 * 0.6),
                        round(n2 * 0.6), n2 - round(n2 * 0.6))
        id_offset <- 0L
        dat <- do.call(rbind, lapply(1:4, function(k) {
          f1 <- c(1, 1, 2, 2)[k]
          f2 <- c(1, 2, 1, 2)[k]
          nk <- cell_sizes[k]
          y  <- cell_means[k] + gen_cell_errors(k, nk)
          ids <- id_offset + seq_len(nk)
          id_offset <<- id_offset + nk
          data.frame(ID = ids, Factor1 = factor(f1), Factor2 = factor(f2), outcome = y)
        }))

      } else if (all(design$factor_type == "within")) {
        subj_eff <- rnorm(S_design, 0, runif(1, 0.5, 3))
        dat <- do.call(rbind, lapply(1:4, function(k) {
          f1 <- c(1, 1, 2, 2)[k]
          f2 <- c(1, 2, 1, 2)[k]
          y  <- cell_means[k] + subj_eff + gen_cell_errors(k, S_design)
          data.frame(ID = seq_len(S_design), Factor1 = factor(f1), Factor2 = factor(f2), outcome = y)
        }))

      } else {
        dat <- do.call(rbind, lapply(1:2, function(b) {
          n_b <- if (b == 1) n1 else n2
          subj_eff <- rnorm(n_b, 0, runif(1, 0.1, 1))
          subj_ids <- if (b == 1) seq_len(n1) else n1 + seq_len(n2)
          do.call(rbind, lapply(1:2, function(w) {
            k <- (b - 1) * 2 + w
            y <- cell_means[k] + subj_eff + gen_cell_errors(k, n_b)
            data.frame(ID = subj_ids, Factor1 = factor(b), Factor2 = factor(w), outcome = y)
          }))
        }))
      }

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
        tryCatch(
          optim_aov(S = t$S, levels = t$levels, target_group_means = t$group_means,
                    subgroup_sizes = t$subgroup_sizes,
                    target_f_list = list(effect = t$effect_names, F = t$f_vals),
                    integer = is_int, range = t$range, formula = t$formula,
                    factor_type = t$factor_type, tolerance = tol,
                    progress_mode = "console"),
          error = function(e) list(best_error = Inf, status = "infeasible",
                                   message = conditionMessage(e))
        )
      }
      cont <- run_one(targ$cont, FALSE)
      int  <- if (is.null(targ$int)) list(best_error = NA, status = "degenerate") else run_one(targ$int, TRUE)
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

# MC AOV parallelized
sim_optim_aov_mc <- function(S, n.cond, tol, dec, R, seed, keep_full = 1L) {
  conds <- aov_conditions(S, n.cond, seed = seed)

  results <- future_lapply(seq_len(R), function(r) {
    dat  <- sim_aov_data(S, conds)
    targ <- aov_extract_targets(dat, dec)
    res  <- apply_optim_aov(targ, tol)
    conv <- check_aov_conv(res, tol)

    out <- list(
      summary = list(
        convergence = conv,
        targets = lapply(targ, function(cond_t) {
          lapply(cond_t, function(des_t) {
            list(
              cont_f = des_t$cont$f_vals,
              int_f  = if (!is.null(des_t$int)) des_t$int$f_vals else NULL
            )
          })
        }),
        errors = lapply(res, function(cond_r) {
          lapply(cond_r, function(des_r) {
            list(cont_error = des_r$cont$best_error,
                 int_error  = des_r$int$best_error)
          })
        })
      )
    )

    if (r <= keep_full) {
      out$full <- list(
        data = dat, targets = targ, results = res, convergence = conv
      )
    }

    out
  }, future.seed = seed)

  list(
    conditions = conds,
    summaries  = lapply(results, `[[`, "summary"),
    full_reps  = Filter(Negate(is.null), lapply(results, `[[`, "full"))
  )
}

# extract errors for aov
extract_mc_errors_aov <- function(mc_res) {
  cond_dists <- sapply(mc_res$conditions, function(spec) {
    paste(spec$cell_dists, collapse = ", ")
  })

  do.call(rbind, lapply(seq_along(mc_res$summaries), function(r) {
    s <- mc_res$summaries[[r]]
    do.call(rbind, lapply(names(s$errors), function(cond) {
      do.call(rbind, lapply(names(s$errors[[cond]]), function(des) {
        targ_f   <- s$targets[[cond]][[des]]$cont_f
        mean_f   <- mean(targ_f, na.rm = TRUE)
        cont_err <- s$errors[[cond]][[des]]$cont_error
        int_err  <- s$errors[[cond]][[des]]$int_error

        data.frame(
          replication    = r,
          condition      = cond,
          design         = des,
          cell_dists     = cond_dists[cond],
          mean_target_F  = mean_f,
          cont_error     = if (is.finite(cont_err)) cont_err else NA_real_,
          cont_rel_error = if (is.finite(cont_err)) cont_err / mean_f else NA_real_,
          int_error      = if (!is.na(int_err) && is.finite(int_err)) int_err else NA_real_,
          int_rel_error  = if (!is.na(int_err) && is.finite(int_err)) int_err / mean_f else NA_real_
        )
      }))
    }))
  }))
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

# VEC
vec_mc_res <- sim_optim_vec_mc(N = 500, tol = 0.005, dec = 2, R = 10000, seed = seed, keep_full = 1)
saveRDS(vec_mc_res, file.path(save_dir, "vec_mc_res"))
cat("VEC module done!\n\n")

# MLR
lm_mc_res <- sim_optim_mlr_mc(N = 500, n.cond = 100, tol = 0.005, dec = 2, R = 100, seed = seed, keep_full = 1)
saveRDS(lm_mc_res, file.path(save_dir, "lm_mc_res"))
cat("MLR module done!\n\n")

# AOV
aov_mc_res <- sim_optim_aov_mc(S = 250, n.cond = 100, tol = 0.005, dec = 2, R = 100, seed = seed, keep_full = 1)
saveRDS(aov_mc_res, file.path(save_dir, "aov_mc_res"))
cat("AOV module done!\n\n")

#mean_check <- check_aov_means(aov_mc)
#mean_check[mean_check$pass, ]

#aov_mc$replications[[1]]$targets$cond10$mixed$cont$f_vals



#### Results ####
if (!HPC) {
options(scipen = 50)

## vec module ##
vec_results <- extract_mc_errors_vec(vec_mc_res)

# manuscript table
vec_results %>%
  summarise(
    median_cont_rel = median(cont_rel_error, na.rm = TRUE) * 100,
    median_int_rel  = median(int_rel_error, na.rm = TRUE) * 100,
    cont_succ_rate  = mean(cont_rel_error < 0.01, na.rm = TRUE) * 100,
    int_succ_rate   = mean(int_rel_error < 0.01, na.rm = TRUE) * 100,
    cont_conv_rate  = mean(cont_error < 0.0005, na.rm = TRUE) * 100,
    int_conv_rate   = mean(int_error < 0.0005, na.rm = TRUE) * 100
  )

# plots
vec.plot    <- plot_vec_example(vec_mc_res, rep_index = 1, filter = "extreme1", type = "cont")
ggsave(
  filename = "data-raw/plots/vec.plot.pdf",
  plot     = vec.plot,
  width    = 200,
  height   = 200,
  units    = "mm",
  bg       = "white",
  dpi = 300
)


## MLR module ##
lm_results <- extract_mc_errors_mlr(lm_mc_res)

# manuscript table
lm_results %>%
  summarise(
    median_cont_rel = median(cont_rel_error, na.rm = TRUE) * 100,
    median_int_rel  = median(int_rel_error, na.rm = TRUE) * 100,
    cont_succ_rate  = mean(cont_rel_error < 0.01, na.rm = TRUE) * 100,
    int_succ_rate   = mean(int_rel_error < 0.01, na.rm = TRUE) * 100,
    cont_conv_rate  = mean(cont_error < 0.005, na.rm = TRUE) * 100,
    int_conv_rate   = mean(int_error < 0.005, na.rm = TRUE) * 100
  )

# manuscript plot
lm.plot <- plot_mlr_example(lm_mc_res, 1, "cond1", "cont", geom = "point")
plot_mlr_example(lm_mc_res, 1, "cond10", "int", geom = "point")
ggsave(
  filename = "data-raw/plots/lm.plot.pdf",
  plot     = lm.plot,
  width    = 300,
  height   = 200,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

## AOV module ##

aov_mc_results <- extract_mc_errors_aov(aov_mc_res)

# manuscript table
aov_mc_results %>%
  group_by(design) %>%
  summarise(
    med_cont      = median(cont_rel_error[is.finite(cont_error)]) * 100,
    med_int       = median(int_rel_error[is.finite(int_error)], na.rm = TRUE) * 100,
    cont_succ_rate = mean(is.finite(cont_error) & cont_rel_error < 0.01) * 100,
    int_succ_rate  = mean(is.finite(int_error) & int_rel_error < 0.01, na.rm = TRUE) * 100,
    cont_conv_rate = mean(is.finite(cont_error) & cont_error < 0.005) * 100,
    int_conv_rate  = mean(is.finite(int_error) & int_error < 0.005, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# manuscript plot
aov.plot <- plot_aov_example(aov_mc_res, 1, "cond1", "mixed", "cont", geom = "histogram", bins = 30)

ggsave(
  filename = "data-raw/plots/aov.plot.pdf",
  plot     = aov.plot,
  width    = 200,
  height   = 250,
  units    = "mm",
  bg       = "white",
  dpi = 300
)

}
