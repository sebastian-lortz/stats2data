### Simulation Study from the manuscript
# HPC
# 72 hours
# 108 cores
# 100GB

#### Configs ####
options(repos = c(CRAN = "https://ftp.belnet.be/mirror/CRAN/"))
HPC <- TRUE
job = 88
n.jobs = 100

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

seed     <- 310779 + job

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

# backend
if (HPC) {
  plan(multisession, workers = availableCores()-5)
} else {
  plan(multisession, workers = 6L)
}



#### ANOVA Module ####

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


# Generate data: for each condition × each design
sim_aov_data <- function(n = 75L, icc = 0.3) {

  cell_moments <- sample_moments(4)
  sigma_e      <- mean(cell_moments$sigma)
  cell_err <- function(k) {
    scale(sample_data(cell_moments[k, ], n)[[1]],
          scale = FALSE)
  }

  CELL_F1 <- c(1, 1, 2, 2)
  CELL_F2 <- c(1, 2, 1, 2)

  f_main <- runif(2, 0.02, 0.35)
  f_int  <- runif(1, 0.02, 0.35)
  f_coh  <- c(f_main, f_int)

  a <- f_main[1] * sigma_e
  b <- f_main[2] * sigma_e
  d <- f_int    * sigma_e

  cell_means <- c(-a - b + d,
                  -a + b - d,
                  a - b - d,
                  a + b + d)

  subject_sd   <- sqrt(icc / (1 - icc)) * sigma_e

  designs <- setNames(lapply(names(aov_designs), function(des_name) {
    design <- aov_designs[[des_name]]
    has_w  <- any(design$factor_type == "within")
    # bg[k] = between-group that cell k belongs to
    bg <- if (all(design$factor_type == "between")) {1:4
      } else if (all(design$factor_type == "within")) {c(1, 1, 1, 1)
        } else {CELL_F1}   # mixed
    subj_re <- if (has_w) {rnorm(max(bg) * n, 0, subject_sd)
      } else {rep(0, max(bg) * n)}

    dat <- do.call(rbind, lapply(1:4, function(k) {
      ids <- ((bg[k] - 1) * n + 1):(bg[k] * n)
      data.frame(ID      = ids,
                 Factor1 = factor(CELL_F1[k]),
                 Factor2 = factor(CELL_F2[k]),
                 outcome = cell_means[k] + subj_re[ids] + cell_err(k))
    }))
    dat$ID <- factor(dat$ID)
    list(data = dat, design = design)
  }), names(aov_designs))

  list(designs = designs, true_coh_f = f_coh, true_means = cell_means,
       true_moments = cell_moments, subject_sd = subject_sd)
}

# d = sim_aov_data(n = 75, icc = 0.3, eff_scale = 3)

# extract aov targets
aov_extract_targets <- function(d, dec) {
  lapply(d$designs, function(x) {
    dat    <- x$data
    design <- x$design

    extract <- function(d) {
      # cell means in order (1,1)(1,2)(2,1)(2,2)
      cm <- aggregate(outcome ~ Factor1 + Factor2, data = d, FUN = mean)
      cm <- cm[order(cm$Factor1, cm$Factor2), ]
      cell_means <- round(cm$outcome, dec)

      fit    <- suppressMessages(afex::aov_car(design$formula_afex, data = d))
      f_vals <- round(fit$anova_table$F, dec)
      rng    <- round(c(min(d$outcome), max(d$outcome)), dec)

      S <- length(unique(d$ID))
      subgroup_sizes <- if (all(design$factor_type == "between")) rep(S %/% 4L, 4L)
      else if (all(design$factor_type == "within"))  NULL
      else                                            rep(S %/% 2L, 2L)

      list(S = S, levels = design$levels,
           subgroup_sizes = subgroup_sizes, factor_type = design$factor_type,
           group_means = cell_means, f_vals = f_vals,
           effect_names = rownames(fit$anova_table),
           formula = design$formula_optim, range = rng)
    }
    cont <- extract(dat)

    dat_int <- dat
    dat_int$outcome <- round(dat_int$outcome)
    uni <- aggregate(outcome ~ Factor1 + Factor2, data = dat_int,
                     FUN = function(v) length(unique(v)))
    int <- if (any(uni$outcome < 2)) NULL else extract(dat_int)

    list(cont = cont, int = int)
  })
}
#test = aov_extract_targets(d = sim_aov_data(n = 75, icc = 0.3, eff_scale = 3), dec = 2)
#test$between$cont$f_vals

# optim aov
apply_optim_aov <- function(targets, tol, keep = FALSE) {
  lapply(targets, function(targ) {
    run_one <- function(t, is_int) {
      res <- tryCatch(
        optim_aov(S = t$S, levels = t$levels,
                  target_group_means = t$group_means,
                  subgroup_sizes = t$subgroup_sizes,
                  target_f_list = list(effect = t$effect_names, F_value = t$f_vals),
                  integer = is_int, range = t$range, formula = t$formula,
                  factor_type = t$factor_type, thresh = tol,
                  progress_mode = "off"),
        error = function(e) list(best_error = Inf, status = "infeasible",
                                 message = conditionMessage(e))
      )
      if (!keep) {
        res$track_error       <- NULL
        res$inputs            <- NULL
        res$data              <- NULL
      }
      res
    }
    cont <- run_one(targ$cont, FALSE)
    int  <- if (is.null(targ$int)) {
      list(best_error = NA, status = "degenerate")
    } else {
      run_one(targ$int, TRUE)
    }
    list(cont = cont, int = int)
  })
}

# test.aov = apply_optim_aov(targets = aov_extract_targets(d = sim_aov_data(n = 75, icc = 0.3, eff_scale = 3), dec = 2), tol = 0.005)

# aov simulation parallel
sim_optim_aov_mc <- function(n=75, n.cond = 100, tol=.005, dec = 2, R, seed) {

  set.seed(seed)

  pivot_targ <- function(lst, varying = c("group_means", "f_vals", "range")) {
    varying <- intersect(varying, names(lst[[1]]))
    static  <- setdiff(names(lst[[1]]), varying)
    c(setNames(lapply(varying, function(nm)
      do.call(rbind, lapply(lst, `[[`, nm))), varying),
      lst[[1]][static])
  }

  grid = list()
  for (i in 1:n.cond) {
    sim_dat = sim_aov_data(n)
    dat = list(
      between = sim_dat$designs$between$data,
      within = sim_dat$designs$within$data,
      mixed = sim_dat$designs$mixed$data)

    subject_sd = sim_dat$subject_sd
    true_means = sim_dat$true_means
    true_coh_f = sim_dat$true_coh_f
    true_moments = sim_dat$true_moments

    aov_targ = aov_extract_targets(d = sim_dat, dec = dec)

    grid[[i]] = list(data = dat, targets = aov_targ, subject_sd = subject_sd,
                     true_means = true_means, true_coh_f = true_coh_f,
                     true_moments = true_moments)
  }

  target_grid = lapply(grid, "[[", "targets")
  grid <- lapply(grid, function(x) {
    x$targets <- NULL
    x})
  out_b_cont = out_b_int = out_w_cont = out_w_int = out_m_cont = out_m_int = list()

  for (i in 1:n.cond) {
    i_t = target_grid[[i]]

    results = future_lapply(1:R, function(r) {
      res  <- apply_optim_aov(i_t, tol, keep = FALSE)
      b_cont_err <- res$between$cont$best_error
      w_cont_err <- res$within$cont$best_error
      m_cont_err <- res$mixed$cont$best_error
      b_int_err  <- res$between$int$best_error
      w_int_err  <- res$within$int$best_error
      m_int_err  <- res$mixed$int$best_error
      list(b_cont_err = b_cont_err, w_cont_err = w_cont_err,
            m_cont_err = m_cont_err, b_int_err = b_int_err,
            w_int_err = w_int_err, m_int_err = m_int_err)
      }, future.seed = TRUE)

    out_b_cont[[i]] = do.call(rbind, lapply(results, `[[`, "b_cont_err"))
    out_w_cont[[i]] = do.call(rbind, lapply(results, `[[`, "w_cont_err"))
    out_m_cont[[i]] = do.call(rbind, lapply(results, `[[`, "m_cont_err"))
    out_b_int[[i]]  = do.call(rbind, lapply(results, `[[`, "b_int_err"))
    out_w_int[[i]]  = do.call(rbind, lapply(results, `[[`, "w_int_err"))
    out_m_int[[i]]  = do.call(rbind, lapply(results, `[[`, "m_int_err"))
    cat("Progress: ", i/n.cond," complete \n")
  }

  b_cont_err = do.call(cbind, out_b_cont)
  w_cont_err = do.call(cbind, out_w_cont)
  m_cont_err = do.call(cbind, out_m_cont)
  b_int_err  = do.call(cbind, out_b_int)
  w_int_err  = do.call(cbind, out_w_int)
  m_int_err  = do.call(cbind, out_m_int)
  cond_names = sprintf("cond_%03d", seq_len(n.cond))
  colnames(b_cont_err) = cond_names
  colnames(w_cont_err)  = cond_names
  colnames(m_cont_err) = cond_names
  colnames(b_int_err)  = cond_names
  colnames(w_int_err)  = cond_names
  colnames(m_int_err)  = cond_names

  data = lapply(grid, '[[', "data")
  true_coh_f = t(sapply(grid, "[[", "true_coh_f"))
  true_means =  t(sapply(grid, "[[", "true_means"))
  true_moments = lapply(grid, '[[', "true_moments")
  subject_sd =  sapply(grid, "[[", "subject_sd")

  b_targ   = lapply(target_grid, '[[', "between")
  b_cont_targ   = lapply(b_targ, '[[', "cont")
  b_int_targ   = lapply(b_targ, '[[', "int") ; rm(b_targ)
  w_targ   = lapply(target_grid, '[[', "within")
  w_cont_targ   = lapply(w_targ, '[[', "cont")
  w_int_targ   = lapply(w_targ, '[[', "int") ; rm(w_targ)
  m_targ   = lapply(target_grid, '[[', "mixed")
  m_cont_targ   = lapply(m_targ, '[[', "cont")
  m_int_targ   = lapply(m_targ, '[[', "int") ; rm(m_targ)

  b_cont_targ <- pivot_targ(b_cont_targ)
  b_int_targ  <- pivot_targ(b_int_targ)
  w_cont_targ <- pivot_targ(w_cont_targ)
  w_int_targ  <- pivot_targ(w_int_targ)
  m_cont_targ <- pivot_targ(m_cont_targ)
  m_int_targ  <- pivot_targ(m_int_targ)

 list(
    data         = data,
    true_coh_f = true_coh_f,
    true_means =  true_means,
    true_moments = true_moments,
    subject_sd =  subject_sd,
    b_cont_targ   = b_cont_targ,
    b_int_targ   = b_int_targ,
    w_cont_targ   = w_cont_targ,
    w_int_targ   = w_int_targ,
    m_cont_targ   = m_cont_targ,
    m_int_targ   = m_int_targ,
    b_cont_err = b_cont_err,
    w_cont_err = w_cont_err,
    m_cont_err = m_cont_err,
    b_int_err  = b_int_err,
    w_int_err  = w_int_err,
    m_int_err  = m_int_err
  )
}

#### Run Simulation ####
cat("Simulation starting...\n\n")

# AOV
aov_mc_res <- sim_optim_aov_mc(n = 75, n.cond = 500/n.jobs, tol = 0.005, dec = 2, R = 100, seed = seed)

saveRDS(aov_mc_res, file.path(save_dir, sprintf("aov_mc_res_%03d", job)))
cat("AOV module done!\n\n")


#### Results ####


if (!HPC) {
  options(scipen = 50)

  aov_mc_res  <- readRDS(file.path(save_dir, "aov_mc_res"))
  aov_results <- extract_mc_errors_aov(aov_mc_res)
  any(is.na(aov_results))

  diag_cont <- extract_diag_aov(aov_mc_res, "cont")
  diag_int  <- extract_diag_aov(aov_mc_res, "int")
  diag_all  <- rbind(diag_cont, diag_int)
  conds      <- aov_conditions(S = 300, n.cond = 100, seed = 310779)
  cond_names <- names(conds)
  des_names  <- names(aov_designs)
  n_cond     <- length(cond_names)
  n_des      <- length(des_names)

  dat  <- sim_aov_data(S = 300, conds)
  targ <- aov_extract_targets(dat, dec = 2)

  full_summary <- aov_results %>%
    group_by(condition, design) %>%
    summarise(mu  = mean(int_error),
              sd  = sd(int_error),
              min = min(int_error),
              max = max(int_error),
              conv = sum(int_error < 0.005) / length(int_error),
              uni = length(unique(int_error)))

  quantile(full_summary$conv, probs = c(.05), na.rm = TRUE)
  # 5th percentile of convergence rates: 0.79%

  min(full_summary$conv)
  # min conv rate across conditions: 0.63%

  quantile(aov_results$int_error, probs = c(.95), na.rm = TRUE)
  # 95% confidence interval for integer error: [0, 0.0049]

  any(aov_results$cont_error > .005)
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
    dplyr::select(condition, design, replication, mean_target_F,
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
    geom_hline(yintercept = .005, linetype = "dashed") +
    scale_colour_manual(values = type_pal, guide = "none") +
    facet_grid(data_type ~ design) +
    coord_cartesian(ylim = c(0, 0.02)) +
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
    geom_hline(yintercept = .005, linetype = "dashed") +
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
    plot_layout(heights = c(1, 1))

  if (FALSE) {
    ggsave("data-raw/plots/sim_aov_rel.pdf", fig_main_aov,
           width = 360, height = 220, units = "mm",
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




gen_cell_errors
aov_exp <- sim_optim_aov_mc(S = 300, n.cond = 1, tol = 0.005, dec = 2, R = 1, seed = seed, keep_full = 1)
aov_exp$conditions$cond1$cell_dists
aov_plot = plot_aov_example(aov_exp, design = "mixed", type = "int", geom = "histogram")
ggsave("data-raw/plots/aov.example.pdf", aov_plot,
       width = 260, height = 300, units = "mm",
       bg = "white", dpi = 300)



#### Testing ####
