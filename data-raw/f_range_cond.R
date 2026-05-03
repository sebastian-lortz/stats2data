## ============================================================================
## Verification: empirical F-value range from streamlined sim_aov_data
##
## Source aov_simulation_study.R first (so aov_conditions, sim_aov_data,
## aov_extract_targets, data_gen, pars_conditions, aov_designs are in scope).
##
## What this checks:
##   (1) Empirical F distribution per design (between / within / mixed),
##       both for continuous and integer-rounded data
##   (2) Cohen's f range -> empirical F range mapping
##   (3) Whether the simulated F's actually cover the difficulty regimes
##       the optimizer has to handle
##   (4) Comparison to the original [0.5, 10] target_F range
## ============================================================================

suppressPackageStartupMessages({
  library(stats2data)
  library(afex)
  library(dplyr)
  library(ggplot2)
})

# --- knobs (smaller than the manuscript run) --------------------------------
S        <- 300
n_cond   <- 100
seed_val <- 310779

# --- step 1: generate conditions and data ----------------------------------
conds <- aov_conditions(S, n_cond, seed = seed_val)

stopifnot(
  "target_f missing — re-source aov_simulation_study.R, then rerun this script from the top" =
    !is.null(conds[[1]]$target_F) && length(conds[[1]]$target_F) == 3L
)

dat  <- sim_aov_data(S, conds)
targ <- aov_extract_targets(dat, dec = 2)

# sanity-check Cohen's f range as drawn
target_f_drawn <- do.call(rbind, lapply(conds, function(s) s$target_F))
cat("Cohen's f range (across conditions x effects):\n")
print(round(c(min = min(target_f_drawn),
              q25 = quantile(target_f_drawn, .25),
              med = median(target_f_drawn),
              q75 = quantile(target_f_drawn, .75),
              max = max(target_f_drawn)), 4))

# --- step 2: collect empirical F's per (cond, design, type, effect) --------
f_long <- do.call(rbind, lapply(names(targ), function(cn) {
  do.call(rbind, lapply(names(targ[[cn]]), function(des) {
    do.call(rbind, lapply(c("cont", "int"), function(type) {
      t <- targ[[cn]][[des]][[type]]
      if (is.null(t)) return(NULL)
      data.frame(
        condition = cn,
        design    = des,
        type      = type,
        effect    = t$effect_names,
        F_emp     = t$f_vals,
        f_seed    = conds[[cn]]$target_F,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

# --- step 3: per-design summary --------------------------------------------
cat("\n========== Empirical F by (design x type) ==========\n")
print(
  f_long %>%
    group_by(design, type) %>%
    summarise(
      n      = n(),
      min    = round(min(F_emp), 3),
      q05    = round(quantile(F_emp, .05), 3),
      q25    = round(quantile(F_emp, .25), 3),
      median = round(median(F_emp), 3),
      q75    = round(quantile(F_emp, .75), 3),
      q95    = round(quantile(F_emp, .95), 3),
      max    = round(max(F_emp), 3),
      .groups = "drop"
    ),
  n = Inf
)

# --- step 4: per-design x effect (does it differ across the 3 effects?) ----
cat("\n========== Empirical F by (design x effect, continuous) ==========\n")
print(
  f_long %>% filter(type == "cont") %>%
    group_by(design, effect) %>%
    summarise(
      median = round(median(F_emp), 3),
      q05    = round(quantile(F_emp, .05), 3),
      q95    = round(quantile(F_emp, .95), 3),
      .groups = "drop"
    ),
  n = Inf
)

# --- step 5: coverage of original [0.5, 10] target range -------------------
cat("\n========== Coverage of original [0.5, 10] target_F range ==========\n")
print(
  f_long %>% filter(type == "cont") %>%
    group_by(design) %>%
    summarise(
      pct_below_0.5 = round(mean(F_emp < 0.5)  * 100, 1),
      pct_in_range  = round(mean(F_emp >= 0.5 & F_emp <= 10) * 100, 1),
      pct_above_10  = round(mean(F_emp > 10)   * 100, 1),
      .groups = "drop"
    )
)

# --- step 6: f-seed -> F-empirical relationship ---------------------------
cat("\n========== Pearson cor(f_seed, F_empirical) by design ==========\n")
print(
  f_long %>% filter(type == "cont") %>%
    group_by(design) %>%
    summarise(
      cor_f_to_F = round(cor(f_seed, F_emp), 3),
      .groups    = "drop"
    )
)

# --- step 7: optional plot --------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot(f_long %>% filter(type == "cont"),
              aes(x = f_seed, y = F_emp, colour = design)) +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.6) +
    geom_hline(yintercept = c(0.5, 10), linetype = "dashed",
               colour = "grey40", linewidth = 0.3) +
    facet_wrap(~ design, scales = "free_y") +
    labs(x = "Cohen's f (seed)", y = "Empirical F (continuous data)",
         title = "Seed-to-empirical F mapping by design",
         subtitle = "Dashed lines = original [0.5, 10] target range") +
    theme_minimal()
  print(p)
  ggsave("f_range_check.pdf", p, width = 240, height = 100, units = "mm")
  cat("\nPlot -> f_range_check.pdf\n")
}

invisible(f_long)
