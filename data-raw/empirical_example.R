### Empirical Example: ANOVA module

# Setup
library(stats2data)
library(ggplot2)
library(jtools)
library(patchwork)
options(scipen = 50)

# Output directory for manuscript figures
fig_dir <- file.path("data-raw", "plots")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(2026)

# Targets (Reynolds & Besner, 2008)
sumstats = list(
S                  = 16,
levels             = c(2, 2),
factor_type        = c("within", "within"),
formula            = "outcome ~ Factor1 * Factor2 + Error(ID / (Factor1 * Factor2))",
integer            = FALSE,
target_f_list = list(
  effect = c("Factor1", "Factor2", "Factor1:Factor2"),
  F_value      = c(30.5, 0.4, 0.5)
),
target_group_means = c(543, 536, 614, 618),
# plausible range based on reporte MSE = 3070
range  = c(min(target_group_means) - floor(4 * sqrt(3070)),
            max(target_group_means) + ceiling(4 * sqrt(3070)))
)


# ---- Single ANOVA optimisation ------------------------------------------
result_aov <- do.call(optim_aov, sumstats)

# Inspect
summary(result_aov)


# Figure: error trajectory + cooling schedule
fig_diag <- (plot_error(result_aov, first_iter = 500)) | (plot_summary(result_aov, standardised = FALSE))

ggsave(file.path(fig_dir, "aov.empirical.diagnostics.pdf"),
       fig_diag,
       width = 260, height = 90, units = "mm",
       bg = "white", dpi = 300)


# ---- Repeated runs via parallel_optim() ---------------------------------
out_parallel_aov <- parallel_optim(
  FUN  = optim_aov,
  args = sumstats,
  runs  = 100,
  cores = 6,
  seed  = 2026
)
saveRDS(out_parallel_aov, file.path("data-raw", "results", "out_parallel_aov") )
# Inspect: across-run aggregation + RMSEs
summary(result.parallel_aov)


# ---- Figure: parallel-run RMSE distribution + best-run lollipop ---------
fig_parallel <- (plot(result.parallel_aov)) | (plot_summary(result.parallel_aov, standardised = FALSE))

ggsave(file.path(fig_dir, "aov.empirical.parallel.pdf"),
       fig_parallel,
       width = 360, height = 150, units = "mm",
       bg = "white", dpi = 300)
