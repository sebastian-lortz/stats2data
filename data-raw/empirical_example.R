### Empirical Example: ANOVA module

library(stats2data)
library(ggplot2)
library(jtools)
library(patchwork)
options(scipen = 50)

# out dir
fig_dir <- file.path("data-raw", "plots")
res_dir <- file.path("data-raw", "results")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)

# Targets (Reynolds & Besner, 2008, Experiment 5)
S   = 16
mse = 3070
target_group_means = c(543, 536, 614, 618)
eta2 <- c(0.002, 0.015)
df_err <- S - 1

# implied numerator (balanced 2 x 2 within design)
m_cell  <- matrix(target_group_means, nrow = 2, byrow = TRUE)  # rows: Factor1, cols: Factor2
m_grand <- mean(m_cell)
ms_f1   <- S * 2 * sum((rowMeans(m_cell) - m_grand)^2)                        # 93636
ms_f2   <- S * 2 * sum((colMeans(m_cell) - m_grand)^2)                        # 36
ms_int  <- S * sum((m_cell - outer(rowMeans(m_cell), colMeans(m_cell), "+") + m_grand)^2)  # 484

# Checks consitency
stopifnot(round(ms_f1 / mse, 1) == 30.5)
stopifnot(round(30.5 / (30.5 + df_err), 3) == 0.670)

# F for the two effects reported as F < 1
F_value_derived = c(round(ms_f1 / mse, 1), round(eta2 * df_err / (1 - eta2), 3))  # 30.5, 0.030, 0.228

sumstats = list(
  S                  = S,
  levels             = c(2, 2),
  factor_type        = c("within", "within"),
  formula            = "outcome ~ Factor1 * Factor2 + Error(ID / (Factor1 * Factor2))",
  integer            = FALSE,
  target_f_list = list(
    effect  = c("Factor1", "Factor2", "Factor1:Factor2"),
    F_value = F_value_derived
  ),
  target_group_means = target_group_means,
  # plausible range
  range  = c(floor(min(target_group_means) - 4 * sqrt(mse)),
             ceiling(max(target_group_means) + 4 * sqrt(mse)))
)
print(sumstats$target_f_list$F_value)
print(sumstats$range)

# single optimization
set.seed(2026)
start = Sys.time()
result_aov <- do.call(optim_aov, sumstats)
end = Sys.time()
end-start

summary(result_aov)

which(result_aov$track_objective < result_aov$inputs$thresh)[1]

range(result_aov$data$outcome)
any(result_aov$data$outcome %in% sumstats$range)

# Figure: diagnostics and summary
fig_diag <- (plot_convergence(result_aov, log_y = TRUE)) | (plot_fit(result_aov, standardised = FALSE))

ggsave(file.path(fig_dir, "aov.empirical.diagnostics.pdf"),
       fig_diag,
       width = 180, height = 80, units = "mm",
       bg = "white", dpi = 300)


# parallel_optim()
out_parallel_aov <- parallel_optim(
  FUN  = optim_aov,
  args = sumstats,
  runs  = 100,
  cores = 6,
  seed  = 2026,
  stop_conv = FALSE
)
saveRDS(out_parallel_aov, file.path(res_dir, "out_parallel_aov.rds"))

summary(out_parallel_aov)
max(sapply(out_parallel_aov$results, function(x) {
  x$best_objective
})) < .005

sum(vapply(out_parallel_aov$results,
           function(r) r$best_objective < r$inputs$thresh, logical(1)))

range(unlist(lapply(out_parallel_aov$results, function(r) r$data$outcome)))


# Figure: parallel run RMSE distribution
fig_parallel <- (plot(out_parallel_aov)) | (plot_fit(out_parallel_aov, standardised = FALSE))

ggsave(file.path(fig_dir, "aov.empirical.parallel.pdf"),
       fig_parallel,
       width = 180, height = 90, units = "mm",
       bg = "white", dpi = 300)
