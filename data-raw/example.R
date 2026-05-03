# Empericial example

##### Descriptive & LM Module ######

#### Empirical Example: Mpox Data ####
# Le Forestier, Page-Gould, & Chasteen (2022), https://osf.io/t6d5y (retrieved: 24.04.2026)

library(stats2data)
library(ggplot2)
library(patchwork)
library(dplyr)
library(jtools)

save_dir <- "data-raw/plots"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# extract descriptives: mean, sd, range

# gender and ethnicity reported as counts, we compute the mean
gender_mean = 103/388
eth_mean = 265/388

# set the means (from table) and name it so the optimizer later knows what variable is what
t_means = c(.35, eth_mean, 4.35, 72.65, gender_mean, 55.92, 1.14,32.35, 14.14, 43.71, 102.35)
names(t_means) = c("marital", "eth",  "health", "age",  "gender", "external", "support", "self_esteem", "religion", "ses", "fear_death")

# set the SDs (from table), note that for binary variables we put the SD to 0, the module generates the distribution based only on the mean
t_sd = c(NA, NA, 0.90, 7.73 , NA, 17.54, 0.36, 4.66, 2.33, 15.55, 23.33)

# set the range as a matrix: first row lower bound, second row upper bounds.
# support was log transformed, thus, i look at the mean and sd and choose sensible ranges and treat the variable as conitnous.
# some effort to check scale ranges if not reported
range_m = matrix(c(0, 0, 1,  60, 0,  16, -3,  1,  3, 11,  37,
                   1, 1, 6, 100, 1, 112,  3, 40, 16, 77, 185),
                 byrow = TRUE, ncol = 11)
int = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)

out = optim_vec(
  N = 1000,
  target_mean = t_means,
  target_sd = t_sd,
  range = range_m,
  integer = int,
  thresh = .005,
  sprite_prec = c(2,2)
)
summary(out)

# inspect e.g., fear of death
hist(out$data$support)



t_reg = c(NA, -0.7, -2.12, -2.91, -0.24, 10.08, 0.38, 5.60, -0.17, -1.22, -0.06)
t_se = c(NA, 2.61, 2.66, 1.31, 0.16, 2.88, 0.07, 3.59, 0.26, 0.51, 0.08)
t_cor = c(-.01, -.03, .17, .34, .08, -.10, .03, .06, -.11, .05,
                -.13, -.25, -.09, -.02, -.01, .11, .19, -.26, -.05,
                      .06, .04, -.06, .03, .21, -.02, .24,  -.12,
                           .11, .12, -.07, -.10, .03, -.02, -.03,
                                -.01, .14, .04, .20, -.08, .17,
                                      -.10, -.21, -.07, -.29, .30,
                                              .06, .23, .11, .07,
                                                    .03, .16, -.11,
                                                          -.06, -.10,
                                                                -.13)



mlr_out = optim_mlr(
  N = 388,
  target_mean = t_means,
  target_sd = t_sd,
  range = range_m,
  integer = int,
  target_reg = t_reg,
  target_cor = t_cor,
  reg_equation = ("fear_death ~ marital+eth+health+age+gender+external+support+self_esteem+religion+ses"),
  max_starts = 50
)
summary(mlr_out)
plot_error(mlr_out, first_iter = 1000)
plot_error(mlr_out, ratio = TRUE)
plot_summary(mlr_out, standardised = FALSE)
# include error plots next to each other, underneath the plot_summary plot

# we observed that regression SE dominates the objective, thus we increase the mixing weight
# to account for that. also, we run optimizations in parallel now,
# and we start from higher temperature (more exploration).

parallel_out = parallel_optim(
  FUN = optim_mlr,
  args = list(N = 388,
              target_mean = t_means,
              target_sd = t_sd,
              range = range_m,
              integer = int,
              target_reg = t_reg,
              target_cor = t_cor,
              reg_equation = ("fear_death ~ marital+eth+health+age+gender+external+support+self_esteem+religion+ses"),
              weight = .9,
              init_temp = 1,
              max_starts = 20),
  seed = 123,
  runs = 12,
  cores = 6
)
summary(parallel_out)
plot_error(parallel_out$best)
plot(parallel_out)
plot_error(parallel_out$best, ratio = TRUE)
plot_summary(parallel_out$best, standardised = FALSE)

