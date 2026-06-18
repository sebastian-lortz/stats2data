# script to generate job scripts for the Habrok HPC
n.jobs <- 100

# set root path
template <- readLines("/Users/lortz/Desktop/PhD/Research/simdata/stats2data/data-raw/aov_simulation_study.R")

# set save path
save.dir <- "/Users/lortz/Desktop/PhD/Research/simdata/stats2data/data-raw/hpc/"
dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)

for (i in 1:n.jobs) {
  script <- gsub("^job = .+$", paste("job =", i), template)
  script <- gsub("^n.jobs = .+$", paste("n.jobs =", n.jobs), script)
  writeLines(script, file.path(save.dir, paste0("batch", i, ".R")))
}

