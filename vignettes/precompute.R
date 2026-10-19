# Precompute the vignette
old <- setwd("vignettes")
knitr::knit("stats2data_application.Rmd.orig",
            output = "stats2data_application.Rmd")
setwd(old)
