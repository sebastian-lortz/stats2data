# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv()
## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
golem::add_positconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----

# install own package from Git and snapshot
# FIRST find and delete all .o .so files
# find . -type f -name '*.so'
# find . -type f -name '*.o'
# ./src/stats2data.so
# rm ./src/stats2data.so

# THEN commit and push!

renv::install("rstudio/rsconnect")
renv::install("github::sebastian-lortz/stats2data")
renv::snapshot(prompt = FALSE)

## Add/update manifest file (optional; for Git backed deployment on Posit )
rsconnect::writeManifest()


## In command line.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)


