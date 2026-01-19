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
golem::run_dev()

## Run checks ----
## Check the package before sending to prod
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

## Add/update manifest file (optional; for Git backed deployment on Posit )
rsconnect::writeManifest()

## In command line.
# rsconnect::deployApp(
#     appName = desc::desc_get_field("Package"),
#     appTitle = desc::desc_get_field("Title"),
#     appFiles = c(
#         # Add any additional files unique to your app here.
#         "R/",
#         "inst/",
#         "data/",
#         "NAMESPACE",
#         "DESCRIPTION",
#         "app.R"
#     ),
#     appId = rsconnect::deployments(".")$appID,
#     lint = FALSE,
#     forceUpdate = TRUE
# )



## Local Testing ----
## Teste die App lokal
golem::run_dev()

## RStudio ----
## Nutze RStudio "Run App" Button

## RStudio ----
## Rstudio Publish Button (Shinyapps.io)
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----

## Preparation for Docker ----
## Wenn du Docker verwenden willst:
# golem::add_dockerfile()

## Deployment mit Docker Build
# docker build -t RSPrismBB .
# docker run -p 3838:3838 RSPrismBB

# Oder mit Docker Compose:
# docker-compose up

## ShinyProxy, Kubernetes, etc. ----

## Deployment über andere Plattformen ----
# Verweise auf die golem Dokumentation für weitere Deployment-Optionen

## Heroku ----
# golem::add_dockerfile_heroku()
