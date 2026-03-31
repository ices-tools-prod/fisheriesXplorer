library(rsconnect)
library(desc)

account <- Sys.getenv("SHINYAPPS_ACCOUNT")
token   <- Sys.getenv("SHINYAPPS_TOKEN")
secret  <- Sys.getenv("SHINYAPPS_SECRET")

stopifnot(nzchar(account), nzchar(token), nzchar(secret))

rsconnect::setAccountInfo(
  name   = account,
  token  = token,
  secret = secret
)

app_name <- "fisheriesxplorer"

existing <- try(rsconnect::deployments("."), silent = TRUE)
app_id <- if (inherits(existing, "try-error") || nrow(existing) == 0) NULL else existing$appID[1]

rsconnect::deployApp(
  appDir    = ".",
  appName   = app_name,
  appTitle  = desc::desc_get_field("Package"),
  account   = account,
  server    = "shinyapps.io",
  appFiles  = c(
    "app.R",
    "DESCRIPTION",
    "NAMESPACE",
    ".Rbuildignore",
    ".renvignore",
    "R/",
    "inst/",
    "data/"
  ),
  appId       = app_id,
  lint        = FALSE,
  forceUpdate = TRUE,
  logLevel    = "verbose"
)