# Fit the coherent four-category format model; requires CmdStan 2.40 or newer.
# Set the working directory to the extracted project root.
ROOT <- normalizePath(getwd(),mustWork=TRUE)
for(script in c("config.R","helpers.R","prepare_data.R","fit_models.R"))
  source(file.path(ROOT,"scripts",script))
required_packages(TRUE)
DATA <- prepare_data()
fit_one_model("repair_formats",MODEL_CONFIG$repair_formats,FALSE,2026100906L)


