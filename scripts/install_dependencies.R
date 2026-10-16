# Optional setup. Review before running; installs packages, not a CmdStan toolchain.
packages <- c("posterior","dplyr","tidyr","ggplot2","gridExtra","jsonlite","digest")
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos="https://cloud.r-project.org")
if("--refit" %in% commandArgs(trailingOnly=TRUE) && !requireNamespace("cmdstanr",quietly=TRUE))
  install.packages("cmdstanr",repos=c("https://stan-dev.r-universe.dev","https://cloud.r-project.org"))
message("Cached reproduction is ready. For refitting, rerun this script with --refit, then install CmdStan 2.40 or newer with cmdstanr::install_cmdstan().")
