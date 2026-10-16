# Compatibility entry point replacing the original reporting-only master script.
# Set the working directory to the extracted project root.
source("scripts/run_all.R")
run_package(mode="cached",root=getwd())
