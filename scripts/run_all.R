# Run with Rscript scripts/run_all.R --mode cached (default) or --mode refit.
# Cached reproduction requires no compiler or CmdStan installation.
run_package <- function(mode="cached",sensitivity=TRUE,root=NULL) {
  if(is.null(root)) {
    args <- commandArgs(FALSE)
    script <- args[grepl("^--file=",args)]
    script_path <- if(length(script)) gsub("~+~"," ",sub("^--file=","",script[1]),fixed=TRUE) else character()
    candidates <- c(if(length(script_path)) dirname(dirname(script_path)) else character(),
      getwd(),dirname(getwd()))
    candidates <- unique(normalizePath(candidates,mustWork=FALSE))
    valid <- candidates[file.exists(file.path(candidates,"data","original","repairs_primary_cs_cds_long.csv")) &
                          file.exists(file.path(candidates,"scripts","config.R"))]
    if(!length(valid)) stop("Run from the extracted project root or provide root= explicitly.")
    root <- valid[1]
  }
  ROOT <<- normalizePath(root,mustWork=TRUE)
  for(d in c("tables","figures","diagnostics","documentation","verification","data/analysis"))
    dir.create(file.path(ROOT,d),recursive=TRUE,showWarnings=FALSE)
  source(file.path(ROOT,"scripts","config.R"),local=.GlobalEnv)
  source(file.path(ROOT,"scripts","helpers.R"),local=.GlobalEnv)
  required_packages(mode=="refit")
  source(file.path(ROOT,"scripts","prepare_data.R"),local=.GlobalEnv)
  DATA <<- prepare_data()
  source(file.path(ROOT,"scripts","fit_models.R"),local=.GlobalEnv)
  if(mode=="refit") fit_requested_models(sensitivity)
  else if(!mode %in% c("cached","verify")) stop("Mode must be cached, verify or refit.")
  MODELS <<- setNames(lapply(names(MODEL_CONFIG),function(n) load_model(n,MODEL_CONFIG[[n]])),
                     names(MODEL_CONFIG))
  SENSITIVITY <<- if(sensitivity) setNames(lapply(names(SENSITIVITY_CONFIG),
    function(n) load_model(n,SENSITIVITY_CONFIG[[n]],TRUE)),names(SENSITIVITY_CONFIG)) else list()
  for(script in c("posterior_summaries.R","predictive_checks.R","check_simulators.R","check_cache_guards.R","make_figures.R","validate_package.R"))
    source(file.path(ROOT,"scripts",script),local=.GlobalEnv)
  summarize_models()
  predictive_checks()
  check_simulators()
  check_cache_guards()
  make_figures()
  validate_package()
  writeLines(capture.output(sessionInfo()),file.path(ROOT,"verification","sessionInfo.txt"))
  cat("Completed",mode,"pipeline. Tables, figures and diagnostics generated from verified models.\n")
  invisible(TRUE)
}
if(sys.nframe()==0L) {
  args <- commandArgs(trailingOnly=TRUE)
  mode <- if("--mode" %in% args) args[match("--mode",args)+1L] else "cached"
  run_package(mode=mode,sensitivity=!"--primary-only" %in% args)
}
