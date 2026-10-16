# Negative tests: modified inputs/metadata must never reuse a stale fit.
check_cache_guards <- function() {
  original_root <- ROOT;original_data <- DATA
  fixture <- tempfile("oir_cache_guard_");dir.create(fixture)
  on.exit({ROOT <<- original_root;DATA <<- original_data;unlink(fixture,recursive=TRUE)},add=TRUE)
  for(d in c("data/original","data/analysis","stan","models/primary"))
    dir.create(file.path(fixture,d),recursive=TRUE,showWarnings=FALSE)
  originals <- list.files(file.path(original_root,"data/original"),full.names=TRUE)
  restore_data <- function() file.copy(originals,file.path(fixture,"data/original"),overwrite=TRUE)
  restore_data()
  file.copy(list.files(file.path(original_root,"stan"),pattern="[.]stan$",full.names=TRUE),file.path(fixture,"stan"))
  fit_file <- file.path(fixture,"models/primary/child_rate.rds")
  file.copy(file.path(original_root,"models/primary/child_rate.rds"),fit_file)
  ROOT <<- fixture;DATA <<- prepare_data()
  checks <- list()
  reject <- function(name,expression,pattern="differs") {
    result <- tryCatch(force(expression),error=function(e)e)
    passed <- inherits(result,"error") && grepl(pattern,conditionMessage(result))
    checks[[name]] <<- data.frame(test=name,passed=passed,
      detail=if(inherits(result,"error")) conditionMessage(result) else "Modified fit was incorrectly accepted.")
  }
  invisible(load_model("child_rate",MODEL_CONFIG$child_rate))
  cfg <- MODEL_CONFIG$child_rate;cfg$slope_sd <- cfg$slope_sd+.01
  reject("Changed model-prior configuration",load_model("child_rate",cfg))
  # Preserve totals and count partitions while changing two child response rows.
  path <- file.path(fixture,"data/original")
  long <- read.csv(file.path(path,"repairs_primary_cs_cds_long.csv"),stringsAsFactors=FALSE)
  proto <- read.csv(file.path(path,"prototypes_new.csv"),stringsAsFactors=FALSE,check.names=FALSE)
  eligible <- which(long$speech_source=="child" & long$oir_T1_n>=1 & long$non_oir_utterance_n>=1)
  stopifnot(length(eligible)>=2L)
  for(j in 1:2) {
    i <- eligible[j];delta <- c(1,-1)[j]
    long$oir_n[i] <- long$oir_n[i]+delta;long$oir_T1_n[i] <- long$oir_T1_n[i]+delta
    long$non_oir_utterance_n[i] <- long$non_oir_utterance_n[i]-delta
    k <- which(proto$language==long$language[i] & proto$target_child==long$target_child[i] & proto$target_age==long$target_age[i])
    stopifnot(length(k)==1L)
    for(field in c("OIR_cs_all","OIR_cs_T1","OIR_total","OIR_total_T1")) proto[[field]][k] <- proto[[field]][k]+delta
  }
  write.csv(long,file.path(path,"repairs_primary_cs_cds_long.csv"),row.names=FALSE)
  write.csv(proto,file.path(path,"prototypes_new.csv"),row.names=FALSE)
  DATA <<- prepare_data()
  reject("Changed balanced response counts",load_model("child_rate",MODEL_CONFIG$child_rate))
  restore_data();DATA <<- prepare_data()
  code <- file.path(fixture,"stan/rates_beta_binomial.stan")
  cat("\n// Intentional change for a cache guard test.\n",file=code,append=TRUE)
  reject("Changed Stan source",load_model("child_rate",MODEL_CONFIG$child_rate),"Stan code differs")
  file.copy(file.path(original_root,"stan/rates_beta_binomial.stan"),code,overwrite=TRUE)
  m <- readRDS(fit_file);m$children <- rev(m$children);saveRDS(m,fit_file,compress="xz")
  reject("Changed child-index metadata",load_model("child_rate",MODEL_CONFIG$child_rate))
  ROOT <<- original_root;DATA <<- original_data
  out <- do.call(rbind,checks)
  write_table(out,"cache_guard_checks.csv","verification")
  if(!all(out$passed)) stop("A stale-fit cache guard test failed.")
  invisible(out)
}
