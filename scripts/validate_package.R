validate_package <- function() {
  checks <- list()
  record <- function(name,condition,detail="") {
    checks[[name]] <<- data.frame(check=name,passed=isTRUE(condition),detail=detail)
  }
  recovered_path <- file.path(ROOT,"models/draws/recovered_age39_chain_provenance.json")
  recovered <- if(file.exists(recovered_path)) jsonlite::fromJSON(recovered_path) else data.frame()
  all <- c(MODELS,SENSITIVITY)
  record("Expected primary models",identical(names(MODELS),names(MODEL_CONFIG)))
  record("Expected sensitivity models",identical(names(SENSITIVITY),names(SENSITIVITY_CONFIG)) || !length(SENSITIVITY),
    if(!length(SENSITIVITY)) "Primary-only run; full distributed package includes all sensitivities." else sprintf("All %d sensitivity fits loaded.",length(SENSITIVITY)))
  record("Original count partitions",all(DATA$long$oir_n==rowSums(DATA$long[c("oir_T1_n","oir_T2_n","oir_T3_n")])) &&
    all(DATA$long$utterance_n==DATA$long$oir_n+DATA$long$non_oir_utterance_n))
  record("Original totals",sum(DATA$long$utterance_n)==49168 && sum(DATA$long$oir_n)==1452)
  record("Age reconciliation",nrow(DATA$age_reconciliation)==1L && DATA$age_reconciliation$analysis_age_months==40)
  simulator <- read.csv(file.path(ROOT,"verification","simulator_regression_checks.csv"))
  for(i in seq_len(nrow(simulator))) record(simulator$test[i],simulator$passed[i],simulator$detail[i])
  guards <- read.csv(file.path(ROOT,"verification","cache_guard_checks.csv"))
  for(i in seq_len(nrow(guards))) record(guards$test[i],guards$passed[i],guards$detail[i])
  for(n in names(all)) {
    m <- all[[n]];a <- posterior::as_draws_array(m$draws)
    record(paste(n,"complete finite posterior draws"),all(is.finite(a)) &&
      dim(a)[1]==m$sampling$iter_sampling && dim(a)[2]==4L)
    record(paste(n,"complete finite sampler records"),nrow(m$sampler)==posterior::ndraws(a) &&
      all(is.finite(as.matrix(m$sampler))))
    record(paste(n,"portable model serialization"),identical(m$draws,
      readRDS(model_file(n,!n %in% names(MODELS)))$draws))
    record(paste(n,"full precision export"),identical(m$sampling$sig_figs,17L))
    for(chain in 1:4) {
      f <- file.path(ROOT,"models","draws",n,paste0("chain_",chain,".csv.gz"))
      if(!file.exists(f)) {record(paste(n,"chain",chain,"CSV completeness"),FALSE,"Missing sampling CSV");next}
      connection <- gzfile(f,"rt");lines <- readLines(connection,warn=FALSE);close(connection)
      rows <- lines[nzchar(lines) & !startsWith(lines,"#")]
      count <- length(rows)-1L
      # Reconstructed age-39 exports have no original CmdStan timing footer.
      # Accept them only with verified source/file hashes and complete numerical data.
      relative <- paste0("models/draws/",n,"/chain_",chain,".csv.gz")
      row <- if(nrow(recovered)) recovered[recovered$file==relative,,drop=FALSE] else data.frame()
      recovered_ok <- nrow(row)==1L &&
        identical(as.character(row$source),paste0("models/sensitivity/",n,".rds")) &&
        row$chain==chain && row$iterations==m$sampling$iter_sampling &&
        !isTRUE(row$original_cmdstan_comments_recovered) &&
        identical(as.character(row$sha256),digest::digest(file=f,algo="sha256")) &&
        identical(as.character(row$source_sha256),
          digest::digest(file=model_file(n,TRUE),algo="sha256")) &&
        any(grepl("# Reconstructed export of retained posterior draws",lines,fixed=TRUE))
      has_footer <- any(grepl("Elapsed Time:",tail(lines,10),fixed=TRUE))
      record(paste(n,"chain",chain,"CSV completeness"),count==m$sampling$iter_sampling &&
        (has_footer || recovered_ok),sprintf("%d retained iterations; %s",count,
          if(recovered_ok) "RDS-derived export hashes verified; numerical equality checked below" else
            "original CmdStan completion footer checked"))
      # Verify serialized parameter draws against the complete CSV, not just its length.
      csv <- read.csv(text=paste(rows,collapse="\n"),check.names=FALSE)
      stan_name <- function(s) {
        parts <- strsplit(s,".",fixed=TRUE)[[1]]
        if(length(parts)>1L && all(grepl("^[0-9]+$",parts[-1])))
          paste0(parts[1],"[",paste(parts[-1],collapse=","),"]") else s
      }
      names(csv) <- vapply(names(csv),stan_name,character(1))
      variables <- posterior::variables(a)
      from_rds <- matrix(as.numeric(a[,chain,]),nrow=m$sampling$iter_sampling)
      record(paste(n,"chain",chain,"all posterior CSV/RDS equality"),
        all(variables %in% names(csv)) && isTRUE(all.equal(unname(as.matrix(csv[,variables])),
          from_rds,tolerance=1e-14,check.attributes=FALSE)))
      sampler <- m$sampler[m$sampler$chain==chain,]
      sampler_variables <- setdiff(names(sampler),c("chain","iteration"))
      record(paste(n,"chain",chain,"all sampler CSV/RDS equality"),
        isTRUE(all.equal(as.matrix(csv[,sampler_variables]),as.matrix(sampler[,sampler_variables]),
          tolerance=1e-14,check.attributes=FALSE)))
    }
  }
  for(i in seq_len(nrow(SAMPLER_SUMMARY))) {
    v <- SAMPLER_SUMMARY[i,];prefix <- paste(v$model,"sampler")
    record(paste(prefix,"Rhat"),v$max_Rhat<1.01,sprintf("Maximum Rhat %.5f",v$max_Rhat))
    record(paste(prefix,"ESS"),v$min_bulk_ESS>=400 && v$min_tail_ESS>=400,
      sprintf("Minimum bulk ESS %.1f; tail ESS %.1f",v$min_bulk_ESS,v$min_tail_ESS))
    record(paste(prefix,"divergences"),v$divergences==0)
    record(paste(prefix,"treedepth"),v$maximum_treedepth_hits==0)
    record(paste(prefix,"E-BFMI"),v$min_E_BFMI>.3,sprintf("Minimum E-BFMI %.4f",v$min_E_BFMI))
  }
  for(i in seq_len(nrow(LOG_LIK_CHECK)))
    record(paste(LOG_LIK_CHECK$model[i],"independent likelihood identity"),LOG_LIK_CHECK$passed[i],
      sprintf("Maximum absolute error %.3g across %d evaluations",LOG_LIK_CHECK$maximum_absolute_difference[i],LOG_LIK_CHECK$evaluations[i]))
  for(n in names(MODELS)) {
    m <- MODELS[[n]]
    if(m$config$kind=="format") {
      p <- format_probabilities(m)
      record("Four format probabilities form a partition",max(abs(p[,,1]+p[,,2]+p[,,3]+p[,,4]-1))<1e-12)
    }
    v <- PPC_SUMMARY[PPC_SUMMARY$model==n & PPC_SUMMARY$metric=="Pearson_marginal",]
    record(paste(n,"marginal PPC not extreme"),nrow(v)==1L &&
      v$Pr_replicated_gt_observed>.01 && v$Pr_replicated_gt_observed<.99,
      sprintf("Pr(Trep > Tobs) %.4f; selected diagnostic only",v$Pr_replicated_gt_observed))
    for(metric in c("age_residual_correlation","squared_age_residual_correlation",
      "adjacent_session_residual_correlation",if(m$config$kind=="joint") "paired_source_residual_correlation")) {
      v <- PPC_SUMMARY[PPC_SUMMARY$model==n & PPC_SUMMARY$metric==metric,]
      record(paste(n,metric,"PPC not extreme"),nrow(v)==1L &&
        v$Pr_replicated_gt_observed>.01 && v$Pr_replicated_gt_observed<.99,
        sprintf("Pr(Trep > Tobs) %.4f; selected diagnostic only",v$Pr_replicated_gt_observed))
    }
  }
  o <- read.csv(file.path(ROOT,"tables","observed_counts_and_rates.csv"))
  record("Observed plotted composition totals",max(abs(rowSums(o[c("T1_share_percent","T2_share_percent","T3_share_percent")])-100))<1e-8)
  for(n in FIGURE_NAMES) for(extension in c("pdf","png")) {
    f <- file.path(ROOT,"figures",paste0(n,".",extension))
    signature <- if(file.exists(f)) readBin(f,"raw",n=8) else raw()
    expected <- if(extension=="pdf") charToRaw("%PDF") else as.raw(c(137,80,78,71,13,10,26,10))
    record(paste(n,extension,"file signature"),length(signature)>=length(expected) &&
      identical(signature[seq_along(expected)],expected) && file.info(f)$size>5000)
  }
  out <- do.call(rbind,checks)
  write_table(out,"automated_checks.csv","verification")
  report <- c("# Automated verification report","",
    sprintf("%d of %d checks passed.",sum(out$passed),nrow(out)),"",
    "Checks cover data reconciliation, count partitions, complete four-chain files, CSV/RDS agreement, cached-fit guards, sampler diagnostics, independent likelihood evaluation, selected predictive checks, and figure file signatures.",
    "",
    "Predictive adequacy and substantive interpretation still require judgment. The checks do not independently verify recordings, repair annotations, or inter-rater reliability.",
    "",
    "Cached checks verify the supplied saved fits and generated outputs; they are not a fresh stochastic refit. Archive hashes are checked separately with scripts/verify_manifest.py.")
  if(any(!out$passed)) report <- c(report,"","## Failed checks",paste0("- ",out$check[!out$passed],": ",out$detail[!out$passed]))
  writeLines(report,file.path(ROOT,"verification","AUTOMATED_VERIFICATION.md"))
  if(any(!out$passed)) stop("Verification failed; inspect verification/automated_checks.csv before release.")
  cat("All",nrow(out),"automated checks passed.\n")
  invisible(out)
}
