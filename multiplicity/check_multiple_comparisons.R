# Post-hoc directional-error analysis of the 75 primary zero-referenced estimands.
# Usage from the package root:
# Rscript multiplicity/check_multiple_comparisons.R --package . --out multiplicity/results
args <- commandArgs(trailingOnly=TRUE)
argument <- function(flag) {
  i <- match(flag,args)
  if(is.na(i) || i==length(args)) stop("Supply ",flag," and its directory.")
  args[i+1L]
}
ROOT <- normalizePath(argument("--package"),mustWork=TRUE)
OUT <- normalizePath(argument("--out"),mustWork=FALSE)
dir.create(OUT,recursive=TRUE,showWarnings=FALSE)
source(file.path(ROOT,"scripts/config.R"))
source(file.path(ROOT,"scripts/helpers.R"))
required_packages(FALSE)
source(file.path(ROOT,"scripts/prepare_data.R"))
DATA <- prepare_data()
MODELS <- setNames(lapply(names(MODEL_CONFIG),function(n)
  load_model(n,MODEL_CONFIG[[n]])),names(MODEL_CONFIG))
source(file.path(ROOT,"scripts/posterior_summaries.R"))

family <- list(); metadata <- list()
add <- function(id,values,model,group,label,units) {
  stopifnot(!id %in% names(family),length(values)==8000L,all(is.finite(values)))
  family[[id]] <<- unname(values)
  metadata[[id]] <<- data.frame(id=id,model=model,group=group,label=label,units=units)
}
pretty <- function(s) {
  for(pair in list(c("equal_language_average","Equal-language average"),
    c("chintang","Chintang"),c("indonesian","Indonesian"),c("russian","Russian"),
    c("_minus_"," minus "),c("_"," ")))
    s <- gsub(pair[1],pair[2],s,fixed=TRUE)
  s
}
c <- child_contrasts(MODELS$child_rate)
for(n in names(c$input)) add(paste0("input_",n),c$input[[n]],"child_rate",
  "Input associations",pretty(n),"OIR per 1000 utterances")
for(n in names(c$age)) add(paste0("child_age_",n),c$age[[n]],"child_rate",
  "Overall child age changes",pretty(n),"OIR per 1000 utterances")
for(n in names(c$pairwise)) add(paste0("pairwise_",n),c$pairwise[[n]],"child_rate",
  "Pairwise age/input differences",pretty(n),"OIR per 1000 utterances")
j <- joint_contrasts(MODELS$joint_rate)
for(n in names(j$source)) add(paste0("source_",n),j$source[[n]],"joint_rate",
  "Speech-source differences",paste(pretty(n),"child minus CDS"),"OIR per 1000 utterances")
for(n in names(j$h3)) add(paste0("dispersion_",n),j$h3[[n]]$delta_SD,"joint_rate",
  "Dispersion contrasts and changes",paste(n,"child minus CDS SD"),"OIR per 1000 utterances")
for(n in names(j$changes)) add(paste0("dispersion_change_",n),j$changes[[n]],"joint_rate",
  "Dispersion contrasts and changes",pretty(n),"OIR per 1000 utterances")
f <- format_contrasts(MODELS$repair_formats)
for(n in names(f$incidence)) add(paste0("format_incidence_",n),f$incidence[[n]],"repair_formats",
  "Format incidence contrasts",pretty(n),"Occurrences per 1000 utterances")
for(n in names(f$composition)) add(paste0("format_composition_",n),f$composition[[n]],"repair_formats",
  "Format composition contrasts",pretty(n),"Percentage points among OIR")
for(n in names(f$age)) add(paste0("format_age_",n),f$age[[n]],"repair_formats",
  "Format age incidence changes",pretty(n),"Occurrences per 1000 utterances")
d <- draw_matrix(MODELS$repair_formats)
for(g in seq_along(MODELS$repair_formats$condition_levels)) for(k in 1:3) {
  n <- paste0("format_linear_",MODELS$repair_formats$condition_levels[g],"_T",k)
  add(n,d[,paste0("age_slope[",g,",",k,"]")],"repair_formats",
    "Local format age coefficients",paste(pretty(MODELS$repair_formats$condition_levels[g]),paste0("T",k)),
    "Log relative odds per age SD")
}
for(n in c("child_rate","joint_rate")) {
  b <- coefficient_draws(MODELS[[n]])
  for(k in which(grepl("I(age_z^2)",colnames(b),fixed=TRUE)))
    add(paste0("quadratic_",n,"_",k),b[,k],n,"Quadratic age coefficients",
      paste(n,colnames(b)[k]),"Log odds per squared age SD")
}
for(k in 1:2) add(paste0("quadratic_repair_formats_",SOURCES[k]),d[,paste0("quadratic[",k,"]")],
  "repair_formats","Quadratic age coefficients",paste("Format model",SOURCES[k]),
  "Log relative odds per squared age SD")
stopifnot(length(family)==75L,!anyDuplicated(names(family)))

out <- do.call(rbind,lapply(names(family),function(n) {
  x <- family[[n]];s <- summarize_vector(x)
  ppos <- mean(x>0);pneg <- mean(x<0)
  stopifnot(ppos+pneg==1)
  direction <- if(ppos>=pneg) "positive" else "negative"
  error <- if(direction=="positive") as.integer(x<=0) else as.integer(x>=0)
  a <- posterior::as_draws_array(array(error,dim=c(2000L,4L,1L),
    dimnames=list(NULL,NULL,"incorrect_sign")))
  mcse <- if(length(unique(error))==1L) 0 else as.numeric(posterior::mcse_mean(a))[1]
  lfsr <- mean(error)
  cbind(metadata[[n]],s,selected_direction=direction,local_false_sign_rate=lfsr,
    sign_probability_mcse=mcse,selected_at_5pct=lfsr<=.05)
}))
rownames(out) <- NULL
out <- out[order(out$group,out$id),]
write.csv(out,file.path(OUT,"multiple_comparisons_all_75.csv"),row.names=FALSE)
selected <- out[out$selected_at_5pct,]
write.csv(selected,file.path(OUT,"multiple_comparisons_selected.csv"),row.names=FALSE)
saveRDS(family,file.path(OUT,"multiple_comparisons_contrast_draws.rds"),compress="xz")
counts <- as.data.frame(table(out$group),stringsAsFactors=FALSE)
names(counts) <- c("group","family_members")
write.csv(counts,file.path(OUT,"multiple_comparisons_family_counts.csv"),row.names=FALSE)
mean_error <- if(nrow(selected)) mean(selected$local_false_sign_rate) else 0
stopifnot(mean_error<=.05,all(out$local_false_sign_rate<=.5))
sha <- vapply(names(MODELS),function(n) digest::digest(file=model_file(n),algo="sha256"),character(1))
summary <- list(post_hoc=TRUE,threshold=.05,family_size=nrow(out),selected=nrow(selected),
  posterior_expected_incorrect_sign_proportion=mean_error,
  posterior_expected_number_incorrect_signs=sum(selected$local_false_sign_rate),
  maximum_selected_local_false_sign_rate=if(nrow(selected)) max(selected$local_false_sign_rate) else 0,
  minimum_unselected_local_false_sign_rate=min(out$local_false_sign_rate[!out$selected_at_5pct]),
  minimum_selected_threshold_gap_in_mcse=if(any(selected$sign_probability_mcse>0))
    min((.05-selected$local_false_sign_rate[selected$sign_probability_mcse>0])/
      selected$sign_probability_mcse[selected$sign_probability_mcse>0]) else NULL,
  rule="Retain a directional claim only if its individual posterior false-sign probability is at most 0.05. The mean error among any such set is therefore also at most 0.05; no independence assumption is required for this expectation identity.",
  scope="Model-dependent posterior directional error; not frequentist FDR/FWER, simultaneous 95% intervals, effect-size protection or preregistered confirmation.",
  original_data_sha256=DATA$original_data_sha256,model_sha256=as.list(sha),
  software=list(R=as.character(getRversion()),posterior=as.character(packageVersion("posterior"))))
jsonlite::write_json(summary,file.path(OUT,"multiple_comparisons_summary.json"),pretty=TRUE,auto_unbox=TRUE,digits=16)
writeLines(capture.output(sessionInfo()),file.path(OUT,"multiple_comparisons_sessionInfo.txt"))
print(summary[c("family_size","selected","posterior_expected_incorrect_sign_proportion")])
