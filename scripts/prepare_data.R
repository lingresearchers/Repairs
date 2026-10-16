prepare_data <- function() {
  original <- file.path(ROOT,"data","original")
  long <- read.csv(file.path(original,"repairs_primary_cs_cds_long.csv"),stringsAsFactors=FALSE)
  proto <- read.csv(file.path(original,"prototypes_new.csv"),stringsAsFactors=FALSE,check.names=FALSE)
  sha <- vapply(c("prototypes_new.csv","repairs_primary_cs_cds_long.csv"),
    function(f) digest::digest(file=file.path(original,f),algo="sha256"),character(1))
  age_from_label <- function(x) vapply(strsplit(x,";",fixed=TRUE),
    function(a) 12*as.numeric(a[1])+as.numeric(a[2]),numeric(1))
  target_age <- age_from_label(long$target_age)
  if (any(long$age_months!=target_age))
    stop("Long-data age disagrees with structured target_age; review source reconciliation.")
  discrepancies <- merge(proto[c("language","target_child","target_age","age_months")],
    unique(long[c("language","target_child","target_age","age_months")]),
    by=c("language","target_child","target_age"),suffixes=c("_session_summary","_long"))
  discrepancies <- discrepancies[discrepancies$age_months_session_summary!=discrepancies$age_months_long,]
  discrepancies$analysis_age_months <- age_from_label(discrepancies$target_age)
  discrepancies$resolution <- "Use structured target_age and matching long-data age; assess age 39 in sensitivity fits."
  write_table(discrepancies,"age_reconciliation.csv","data/analysis")
  counts <- c("utterance_n","oir_n","oir_T1_n","oir_T2_n","oir_T3_n","non_oir_utterance_n")
  stopifnot(nrow(long)==72L,nrow(proto)==36L,
    all(vapply(long[counts],function(v) all(is.finite(v)&v>=0&v==floor(v)),logical(1))),
    all(long$utterance_n>0),all(long$oir_n<=long$utterance_n),
    all(long$oir_T1_n+long$oir_T2_n+long$oir_T3_n==long$oir_n),
    all(long$non_oir_utterance_n+long$oir_n==long$utterance_n),
    !anyDuplicated(long[c("session_id","speech_source")]))
  # Match count summaries without treating ambient CSS as CDS or child speech.
  for(src in SOURCES) {
    actual <- long[long$speech_source==src,]
    p <- merge(actual,proto,by=c("language","target_child","target_age"))
    expected <- if(src=="child") c("utterances_TCH","OIR_cs_all","OIR_cs_T1","OIR_cs_T2","OIR_cs_T3") else
      c("utterances_CDS","OIR_cds_all","OIR_cds_T1","OIR_cds_T2","OIR_cds_T3")
    for(k in 1:5) stopifnot(all(p[[counts[k]]]==p[[expected[k]]]))
  }
  long$language <- factor(long$language,levels=LANGUAGES)
  long$speech_source <- factor(long$speech_source,levels=SOURCES)
  long$child_id <- paste(long$language,long$target_child,sep="::")
  long <- long[order(long$language,long$child_id,long$age_months,long$speech_source),]
  rownames(long) <- NULL
  age <- long$age_months[long$speech_source=="child"]
  mean_age <- mean(age); sd_age <- sd(age)
  long$age_z <- (long$age_months-mean_age)/sd_age
  cds <- long[long$speech_source=="CDS",c("session_id","oir_n","utterance_n")]
  cds$cds_rate_per1000 <- 1000*cds$oir_n/cds$utterance_n
  long$cds_rate_per1000 <- cds$cds_rate_per1000[match(long$session_id,cds$session_id)]
  # Calculate the language-specific scale on 12 unique sessions, not 24 source rows.
  child <- long[long$speech_source=="child",]
  child$cds_rate_z_lang <- ave(child$cds_rate_per1000,child$language,
                             FUN=function(v) as.numeric(scale(v)))
  long$cds_rate_z_lang <- child$cds_rate_z_lang[match(long$session_id,child$session_id)]
  child$child_cds_mean <- ave(child$cds_rate_per1000,child$child_id,FUN=mean)
  child$lang_cds_mean <- ave(child$cds_rate_per1000,child$language,FUN=mean)
  child$lang_cds_sd <- ave(child$cds_rate_per1000,child$language,FUN=sd)
  child$cds_within_z <- (child$cds_rate_per1000-child$child_cds_mean)/child$lang_cds_sd
  child$cds_between_z <- (child$child_cds_mean-child$lang_cds_mean)/child$lang_cds_sd
  stopifnot(max(abs(child$cds_within_z+child$cds_between_z-child$cds_rate_z_lang))<1e-12,
            length(unique(child$child_id))==6L,sum(long$utterance_n)==49168L,
            sum(long$oir_n)==1452L)
  for(lang in LANGUAGES) stopifnot(length(unique(child$child_id[child$language==lang]))==2L)
  rownames(child) <- NULL
  write_table(long,"session_source_counts.csv","data/analysis")
  write_table(child,"child_rate_predictors.csv","data/analysis")
  scales <- data.frame(variable="age_months",center=mean_age,scale=sd_age,
    definition="Across 36 child sessions; identical transformation in all primary models")
  for(lang in LANGUAGES) {
    v <- child$cds_rate_per1000[child$language==lang]
    scales <- rbind(scales,data.frame(variable=paste0("CDS_rate_",lang),
      center=mean(v),scale=sd(v),definition="Across 12 sessions in that language"))
  }
  write_table(scales,"standardization.csv","data/analysis")
  list(long=long,child=child,mean_age=mean_age,sd_age=sd_age,
       original_data_sha256=sha,age_reconciliation=discrepancies)
}
make_model_data <- function(cfg) {
  dat <- if(cfg$kind=="child") DATA$child else DATA$long
  age_center <- DATA$mean_age; age_scale <- DATA$sd_age
  if(isTRUE(cfg$age39)) {
    dat$age_months[dat$child_id=="chintang::LDCh2" & dat$target_age=="3;04.00"] <- 39
    v <- if(cfg$kind=="child") dat$age_months else dat$age_months[dat$speech_source=="child"]
    age_center <- mean(v); age_scale <- sd(v)
    dat$age_z <- (dat$age_months-age_center)/age_scale
  }
  children <- unique(as.character(dat$child_id))
  child_language <- vapply(children,function(c) unique(as.character(dat$language[dat$child_id==c])),character(1))
  if(cfg$kind!="format") {
    f <- if(cfg$kind=="joint") ~0+language:speech_source+language:speech_source:age_z else
      if(isTRUE(cfg$within_between)) ~0+language+language:age_z+language:cds_within_z+language:cds_between_z else
        ~0+language+language:age_z+language:cds_rate_z_lang
    if(isTRUE(cfg$quadratic)) f <- if(cfg$kind=="joint")
      update(f,~.+speech_source:I(age_z^2)) else update(f,~.+I(age_z^2))
    environment(f) <- baseenv()
    X <- model.matrix(f,data=dat)
    Z <- if(cfg$kind=="child") cbind(Intercept=1,age_z=dat$age_z,
      input=if(isTRUE(cfg$within_between)) dat$cds_within_z else dat$cds_rate_z_lang) else
        cbind(child=as.integer(dat$speech_source=="child"),CDS=as.integer(dat$speech_source=="CDS"),
          child_age=as.integer(dat$speech_source=="child")*dat$age_z,
          CDS_age=as.integer(dat$speech_source=="CDS")*dat$age_z)
    baseline <- !grepl("age_z|cds_rate_z_lang|cds_within_z|cds_between_z",colnames(X))
    stan <- list(N=nrow(dat),K=ncol(X),M=ncol(Z),C=length(children),
      y=as.integer(dat$oir_n),trials=as.integer(dat$utterance_n),
      X=unname(X),Z=unname(Z),child=match(dat$child_id,children),
      b_mean=ifelse(baseline,cfg$baseline_mean,0),
      b_sd=ifelse(baseline,cfg$baseline_sd,cfg$slope_sd),
      sd_scale=cfg$sd_scale,phi_shape=cfg$phi_shape,phi_rate=cfg$phi_rate,
      correlated=as.integer(cfg$correlated))
    list(stan_data=stan,analysis_data=dat,children=children,child_language=child_language,
      design_formula=f,coefficient_names=colnames(X),varying_names=colnames(Z),
      age_center=age_center,age_scale=age_scale,stan_file="rates_beta_binomial.stan")
  } else {
    conditions <- as.vector(t(outer(LANGUAGES,SOURCES,paste,sep="_")))
    y <- cbind(dat$non_oir_utterance_n,dat$oir_T1_n,dat$oir_T2_n,dat$oir_T3_n)
    stopifnot(all(rowSums(y)==dat$utterance_n))
    stan <- list(N=nrow(dat),G=6L,C=length(children),V=if(isTRUE(cfg$shared_child)) 1L else 6L,
      y=unname(y),condition=match(paste(dat$language,dat$speech_source,sep="_"),conditions),
      source=match(dat$speech_source,SOURCES),child=match(dat$child_id,children),
      age_z=dat$age_z,baseline_mean=cfg$baseline_mean,baseline_sd=cfg$baseline_sd,
      slope_sd=cfg$slope_sd,sd_scale=cfg$sd_scale,phi_shape=cfg$phi_shape,phi_rate=cfg$phi_rate)
    list(stan_data=stan,analysis_data=dat,children=children,child_language=child_language,
      condition_levels=conditions,age_center=age_center,age_scale=age_scale,
      stan_file=if(isTRUE(cfg$quadratic)) "formats_dirichlet_multinomial_quadratic.stan" else "formats_dirichlet_multinomial.stan")
  }
}
