required_packages <- function(fitting=FALSE) {
  packages <- c("posterior","dplyr","tidyr","ggplot2","gridExtra","jsonlite","digest")
  if (fitting) packages <- c(packages,"cmdstanr")
  absent <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
  if (length(absent)) stop("Install missing R packages: ",paste(absent,collapse=", "),
                          ". See scripts/install_dependencies.R.")
}
write_table <- function(x, name, directory="tables") {
  dir.create(file.path(ROOT,directory),recursive=TRUE,showWarnings=FALSE)
  write.csv(x,file.path(ROOT,directory,name),row.names=FALSE,na="")
  invisible(x)
}
summarize_vector <- function(x) {
  stopifnot(all(is.finite(x)))
  data.frame(median=median(x),lower=unname(quantile(x,.025)),
    upper=unname(quantile(x,.975)),P_gt0=mean(x>0),P_lt0=mean(x<0))
}
plain_draw_matrix <- function(draws) {
  d <- posterior::as_draws_matrix(draws)
  matrix(as.numeric(d),nrow=nrow(d),ncol=ncol(d),dimnames=dimnames(d))
}
draw_matrix <- function(model) {
  if(!is.null(attr(model,"matrix_cache"))) return(attr(model,"matrix_cache"))
  plain_draw_matrix(model$draws)
}
model_file <- function(name, sensitivity=FALSE) {
  file.path(ROOT,"models",if(sensitivity) "sensitivity" else "primary",
            paste0(name,".rds"))
}
load_model <- function(name, cfg, sensitivity=FALSE) {
  path <- model_file(name,sensitivity)
  if (!file.exists(path)) stop("Missing fitted model: ",path,". Run --mode refit.")
  m <- readRDS(path)
  expected <- make_model_data(cfg)
  expected_sampling <- if(sensitivity) SENSITIVITY_SAMPLING else SAMPLING
  expected_seed <- if(sensitivity) SENSITIVITY_SEEDS[[name]] else PRIMARY_SEEDS[[name]]
  metadata_matches <- all(vapply(names(expected),function(field) {
    if(field=="design_formula") identical(deparse(m[[field]]),deparse(expected[[field]])) else
      isTRUE(all.equal(m[[field]],expected[[field]],tolerance=1e-12))
  },logical(1)))
  if (!identical(m$name,name) || !identical(m$seed,expected_seed) ||
      !identical(m$config,cfg) || !identical(m$sampling,expected_sampling) ||
      !metadata_matches ||
      !isTRUE(all.equal(m$analysis_data,expected$analysis_data,tolerance=1e-12)) ||
      !isTRUE(all.equal(m$stan_data,expected$stan_data,tolerance=1e-12)) ||
      !identical(m$original_data_sha256,DATA$original_data_sha256))
    stop(name,": current source data or model configuration differs from the fit. Refit explicitly.")
  if (!identical(m$stan_code,readLines(file.path(ROOT,"stan",m$stan_file),warn=FALSE)))
    stop(name,": Stan code differs from the saved fit. Refit explicitly.")
  stopifnot(inherits(m,"oir_model"),posterior::nchains(m$draws)==4L)
  attr(m,"matrix_cache") <- plain_draw_matrix(m$draws)
  m
}
coefficient_draws <- function(model) {
  d <- draw_matrix(model)
  out <- d[,paste0("b[",seq_along(model$coefficient_names),"]"),drop=FALSE]
  colnames(out) <- model$coefficient_names
  out
}
new_rate_design <- function(model,language,source="child",age_z=0,input_z=0,
                            within_z=0,between_z=0) {
  d <- data.frame(language=factor(language,levels=LANGUAGES),
    speech_source=factor(source,levels=SOURCES),age_z=age_z,
    cds_rate_z_lang=input_z,cds_within_z=within_z,cds_between_z=between_z)
  X <- model.matrix(model$design_formula,data=d)
  stopifnot(identical(colnames(X),model$coefficient_names))
  X
}
rate_predictions <- function(model,newdata=NULL,conditional=FALSE) {
  if (is.null(newdata)) newdata <- model$analysis_data
  X <- model.matrix(model$design_formula,data=newdata)
  stopifnot(identical(colnames(X),model$coefficient_names))
  eta <- coefficient_draws(model) %*% t(X)
  if (conditional) {
    d <- draw_matrix(model)
    for(i in seq_len(nrow(newdata))) {
      c <- match(as.character(newdata$child_id[i]),model$children)
      if (is.na(c)) stop("Conditional predictions require a fitted child.")
      Z <- if(model$config$kind=="child")
        c(1,newdata$age_z[i],if(isTRUE(model$config$within_between))
          newdata$cds_within_z[i] else newdata$cds_rate_z_lang[i]) else
        c(as.integer(newdata$speech_source[i]=="child"),
          as.integer(newdata$speech_source[i]=="CDS"),
          as.integer(newdata$speech_source[i]=="child")*newdata$age_z[i],
          as.integer(newdata$speech_source[i]=="CDS")*newdata$age_z[i])
      for(k in seq_along(Z)) eta[,i] <- eta[,i]+Z[k]*d[,paste0("u[",k,",",c,"]")]
    }
  }
  plogis(eta)
}
format_probabilities <- function(model,newdata=NULL,conditional=TRUE) {
  if (is.null(newdata)) newdata <- model$analysis_data
  d <- draw_matrix(model); S <- nrow(d); N <- nrow(newdata)
  result <- array(NA_real_,dim=c(S,N,4),
                  dimnames=list(NULL,NULL,c("non_repair",TYPES)))
  for(i in seq_len(N)) {
    g <- match(paste(newdata$language[i],newdata$speech_source[i],sep="_"),
               model$condition_levels)
    src <- match(as.character(newdata$speech_source[i]),SOURCES)
    c <- match(as.character(newdata$child_id[i]),model$children)
    eta <- matrix(0,S,4)
    for(k in 1:3) {
      eta[,k+1] <- d[,paste0("baseline[",g,",",k,"]")]+
                   newdata$age_z[i]*d[,paste0("age_slope[",g,",",k,"]")]
      if(isTRUE(model$config$quadratic)) eta[,k+1] <- eta[,k+1]+
        newdata$age_z[i]^2*d[,paste0("quadratic[",src,"]")]
      if (conditional) {
        if (is.na(c)) stop("Conditional predictions require a fitted child.")
        v <- if(isTRUE(model$config$shared_child)) 1L else (src-1L)*3L+k
        eta[,k+1] <- eta[,k+1]+d[,paste0("u[",v,",",c,"]")]
      }
    }
    eta <- eta-pmax(eta[,1],eta[,2],eta[,3],eta[,4])
    e <- exp(eta)
    result[,i,] <- e/rowSums(e)
  }
  stopifnot(max(abs(result[,,1]+result[,,2]+result[,,3]+result[,,4]-1))<1e-12,
            all(result>0),all(result<1))
  result
}
format_cohort <- function(model,lang,src,age_z=0,conditional_share=FALSE) {
  ids <- model$children[model$child_language==lang]
  stopifnot(length(ids)==2L)
  new <- data.frame(language=factor(rep(lang,2),levels=LANGUAGES),
    speech_source=factor(rep(src,2),levels=SOURCES),child_id=ids,age_z=age_z)
  p <- format_probabilities(model,new,TRUE)[,,2:4,drop=FALSE]
  if (conditional_share) {
    total <- p[,,1]+p[,,2]+p[,,3]
    for(k in 1:3) p[,,k] <- p[,,k]/total
  }
  # Equal-child arithmetic means of probabilities preserve the category partition.
  (p[,1,]+p[,2,])/2
}
portable_diagnostics <- function(model) {
  s <- posterior::summarise_draws(model$draws)
  core <- !grepl("^log_lik\\[",s$variable)
  ss <- model$sampler
  bfmi <- vapply(split(ss$energy__,ss$chain),
    function(x) mean(diff(x)^2)/var(x),numeric(1))
  data.frame(model=model$name,draws=posterior::ndraws(model$draws),
    max_Rhat=max(s$rhat[core],na.rm=TRUE),
    min_bulk_ESS=min(s$ess_bulk[core],na.rm=TRUE),
    min_tail_ESS=min(s$ess_tail[core],na.rm=TRUE),
    divergences=sum(ss$divergent__),
    maximum_treedepth_hits=sum(ss$treedepth__>=model$sampling$max_treedepth),
    min_E_BFMI=min(bfmi))
}
