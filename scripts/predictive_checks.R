# Posterior predictive draws are generated for the observed children and designs.
# These checks diagnose selected features; they do not certify a model as true.
simulate_counts <- function(p,phi,trials) {
  S <- length(phi); N <- length(trials)
  if(length(dim(p))==2L) {
    result <- matrix(0L,S,N)
    for(i in seq_len(N)) result[,i] <- rbinom(S,trials[i],
      rbeta(S,phi*p[,i],phi*(1-p[,i])))
    return(result)
  }
  result <- array(0L,c(S,N,4))
  for(i in seq_len(N)) {
    alpha <- p[,i,]*phi
    small <- alpha<1
    # Gamma(a) = Gamma(a+1) * U^(1/a) for a<1. Work on the log scale
    # so rare-category Gamma draws cannot all underflow before normalization.
    log_q <- matrix(log(rgamma(S*4,shape=as.vector(alpha+small))),S,4)
    log_q[small] <- log_q[small]+log(runif(sum(small)))/alpha[small]
    stopifnot(all(is.finite(log_q)))
    q <- exp(log_q-pmax(log_q[,1],log_q[,2],log_q[,3],log_q[,4]))
    q <- q/rowSums(q)
    remaining <- rep(trials[i],S)
    for(k in 1:3) {
      denominator <- rowSums(q[,k:4,drop=FALSE])
      stopifnot(all(denominator>0 | remaining==0))
      conditional_p <- rep(0,S)
      supported <- denominator>0
      conditional_p[supported] <- q[supported,k]/denominator[supported]
      # Floating point rounding alone can move this ratio outside [0,1].
      conditional_p <- pmax(0,pmin(1,conditional_p))
      result[,i,k] <- rbinom(S,remaining,conditional_p)
      remaining <- remaining-result[,i,k]
    }
    result[,i,4] <- remaining
  }
  result
}
predictive_discrepancies <- function(model,p,phi,yrep) {
  y <- model$stan_data$y; n <- model$analysis_data$utterance_n
  S <- length(phi); dat <- model$analysis_data
  is_format <- model$config$kind=="format"
  obs <- rep <- rep(0,S)
  if(!is_format) {
    for(i in seq_along(n)) {
      mu <- n[i]*p[,i]
      variance <- n[i]*p[,i]*(1-p[,i])*(n[i]+phi)/(1+phi)
      obs <- obs+(y[i]-mu)^2/variance
      rep <- rep+(yrep[,i]-mu)^2/variance
    }
  } else {
    for(i in seq_along(n)) for(k in 1:4) {
      mu <- n[i]*p[,i,k]
      variance <- n[i]*p[,i,k]*(1-p[,i,k])*(n[i]+phi)/(1+phi)
      obs <- obs+(y[i,k]-mu)^2/variance
      rep <- rep+(yrep[,i,k]-mu)^2/variance
    }
  }
  values <- list(Pearson_marginal=list(observed=obs,replicated=rep))
  # DM category residuals are dependent; this is a discrepancy, not a chi-square test.
  if(!is_format) {
    values$zero_rows <- list(observed=rep(sum(y==0),S),replicated=rowSums(yrep==0))
    values$total_OIR <- list(observed=rep(sum(y),S),replicated=rowSums(yrep))
  } else {
    for(k in 2:4) {
      values[[paste0("zero_",TYPES[k-1])]] <- list(observed=rep(sum(y[,k]==0),S),
        replicated=rowSums(yrep[,,k]==0))
      values[[paste0("total_",TYPES[k-1])]] <- list(observed=rep(sum(y[,k]),S),
        replicated=rowSums(yrep[,,k]))
    }
  }
  if(model$config$kind=="joint") {
    i <- which(dat$speech_source=="child")
    j <- match(dat$session_id[i],dat$session_id[dat$speech_source=="CDS"])
    j <- which(dat$speech_source=="CDS")[j]
    # Same-session residual correlation is not explicitly modeled by the primary fit.
    observed <- replicated <- rep(NA_real_,S)
    for(s in seq_len(S)) {
      mu <- n*p[s,]
      v <- n*p[s,]*(1-p[s,])*(n+phi[s])/(1+phi[s])
      ro <- (y-mu)/sqrt(v);rr <- (yrep[s,]-mu)/sqrt(v)
      observed[s] <- cor(ro[i],ro[j]);replicated[s] <- cor(rr[i],rr[j])
    }
    values$paired_source_residual_correlation <- list(observed=observed,replicated=replicated)
  }
  # Age-pattern discrepancy evaluates residual trends that linear age terms may miss.
  ages <- dat$age_z
  observed <- replicated <- quadratic_obs <- quadratic_rep <- serial_obs <- serial_rep <- rep(NA_real_,S)
  grouping <- if(model$config$kind=="child") dat$child_id else paste(dat$child_id,dat$speech_source)
  adjacent <- lapply(split(seq_len(nrow(dat)),grouping),function(i) i[order(dat$age_months[i])])
  earlier <- unlist(lapply(adjacent,function(i) head(i,-1)),use.names=FALSE)
  later <- unlist(lapply(adjacent,function(i) tail(i,-1)),use.names=FALSE)
  for(s in seq_len(S)) {
    probability <- if(is_format) rowSums(p[s,,2:4]) else p[s,]
    actual <- if(is_format) rowSums(y[,2:4]) else y
    fake <- if(is_format) rowSums(yrep[s,,2:4]) else yrep[s,]
    mu <- n*probability
    v <- n*probability*(1-probability)*(n+phi[s])/(1+phi[s])
    ro <- (actual-mu)/sqrt(v);rr <- (fake-mu)/sqrt(v)
    observed[s] <- cor(ages,ro);replicated[s] <- cor(ages,rr)
    quadratic_obs[s] <- cor(ages^2,ro);quadratic_rep[s] <- cor(ages^2,rr)
    serial_obs[s] <- cor(ro[earlier],ro[later]);serial_rep[s] <- cor(rr[earlier],rr[later])
  }
  values$age_residual_correlation <- list(observed=observed,replicated=replicated)
  values$squared_age_residual_correlation <- list(observed=quadratic_obs,replicated=quadratic_rep)
  values$adjacent_session_residual_correlation <- list(observed=serial_obs,replicated=serial_rep)
  values
}
verify_log_likelihood <- function(model,p,phi) {
  d <- draw_matrix(model); chosen <- unique(round(seq(1,nrow(d),length.out=100)))
  N <- nrow(model$analysis_data); errors <- numeric(length(chosen)*N); pos <- 0L
  for(s in chosen) for(i in seq_len(N)) {
    if(model$config$kind=="format") {
      y <- model$stan_data$y[i,];n <- sum(y);a <- phi[s]*p[s,i,]
      independent <- lgamma(n+1)-sum(lgamma(y+1))+lgamma(sum(a))-
        lgamma(n+sum(a))+sum(lgamma(y+a)-lgamma(a))
    } else {
      y <- model$stan_data$y[i];n <- model$stan_data$trials[i]
      a <- phi[s]*p[s,i];b <- phi[s]*(1-p[s,i])
      independent <- lchoose(n,y)+lbeta(y+a,n-y+b)-lbeta(a,b)
    }
    pos <- pos+1L;errors[pos] <- abs(independent-d[s,paste0("log_lik[",i,"]")])
  }
  # Requires unrounded sampling CSVs (sig_figs=17 in the fitting script).
  data.frame(model=model$name,evaluations=length(errors),
    maximum_absolute_difference=max(errors),tolerance=1e-7,passed=max(errors)<1e-7)
}
prior_probabilities <- function(model,S=PRIOR_DRAWS) {
  a <- model$stan_data;cfg <- model$config;N <- a$N
  phi <- rgamma(S,cfg$phi_shape,rate=cfg$phi_rate)
  if(cfg$kind!="format") {
    b <- sapply(seq_len(a$K),function(k) rnorm(S,a$b_mean[k],a$b_sd[k]))
    sd <- matrix(abs(rnorm(S*a$M,0,cfg$sd_scale)),S,a$M)
    u <- array(rnorm(S*a$M*a$C),c(S,a$M,a$C))
    for(k in seq_len(a$M)) for(c in seq_len(a$C)) u[,k,c] <- u[,k,c]*sd[,k]
    eta <- b%*%t(a$X)
    for(i in seq_len(N)) for(k in seq_len(a$M)) eta[,i] <- eta[,i]+a$Z[i,k]*u[,k,a$child[i]]
    return(list(p=plogis(eta),phi=phi))
  }
  baseline <- array(rnorm(S*a$G*3,cfg$baseline_mean,cfg$baseline_sd),c(S,a$G,3))
  slope <- array(rnorm(S*a$G*3,0,cfg$slope_sd),c(S,a$G,3))
  quadratic <- if(isTRUE(cfg$quadratic)) matrix(rnorm(S*2,0,cfg$slope_sd),S,2) else matrix(0,S,2)
  sd <- matrix(abs(rnorm(S*a$V,0,cfg$sd_scale)),S,a$V)
  u <- array(rnorm(S*a$V*a$C),c(S,a$V,a$C))
  for(v in seq_len(a$V)) for(c in seq_len(a$C)) u[,v,c] <- u[,v,c]*sd[,v]
  p <- array(NA_real_,c(S,N,4))
  for(i in seq_len(N)) {
    eta <- matrix(0,S,4)
    for(k in 1:3) {
      v <- if(a$V==1L) 1L else (a$source[i]-1L)*3L+k
      eta[,k+1] <- baseline[,a$condition[i],k]+a$age_z[i]*slope[,a$condition[i],k]+
        u[,v,a$child[i]]+quadratic[,a$source[i]]*a$age_z[i]^2
    }
    e <- exp(eta-pmax(eta[,1],eta[,2],eta[,3],eta[,4]));p[,i,] <- e/rowSums(e)
  }
  list(p=p,phi=phi)
}
predictive_checks <- function() {
  all <- c(MODELS,SENSITIVITY)
  SAMPLER_SUMMARY <<- do.call(rbind,lapply(all,portable_diagnostics))
  write_table(SAMPLER_SUMMARY,"all_model_sampler_diagnostics.csv","diagnostics")
  set.seed(PREDICTIVE_SEED)
  result <- list();summary <- list();rows <- list();likelihood <- list()
  for(name in names(all)) {
    model <- all[[name]];d <- draw_matrix(model);phi <- as.numeric(d[,"phi"])
    p <- if(model$config$kind=="format") format_probabilities(model) else
      rate_predictions(model,conditional=TRUE)
    yrep <- simulate_counts(p,phi,model$analysis_data$utterance_n)
    stopifnot(all(is.finite(yrep)),all(yrep>=0),all(yrep==floor(yrep)))
    if(model$config$kind=="format") stopifnot(all(yrep[,,1]+yrep[,,2]+yrep[,,3]+yrep[,,4]==
      rep(model$analysis_data$utterance_n,each=nrow(d))))
    values <- predictive_discrepancies(model,p,phi,yrep)
    likelihood[[name]] <- verify_log_likelihood(model,p,phi)
    for(metric in names(values)) {
      v <- values[[metric]]
      summary[[paste(name,metric)]] <- data.frame(model=name,metric=metric,
        observed_median=median(v$observed),replicated_median=median(v$replicated),
        replicated_lower=unname(quantile(v$replicated,.025)),
        replicated_upper=unname(quantile(v$replicated,.975)),
        Pr_replicated_gt_observed=mean(v$replicated>v$observed),
        Pr_replicated_eq_observed=mean(v$replicated==v$observed))
    }
    result[[name]] <- values
    if(name %in% names(MODELS)) {
      dat <- model$analysis_data
      if(model$config$kind=="format") {
        for(i in seq_len(nrow(dat))) for(k in 1:4) {
          v <- yrep[,i,k]
          rows[[paste(name,i,k)]] <- data.frame(model=name,session_id=dat$session_id[i],
            language=dat$language[i],source=dat$speech_source[i],age_months=dat$age_months[i],
            outcome=c("non_repair",TYPES)[k],observed= model$stan_data$y[i,k],
            expected_median=median(dat$utterance_n[i]*p[,i,k]),
            predictive_lower=unname(quantile(v,.025)),predictive_upper=unname(quantile(v,.975)))
        }
      } else for(i in seq_len(nrow(dat))) {
        v <- yrep[,i]
        rows[[paste(name,i)]] <- data.frame(model=name,session_id=dat$session_id[i],
          language=dat$language[i],source=dat$speech_source[i],age_months=dat$age_months[i],
          outcome="all_OIR",observed=dat$oir_n[i],expected_median=median(dat$utterance_n[i]*p[,i]),
          predictive_lower=unname(quantile(v,.025)),predictive_upper=unname(quantile(v,.975)))
      }
    }
  }
  PPC_SUMMARY <<- do.call(rbind,summary)
  PPC_DRAWS <<- result
  PPC_ROWS <<- do.call(rbind,rows)
  PPC_ROWS$in_95_prediction_interval <<- with(PPC_ROWS,observed>=predictive_lower & observed<=predictive_upper)
  LOG_LIK_CHECK <<- do.call(rbind,likelihood)
  write_table(PPC_SUMMARY,"posterior_predictive_summary.csv","diagnostics")
  write_table(PPC_ROWS,"posterior_predictive_observations.csv","diagnostics")
  write_table(LOG_LIK_CHECK,"independent_likelihood_verification.csv","verification")
  saveRDS(PPC_DRAWS,file.path(ROOT,"diagnostics","predictive_discrepancy_draws.rds"),compress="xz")
  set.seed(PRIOR_SEED)
  prior_rows <- prior_draws <- list()
  for(name in names(MODELS)) {
    m <- MODELS[[name]];dat <- m$analysis_data
    sim <- prior_probabilities(m);yrep <- simulate_counts(sim$p,sim$phi,dat$utterance_n)
    for(lang in LANGUAGES) for(src in unique(as.character(dat$speech_source))) {
      i <- which(dat$language==lang & dat$speech_source==src)
      if(!length(i)) next
      if(m$config$kind=="format") {
        observed <- sum(dat$oir_n[i]);counts <- apply(yrep[,i,2:4,drop=FALSE],1,sum)
        rates <- apply(sim$p[,i,2:4,drop=FALSE],1,function(a)
          sum(a*rep(dat$utterance_n[i],times=3))/sum(dat$utterance_n[i]))*1000
      } else {
        observed <- sum(dat$oir_n[i]);counts <- rowSums(yrep[,i,drop=FALSE])
        rates <- as.numeric(sim$p[,i,drop=FALSE]%*%dat$utterance_n[i])/sum(dat$utterance_n[i])*1000
      }
      fake_rate <- 1000*counts/sum(dat$utterance_n[i]);actual_rate <- 1000*observed/sum(dat$utterance_n[i])
      key <- paste(name,lang,src)
      prior_draws[[key]] <- data.frame(model=name,language=lang,source=src,
        draw=seq_len(PRIOR_DRAWS),expected_rate_per1000=rates,
        replicated_rate_per1000=fake_rate,observed_rate_per1000=actual_rate)
      prior_rows[[key]] <- data.frame(model=name,language=lang,source=src,
        observed_rate_per1000=actual_rate,
        expected_median=median(rates),expected_lower=unname(quantile(rates,.025)),
        expected_upper=unname(quantile(rates,.975)),
        replicated_lower=unname(quantile(fake_rate,.025)),replicated_upper=unname(quantile(fake_rate,.975)),
        Pr_expected_rate_gt100_per1000=mean(rates>100),
        Pr_expected_rate_gt500_per1000=mean(rates>500))
    }
  }
  PRIOR_SUMMARY <<- do.call(rbind,prior_rows)
  PRIOR_DRAWS_TABLE <<- do.call(rbind,prior_draws)
  write_table(PRIOR_SUMMARY,"prior_predictive_summary.csv","diagnostics")
  saveRDS(PRIOR_DRAWS_TABLE,file.path(ROOT,"diagnostics","prior_predictive_draws.rds"),compress="xz")
  # Monte Carlo precision for the actually reported contrasts, preserving chain order.
  mcse <- list()
  for(name in names(SENSITIVITY_CONTRAST_DRAWS)) for(key in names(SENSITIVITY_CONTRAST_DRAWS[[name]])) {
    m <- all[[name]];v <- SENSITIVITY_CONTRAST_DRAWS[[name]][[key]]
    chain_matrix <- matrix(v,nrow=m$sampling$iter_sampling,ncol=4)
    mcse[[paste(name,key)]] <- data.frame(model=name,estimand=key,
      Rhat=posterior::rhat(chain_matrix),bulk_ESS=posterior::ess_bulk(chain_matrix),
      tail_ESS=posterior::ess_tail(chain_matrix),MCSE_mean=posterior::mcse_mean(chain_matrix),
      MCSE_P_gt0=posterior::mcse_mean(chain_matrix>0))
  }
  CONTRAST_PRECISION <<- do.call(rbind,mcse)
  write_table(CONTRAST_PRECISION,"contrast_monte_carlo_precision.csv","diagnostics")
}
