fit_one_model <- function(name,cfg,sensitivity=FALSE,seed=2026100903L) {
  required_packages(TRUE)
  if(nzchar(Sys.getenv("OIR_CMDSTAN_PATH"))) cmdstanr::set_cmdstan_path(Sys.getenv("OIR_CMDSTAN_PATH"))
  sampling <- if(sensitivity) SENSITIVITY_SAMPLING else SAMPLING
  prepared <- make_model_data(cfg)
  directory <- file.path(ROOT,"verification","sampling_work",name)
  if(dir.exists(directory) && length(list.files(directory,pattern="\\.csv$")))
    stop("Existing raw refit output for ",name,". Archive/remove that model's sampling_work folder before refitting.")
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  stan_path <- file.path(ROOT,"stan",prepared$stan_file)
  stan <- cmdstanr::cmdstan_model(stan_path,quiet=TRUE)
  init <- if(cfg$kind=="format") function() {
    out <- list(baseline=matrix(cfg$baseline_mean,6,3),age_slope=matrix(0,6,3),
      group_sd=rep(.2,prepared$stan_data$V),
      z=matrix(0,prepared$stan_data$V,prepared$stan_data$C),phi=40)
    if(isTRUE(cfg$quadratic)) out$quadratic <- rep(0,2)
    out
  } else
      function() list(b=prepared$stan_data$b_mean,group_sd=rep(.2,prepared$stan_data$M),
        z=matrix(0,prepared$stan_data$M,prepared$stan_data$C),
        L=if(cfg$correlated) list(diag(prepared$stan_data$M)) else list(),phi=40)
  cat("Fitting",name,"(",cfg$family,")\n")
  fit <- stan$sample(data=prepared$stan_data,chains=sampling$chains,
    parallel_chains=sampling$parallel_chains,iter_warmup=sampling$iter_warmup,
    iter_sampling=sampling$iter_sampling,adapt_delta=sampling$adapt_delta,
    max_treedepth=sampling$max_treedepth,seed=seed,init=init,
    refresh=0,sig_figs=sampling$sig_figs,output_dir=directory)
  raw_files <- fit$output_files()
  # Verify the actual files before serializing; a completion message alone is insufficient.
  for(f in raw_files) {
    lines <- readLines(f,warn=FALSE)
    records <- lines[nzchar(lines) & !startsWith(lines,"#")]
    if(length(records)!=sampling$iter_sampling+1L ||
       !any(grepl("Elapsed Time:",tail(lines,10),fixed=TRUE)))
      stop(name,": sampling CSV is incomplete: ",basename(f))
  }
  draws <- posterior::as_draws_array(fit$draws())
  stopifnot(posterior::nchains(draws)==4L,
            posterior::ndraws(draws)==4L*sampling$iter_sampling)
  sd <- fit$sampler_diagnostics(format="draws_array")
  ss <- do.call(rbind,lapply(seq_len(dim(sd)[2]),function(c) {
    values <- matrix(as.numeric(sd[,c,]),nrow=dim(sd)[1],ncol=dim(sd)[3])
    colnames(values) <- dimnames(sd)[[3]]
    d <- as.data.frame(values,check.names=FALSE)
    d$chain <- c; d$iteration <- seq_len(nrow(d)); d
  }))
  m <- c(list(name=name,config=cfg,sampling=sampling,seed=seed,
    draws=draws,sampler=ss,original_data_sha256=DATA$original_data_sha256,
    stan_code=readLines(stan_path,warn=FALSE),
    runtime=list(R=as.character(getRversion()),cmdstan=as.character(cmdstanr::cmdstan_version()),
      cmdstanr=as.character(utils::packageVersion("cmdstanr")),
      posterior=as.character(utils::packageVersion("posterior"))),
    implementation="Stan through cmdstanr; portable R list, not a brmsfit/R6 object"),
    prepared)
  class(m) <- c("oir_model","list")
  path <- model_file(name,sensitivity)
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  saveRDS(m,path,compress="xz")
  # Read back each fit and compare all draws; never deliver a pointer to external files.
  check <- readRDS(path)
  stopifnot(identical(m$draws,check$draws),identical(m$stan_data,check$stan_data))
  write_table(posterior::summarise_draws(draws),paste0(name,"_parameters.csv"),"diagnostics")
  write_table(portable_diagnostics(m),paste0(name,"_sampler_summary.csv"),"diagnostics")
  # Preserve the original complete CSV bytes as compressed, relative-path artifacts.
  dir.create(file.path(ROOT,"models","draws",name),recursive=TRUE,showWarnings=FALSE)
  for(i in seq_along(raw_files)) {
    target <- file.path(ROOT,"models","draws",name,paste0("chain_",i,".csv.gz"))
    input <- file(raw_files[i],"rb"); output <- gzfile(target,"wb")
    repeat {
      chunk <- readBin(input,"raw",n=1024L*1024L)
      if(!length(chunk)) break
      writeBin(chunk,output)
    }
    close(input);close(output)
  }
  cat("Saved and verified",name,"\n")
  invisible(m)
}
fit_requested_models <- function(include_sensitivity=TRUE) {
  for(i in seq_along(MODEL_CONFIG))
    fit_one_model(names(MODEL_CONFIG)[i],MODEL_CONFIG[[i]],FALSE,PRIMARY_SEEDS[[names(MODEL_CONFIG)[i]]])
  if(include_sensitivity) for(i in seq_along(SENSITIVITY_CONFIG))
    fit_one_model(names(SENSITIVITY_CONFIG)[i],SENSITIVITY_CONFIG[[i]],TRUE,SENSITIVITY_SEEDS[[names(SENSITIVITY_CONFIG)[i]]])
}
