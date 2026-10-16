# Independent moment and numerical-stability checks for predictive simulators.
check_simulators <- function() {
  old_seed <- if(exists(".Random.seed",envir=.GlobalEnv)) get(".Random.seed",envir=.GlobalEnv) else NULL
  on.exit(if(!is.null(old_seed)) assign(".Random.seed",old_seed,envir=.GlobalEnv),add=TRUE)
  set.seed(420)
  S <- 50000L;n <- 120;phi <- 8
  checks <- list()
  add <- function(name,passed,detail) checks[[name]] <<- data.frame(test=name,passed=passed,detail=detail)
  p <- .2;bb <- simulate_counts(matrix(p,S,1),rep(phi,S),n)[,1]
  expected_mean <- n*p;expected_variance <- n*p*(1-p)*(n+phi)/(1+phi)
  add("Beta-binomial empirical mean",abs(mean(bb)-expected_mean)<.5,
    sprintf("Observed %.4f; analytic %.4f; tolerance 0.5",mean(bb),expected_mean))
  add("Beta-binomial empirical variance",abs(var(bb)/expected_variance-1)<.08,
    sprintf("Observed %.4f; analytic %.4f; relative tolerance 0.08",var(bb),expected_variance))
  add("Beta-binomial count bounds",all(bb>=0 & bb<=n & bb==floor(bb)),"All simulated counts are valid integers.")
  probability <- c(.7,.1,.1,.1)
  pr <- array(rep(probability,each=S),c(S,1,4));dm <- simulate_counts(pr,rep(phi,S),n)[,1,]
  expected_mean <- n*probability
  expected_variance <- n*probability*(1-probability)*(n+phi)/(1+phi)
  add("Dirichlet-multinomial empirical means",max(abs(colMeans(dm)-expected_mean))<.5,
    sprintf("Maximum mean difference %.5f; tolerance 0.5",max(abs(colMeans(dm)-expected_mean))))
  add("Dirichlet-multinomial empirical variances",max(abs(apply(dm,2,var)/expected_variance-1))<.08,
    sprintf("Maximum relative variance difference %.5f; tolerance 0.08",max(abs(apply(dm,2,var)/expected_variance-1))))
  add("Dirichlet-multinomial count conservation",all(rowSums(dm)==n),"Every vector sums to its trial count.")
  tiny <- array(rep(c(1-3e-14,rep(1e-14,3)),each=2000),c(2000,1,4))
  v <- simulate_counts(tiny,rep(.2,2000),500)
  add("Rare-category numerical stability",all(is.finite(v)) && all(rowSums(v[,1,])==500),
    "Very small Dirichlet shapes: no missing values; counts still partition 500 trials.")
  out <- do.call(rbind,checks)
  write.csv(out,file.path(ROOT,"verification","simulator_regression_checks.csv"),row.names=FALSE)
  if(!all(out$passed)) stop("Predictive simulation moment/stability check failed.")
  invisible(out)
}
if(sys.nframe()==0L) {
  arg <- commandArgs(FALSE);script <- arg[grepl("^--file=",arg)]
  ROOT <- if(length(script)) dirname(dirname(normalizePath(gsub("~+~"," ",sub("^--file=","",script[1]),fixed=TRUE)))) else getwd()
  source(file.path(ROOT,"scripts","predictive_checks.R"))
  check_simulators()
  cat("All predictive simulator moment/stability checks passed.\n")
}
