data {
  int<lower=1> N;
  int<lower=1> K;
  int<lower=1> M;
  int<lower=1> C;
  array[N] int<lower=0> y;
  array[N] int<lower=1> trials;
  matrix[N,K] X;
  matrix[N,M] Z;
  array[N] int<lower=1,upper=C> child;
  vector[K] b_mean;
  vector<lower=0>[K] b_sd;
  real<lower=0> sd_scale;
  real<lower=0> phi_shape;
  real<lower=0> phi_rate;
  int<lower=0,upper=1> correlated;
}
parameters {
  vector[K] b;
  vector<lower=0>[M] group_sd;
  matrix[M,C] z;
  array[correlated] cholesky_factor_corr[M] L;
  real<lower=0> phi;
}
transformed parameters {
  matrix[M,C] u;
  vector[N] eta = X*b;
  if (correlated == 1) u = diag_pre_multiply(group_sd,L[1])*z;
  else u = diag_matrix(group_sd)*z;
  for (i in 1:N) eta[i] += Z[i]*u[,child[i]];
}
model {
  b ~ normal(b_mean,b_sd);
  group_sd ~ normal(0,sd_scale);
  to_vector(z) ~ std_normal();
  if (correlated == 1) L[1] ~ lkj_corr_cholesky(2);
  phi ~ gamma(phi_shape,phi_rate);
  {
    vector[N] p = inv_logit(eta);
    y ~ beta_binomial(trials,p*phi,(1-p)*phi);
  }
}
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    real p = inv_logit(eta[i]);
    log_lik[i] = beta_binomial_lpmf(y[i] | trials[i],p*phi,(1-p)*phi);
  }
}
