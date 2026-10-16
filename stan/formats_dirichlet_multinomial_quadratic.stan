data {
  int<lower=1> N;
  int<lower=1> G;
  int<lower=1> C;
  int<lower=1> V;
  array[N,4] int<lower=0> y;
  array[N] int<lower=1,upper=G> condition;
  array[N] int<lower=1,upper=2> source;
  array[N] int<lower=1,upper=C> child;
  vector[N] age_z;
  real baseline_mean;
  real<lower=0> baseline_sd;
  real<lower=0> slope_sd;
  real<lower=0> sd_scale;
  real<lower=0> phi_shape;
  real<lower=0> phi_rate;
}
parameters {
  matrix[G,3] baseline;
  matrix[G,3] age_slope;
  vector<lower=0>[V] group_sd;
  matrix[V,C] z;
  real<lower=0> phi;
  vector[2] quadratic;
}
transformed parameters {
  matrix[V,C] u = diag_matrix(group_sd)*z;
}
model {
  to_vector(baseline) ~ normal(baseline_mean,baseline_sd);
  to_vector(age_slope) ~ normal(0,slope_sd);
  group_sd ~ normal(0,sd_scale);
  to_vector(z) ~ std_normal();
  phi ~ gamma(phi_shape,phi_rate);
  quadratic ~ normal(0,slope_sd);
  for (i in 1:N) {
    vector[4] eta;
    eta[1] = 0;
    for (k in 1:3) {
      int v = V == 1 ? 1 : (source[i]-1)*3+k;
      eta[k+1] = baseline[condition[i],k]+age_slope[condition[i],k]*age_z[i]+u[v,child[i]]+quadratic[source[i]]*square(age_z[i]);
    }
    y[i] ~ dirichlet_multinomial(phi*softmax(eta));
  }
}
generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    vector[4] eta;
    eta[1] = 0;
    for (k in 1:3) {
      int v = V == 1 ? 1 : (source[i]-1)*3+k;
      eta[k+1] = baseline[condition[i],k]+age_slope[condition[i],k]*age_z[i]+u[v,child[i]]+quadratic[source[i]]*square(age_z[i]);
    }
    log_lik[i] = dirichlet_multinomial_lpmf(y[i] | phi*softmax(eta));
  }
}
