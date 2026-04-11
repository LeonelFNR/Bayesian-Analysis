
data {
  int<lower=1> N_boys;
  int<lower=1> N_girls;
  vector[N_boys] y_boys;
  vector[N_girls] y_girls;
}
parameters {
  real mu_boys;
  real mu_girls;
  real<lower=0> sigma_boys;
  real<lower=0> sigma_girls;
}
model {
  // priors
  mu_boys ~ normal(7, 2);
  mu_girls ~ normal(7, 2);
  sigma_boys ~ normal(0, 2);
  sigma_girls ~ normal(0, 2);

  // likelihood
  y_boys ~ normal(mu_boys, sigma_boys);
  y_girls ~ normal(mu_girls, sigma_girls);
}
generated quantities {
  real delta;
  real y_new_girl;
  delta = mu_boys - mu_girls;
  y_new_girl = normal_rng(mu_girls, sigma_girls);
}

