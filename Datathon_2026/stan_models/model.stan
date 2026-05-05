data {
  int<lower=0> N;
  int<lower=0> J;
  int<lower=1,upper=J> group[N];
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
  vector[J] u;
}

parameters {
  vector[J] alpha;

  real gamma0;
  real gamma1;
  real<lower=0> sigma_alpha;

  vector[K] beta;
  real<lower=0> sigma;
}

model {
  gamma0 ~ normal(0, 5);
  gamma1 ~ normal(0, 5);
  sigma_alpha ~ normal(0, 5);

  alpha ~ normal(gamma0 + gamma1 * u, sigma_alpha);

  beta ~ normal(0, 3);
  sigma ~ normal(0, 3);

  y ~ normal(alpha[group] + X * beta, sigma);
}