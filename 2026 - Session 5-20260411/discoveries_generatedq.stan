data {
  int<lower=0> N;     // sample size
  int<lower=0> x[N];  //number of invention
}

parameters {
  real<lower=0> lambda;
}

model {
  x ~ poisson(lambda);  	// likelihood
  
  lambda ~ lognormal(2, 1); // prior for lambda
}

generated quantities {
  int<lower=0> x_sim[N];
  
  for(i in 1:N){
    x_sim[i] = poisson_rng(lambda);
  }
}