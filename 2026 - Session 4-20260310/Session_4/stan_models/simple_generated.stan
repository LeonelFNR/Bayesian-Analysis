data {
  int N;      // sample size
  real Y[N]; // heights for 10 people
}

parameters {
  real mu; // mean height in population
  real<lower=0> sigma; // sd for height distribution
}

model {
  for (i in 1:N) {
    Y[i] ~ normal(mu, sigma); // likelihood
  }
  
  mu ~ normal(1.5, 0.1);  // prior for mu
  sigma ~ gamma(1, 1);    // prior for sigma
}

generated quantities {
  vector[N] lSimData;
  int aMax_indicator;
  int aMin_indicator;
  
  // Generate posterior predictive samples
  for (i in 1:N) {
    lSimData[i] = normal_rng(mu, sigma);  
  }
  
  // Compare with real data
  aMax_indicator = max(lSimData) > max(Y);
  aMin_indicator = min(lSimData) > min(Y);
}