data{ 
  int T;
  real v[T];
  real h[T];
}

parameters{
  real beta0;
  real beta1;
  real<lower=0> sigma;
}

model{
  for(t in 1:T) {
    v[t] ~normal(beta0 + beta1 * h[t], sigma);
  }
  
  beta0 ~ normal(0,50);
  beta1 ~ normal(0,50);
  sigma ~ normal(0,10);
}

generated quantities{
  real increase;
  
  increase = (beta0 + beta1) / beta0;
}  
