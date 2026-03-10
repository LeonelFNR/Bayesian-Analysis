
# Example 1 - simple ------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores())

# Generative normal model
N <- 10
mu <- 1.5
sigma <- 0.1
Y <- rnorm(N, mu, sigma)

# Data
data_list <- list(
  N = N,
  Y = Y)

# Compile and run the MCMC on the Stan program
set.seed(1234)
fit <- stan("stan_models/simple.stan", iter = 2000, chains = 4,
  data = data_list)

fit


# Interpreting the results  -----------------------------------------------
# Model Summary
print(fit, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95))

# Posterior predictions
posterior <- as.matrix(fit)
colnames(posterior)
# Autocorrelation plot
acf(posterior[, "mu"])
acf(posterior[, "sigma"])

# Samples vs. iteration plot
traceplot(fit)

# Posterior of prob1
plot(density(posterior[, "mu"]))
plot(density(posterior[, "sigma"]))


library(bayesplot)

plot_title <- ggtitle("Posterior distributions of mu and sigma", "with medians and 80% intervals")
mcmc_areas(posterior, 
  pars = c("mu", "sigma"), 
  prob = 0.8) + plot_title

posterior
mcmc_trace(posterior, 
  pars = c("mu", "sigma"))

mcmc_trace(posterior, 
           pars = c("mu", "sigma"),
           facet_args = list(nrow = 2))

#save(posterior, file=paste("results/simple.RData", sep=""))


# Example 2 - Poisson -----------------------------------------------------

# a, b, c, d) discoveries.stan model

# e) data

# f) run the model using Stan


# g) diagnose whether your model has converged by printing `fit`



# h) equivalent number of samples



# i) central posterior 80% credible interval for lambda



# j) histogram of the lambda posterior sample



# k) graph the data




# l) posterior predictive

