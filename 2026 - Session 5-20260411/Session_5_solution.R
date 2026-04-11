# Session 5

# Exercise: discoveries ---------------------------------------------------

# a, b, c) discoveries.stan model

# d)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# e) data
df <- read_csv(file = "data/evaluation_discoveries.csv")

N <- nrow(df)

data_list <- list(
  N = N, 
  x = df$discoveries,
  a = 2,
  b = 1)

# f) run the model using Stan
fit <- stan("stan_models/discoveries.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

# g) diagnose whether your model has converged by printing `fit`
print(fit)

# h) equivalent number of samples

# i) central posterior 80% credible interval for lambda
## Option 1:
print(fit, pars = "lambda", probs = c(0.1, 0.9))

## Option 2: extract lambda from the fit object
lambda <- extract(fit, "lambda")[[1]]
quantile(lambda, c(0.1, 0.9))
ggplot(tibble(lambda)) +
  geom_density(aes(lambda))

# j) histogram of the lambda posterior sample
lambda <- extract(fit, "lambda")[[1]]
ggplot(tibble(lambda), aes(lambda)) + 
  geom_histogram()


# k) graph the data
ggplot(df) +
  geom_line(aes(time, discoveries), col="blue")

ggplot(df) +
  geom_histogram(aes(discoveries), fill="blue", col = "lightblue", bins = 7)

acf(df$discoveries)


# l) posterior predictive

# Option 1: in R
lambda
post_pred <- map(lambda, ~ rpois(N, .x))


# Option 2: generated_quantities
fit2 <- stan("stan_models/discoveries_generatedq.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

print(fit2)

extract(fit2, pars="x_sim[1]")[[1]]
extract(fit2, pars="x_sim")[[1]]



# m) negative binomial model.extract
fit3 <- stan("stan_models/discoveries_negbin.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)



# Interpreting the results  -----------------------------------------------
# Model Summary
print(fit, probs = c(0.25, 0.5, 0.75))

# Posterior predictions
posterior <- as.matrix(fit)
colnames(posterior)
# Autocorrelation plot
acf(posterior[, "lambda"])

# Samples vs. iteration plot
traceplot(fit)

# Posterior of prob1
plot(density(posterior[, "lambda"]))


library(bayesplot)

plot_title <- ggtitle("Posterior distributions of lambda", "with medians and 80% intervals")
mcmc_areas(posterior, 
  pars = c("lambda"), 
  prob = 0.8) + plot_title

posterior
mcmc_trace(posterior, 
  pars = c("lambda"),
           facet_args = list(nrow = 2))




