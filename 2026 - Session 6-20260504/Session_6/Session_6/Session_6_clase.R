# Session 6

library(rstan)
library(bayesplot)
library(tidyverse)

# Exercise: hungover holiday regressions ----------------------------------

# a, b) data
df <- read_csv(file = "data/hangover.csv")
df

# c) interpretation

ggplot(df) +
  geom_point(aes(date, volume, col=holiday))

ggplot(df) +
  geom_jitter(aes(holiday, volume))

# d) stan model
T <- nrow(df)

data_list <- list(
  T = T, 
  v = df$volume,
  h = df$holiday)

# f) run the model using Stan
fit <- stan("stan_models/hangover.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

print(fit)

print(fit, pars = "increase", probs = c(0.1, 0.9))


posterior <- as.data.frame(fit)
names(posterior)


plot_title <- ggtitle("Posterior distributions of beta0, beta1 and sigma", "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("beta0", "beta1", "sigma"), 
           prob = 0.8) + plot_title



# Exercise 4.1 - Suicides -------------------------------------------------

# a) p point estimate

## Stan model
data_list <- list(

  )

fit2 <- stan("stan_models/4-1_suicides.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(fit2)

posterior <- as.data.frame(fit2)


### Autocorrelation plot

### Samples vs. iteration plot

### Posterior distribution



## Point estimate:


# b) posterior predictive
n_sim <- 10000
post_predict <- 

ggplot(tibble(post_predict)) +
  geom_bar(aes(x = post_predict)) +
  ggtitle("Posterior predictive of y", "for 103 missing patients")

## credibility interval


