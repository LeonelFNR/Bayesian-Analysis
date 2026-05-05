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


##'Volumen = beta0 + beta1 * holiday
##'
##'holiday = 0: Volumen = beta0 
##'holiday = 1: Volumen = beta0 + beta1 

##' Increase: Volumen_holiday / Volumen_noholiday
##' (beta0 + beta1) / beta0

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

posterior %>% 
  mutate(increase_2 = (beta0 + beta1)/beta0)
  
  
plot_title <- ggtitle("Posterior distributions of beta0, beta1 and sigma", "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("beta0", "beta1", "sigma"), 
           prob = 0.8) + plot_title



# Exercise 4.1 - Suicides -------------------------------------------------

# a) p point estimate

## Stan model
data_list <- list(
    n=874,
    y=103
  )

fit2 <- stan("stan_models/4-1_suicides.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1, thin=2)

## Convergence analysis
print(fit2)

posterior <- as.data.frame(fit2)


### Autocorrelation plot
acf(posterior[, "p"])

### Samples vs. iteration plot
traceplot(fit2)

### Posterior distribution
p <- posterior[, "p"]
ggplot(tibble(p)) +
  geom_density(aes(p))


plot_title <- ggtitle("Posterior distributions of p", "with medians and 80% intervals")
mcmc_areas(posterior, 
           pars = c("p"), 
           prob = 0.8) + plot_title


## Point estimate:
mean(p)
median(p)


# b) posterior predictive: y
n_sim <- 10000
post_predict <- rbinom(n_sim, 100, p)

ggplot(tibble(post_predict)) +
  geom_bar(aes(x = post_predict)) +
  ggtitle("Posterior predictive of y", "for 100 missing patients")

## credibility interval
quantile(post_predict, c(0.025, 0.975))


