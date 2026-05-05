# Session 8 - HIERARCHICAL MODELS

library(rstan)
library(ggplot2)
library(bayesplot)
library(tidyverse)

theme_set(theme_bw())

# Exercise 6.2 - Surgical -------------------------------------------------

# random sample of 12 hospitals from around the country

# The objective of this study is to know the probability of death around all the 
# hospitals in the country, not only in the hospitals that are in the sample

# y[i]: the number of deaths performing a specific cardiac surgery in hospital i-th
# n[i]: number of operations in hospital i-th

N <- 12
df <- tibble(
        hosp = LETTERS[1:N],
        y = c(0, 18, 8, 46, 8, 13, 9, 31, 14, 8, 29, 24),
        n = c(47, 148, 119, 810, 211, 196, 148, 215, 207, 97, 256, 360)
    )



# model_1: pooled model ---------------------------------------------------

## Stan model
data_list <- list(
  N = N, 
  y = df$y, 
  n = df$n, 
  a = 1,
  b = 1
  )

hosp_1 <- stan("stan_models/6-2_hospital_1.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(hosp_1)

### Samples vs. iteration plot
traceplot(hosp_1)

p_post <- rstan::extract(hosp_1)[[1]]
mean_pooled <- mean(p_post)
sum(df$y)/sum(df$n)

df <- df %>% 
  mutate(p_obs = y/n)

ggplot(df) +
  geom_point(aes(hosp, p_obs)) +
  geom_hline(aes(yintercept = mean_pooled), linetype = 2, col = "blue") +
  ggtitle("Pooled model")


posterior <- as.data.frame(hosp_1)

plot_title <- ggtitle("Posterior distributions of p, pooled model", "with medians and 80% intervals")
p1 <- mcmc_areas(posterior, 
  pars = c("p"), 
  prob = 0.8) + plot_title
p1



# model_2: heterogeneous model --------------------------------------------

## Stan model
data_list <- list(
  N = N, 
  y = df$y, 
  n = df$n, 
  a = 1,
  b = 1
  )

hosp_2 <- stan("stan_models/6-2_hospital_2.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(hosp_2)

### Samples vs. iteration plot
traceplot(hosp_2)

p_post <- rstan::extract(hosp_2, "p")[[1]]
p_post_mean <- apply(p_post, 2, mean)

df <- df %>% 
	mutate(p_heter = p_post_mean)

df %>% 
  select(-c(y, n)) %>% 
  gather("distrib", "value", -hosp) %>% 
  ggplot() +
    geom_point(aes(hosp, value, colour = distrib)) +
    geom_hline(aes(yintercept = mean_pooled), linetype = 2, col = "blue") +
    ggtitle("Heterogeneous model")

posterior <- as.data.frame(hosp_2)

params <- paste0("p[", 1:12 ,"]")
plot_title <- ggtitle("Posterior distributions of p, heterogeneous model", "with medians and 80% intervals")
p_heter <- mcmc_areas(posterior, 
  pars = c(params), 
  prob = 0.8) + plot_title
p_heter

df$n




# model_3: hierarchical model ---------------------------------------------

## Stan model
data_list <- list(
  N = N, 
  y = df$y, 
  n = df$n, 
  a1 = 0.01,
  a2 = 0.01,
  b1 = 0.01,
  b2 = 0.01
  )

hosp_3 <- stan("stan_models/6-2_hospital_3.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(hosp_3)

### Samples vs. iteration plot
traceplot(hosp_3)

p_post3 <- rstan::extract(hosp_3, "p")[[1]]
p_post3_mean <- apply(p_post3, 2, mean)

df <- df %>% 
	mutate(p_hier = p_post3_mean)

df %>% 
  select(-c(y, n)) %>% 
  gather("distrib", "value", -hosp) %>% 
  ggplot() +
  geom_point(aes(hosp, value, colour = distrib)) +
  geom_hline(aes(yintercept = mean_pooled), linetype = 2, col = "blue") +
  ggtitle("Hierarchical model")



a_post <- rstan::extract(hosp_3, "a")[[1]]
b_post <- rstan::extract(hosp_3, "b")[[1]]
p_post_hier <- rbeta(length(a_post), a_post, b_post)

p_post_pool <- rstan::extract(hosp_1, "p")[[1]]


aux <- tibble(pooled = p_post_pool, hierarchical = p_post_hier)

aux %>% 
	gather(model, value)  %>%
  ggplot() +
    geom_density(aes(value, fill = model), alpha = 0.5) +
    ggtitle("p posterior comparison")


