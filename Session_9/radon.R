# Ref: https://jrnold.github.io/bayesian_notes/multilevel-models.html

library(rstan) 
library(rstanarm) 
library(tidyverse) 
library(broom)

options(mc.cores = parallel::detectCores())

# Data
data("radon", package = "rstanarm")
glimpse(radon)

# N          : number of observations
# J          : number of counties
# y          : logarithm of the radon measurement in house i, i = 1,...,N
# x          : the floor of the measurement
#             0: basement
#             1: first floor
# county     : county id, 1,...,J
# county_name: county name
# u          : measurement of soil uranium in county j, j = 1,...,J

N <- nrow(radon)
county <- as.numeric(radon$county)
county_name <- unique(radon$county)
J <- length(unique(county))
y <- radon$log_radon
x <- radon$floor
u <- radon %>% 
	group_by(county) %>% 
	summarise(u = first(log_uranium)) %>% 
	pull(u)

# Data visualization
radon_county <- radon %>%
  group_by(county) %>%
  summarise(log_radon_mean = mean(log_radon),
            log_radon_sd = sd(log_radon),
            log_uranium = mean(log_uranium),
            n = length(county)) %>%
  mutate(log_radon_se = log_radon_sd / sqrt(n))

ggplot(radon) +
  geom_boxplot(aes(y = log_radon,
                   x = fct_reorder(county, log_radon, mean)),
               colour = "gray") +
  geom_point(aes(y = log_radon,
                 x = fct_reorder(county, log_radon, mean)),
             colour = "gray") +
  geom_point(data = radon_county,
             aes(x = fct_reorder(county, log_radon_mean),
                 y = log_radon_mean),
             colour = "black") +
  coord_flip() +
  labs(y = "log(radon)", x = "")



# radon_1: pooled model ---------------------------------------------------

data_list <- list(
  N = N,
  y = y,
  x = x
)

radon_1 <- stan("stan_models/radon_1_pooled.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(radon_1)
plot(radon_1)

radon_summary <- tidy(radon_1, conf.int = T, level = 0.8, rhat = T, ess = T)

df_pooled  <- tibble(
  county = county_name,
  model = "pooled",
  intercept = radon_summary$estimate[1],
  slope = radon_summary$estimate[2]
)

id_county <- c("LACQUIPARLE", 
"AITKIN", "KOOCHICHING", "DOUGLAS", "CLAY", "STEARNS", "RAMSEY", 
"SIBLEY")


df_model <- df_pooled %>% 
  left_join(radon, by = "county") %>%
 	filter(county %in% id_county) 

ggplot(df_model) +
  geom_point(aes(floor, log_radon)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ county, ncol = 4) + 
  theme(legend.position = "bottom")
  


# radon_2_no pooled intercept --------------------------------------

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  county = county
)

radon_2 <- stan("stan_models/radon_2_no_pooled_a.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(radon_2)
plot(radon_2)

radon_summary <- tidy(radon_2, conf.int = T, level = 0.8, rhat = T, ess = T)

df_no_pooled  <- tibble(
  county = county_name,
  model = "no_pooled",
  intercept = radon_summary$estimate[1:J],
  slope = radon_summary$estimate[J+1]
)

df_model <- bind_rows(df_pooled, df_no_pooled) %>% 
  left_join(radon, by = "county") %>%
 	filter(county %in% id_county) 

ggplot(df_model) +
  geom_jitter(aes(floor, log_radon)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ county, ncol = 4) + 
  scale_x_continuous(breaks = 0:1) + 
  theme(legend.position = "bottom")


# radon_3_multilevel ----------------------------------------

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  county = county
)

radon_3 <- stan("stan_models/radon_3_multilevel.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(radon_3)
plot(radon_3)

radon_summary <- tidy(radon_3, conf.int = T, level = 0.8, rhat = T, ess = T)

df_multilevel  <- tibble(
  county = county_name,
  model = "multilevel",
  intercept = radon_summary$estimate[1:J],
  slope = radon_summary$estimate[J+1]
)

df_model <- bind_rows(df_pooled, df_no_pooled, df_multilevel) %>% 
  left_join(radon, by = "county") %>%
 	filter(county %in% id_county) 

ggplot(df_model) +
  geom_jitter(aes(floor, log_radon)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ county, ncol = 4) + 
  scale_x_continuous(breaks = 0:1) + 
  theme(legend.position = "bottom")



# radon_4_multilevel + covariable u ----------------------------------

data_list <- list(
  N = N,
  J = J,
  y = y,
  x = x,
  county = county,
  u
)

radon_4 <- stan("stan_models/radon_4_multilevel_covariable.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)

## Convergence analysis
print(radon_4)
plot(radon_4)

radon_summary <- tidy(radon_4, conf.int = T, level = 0.8, rhat = T, ess = T)

df_multilevel_cov  <- tibble(
  county = county_name,
  model = "multilevel_cov",
  intercept = radon_summary$estimate[1:J],
  slope = radon_summary$estimate[J+1]
)

df_model <- bind_rows(df_pooled, df_multilevel, df_multilevel_cov) %>% 
  left_join(radon, by = "county") %>%
 	filter(county %in% id_county) 

ggplot(df_model) +
  geom_jitter(aes(floor, log_radon)) +
  geom_abline(aes(intercept = intercept, slope = slope, color = model)) +
  facet_wrap(~ county, ncol = 4) + 
  scale_x_continuous(breaks = 0:1) + 
  theme(legend.position = "bottom")



# Prediction --------------------------------------------------------------

## Predictions using R, for county 26 and floor = 1

sims <- rstan::extract(radon_4)
# new unit in an existing group 
a <- sims$a
b <- sims$b
sigma.y <- sims$sigma_y
n.sims <- dim(a)[1]
y.tilde <- rnorm(n.sims, a[,26] + b * 1, sigma.y)
# new unit in a new group
g.0 <- sims$g_0
g.1 <- sims$g_1
u.tilde <- mean(u)
sigma.a <- sims$sigma_a
a.tilde <- rnorm(n.sims, g.0 + g.1 * u.tilde, sigma.a)
y.tilde <- rnorm(n.sims, a.tilde + b * 1, sigma.y)


## Prediction using Stan's generated quantities block

# Predicting a new unit in an existing group using Stan

pred_1 <- stan("stan_models/radon_predict_1.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)
print(pred_1)

y.tilde <- extract(pred_1, "y_tilde")$y_tilde
quantile(exp(y.tilde), c(.25, .75))

# Predicting a new unit in a new group using Stan

pred_2 <- stan("stan_models/radon_predict_2.stan", iter = 1000, chains = 4,
  data = data_list, seed = 1)
print(pred_2)

y.tilde <- extract(pred_2, "y_tilde")$y_tilde
quantile(exp(y.tilde), c(.25, .75))



