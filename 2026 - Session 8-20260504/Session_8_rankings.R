# Session 9

library(rstan)
library(ggplot2)
library(bayesplot)
library(tidyverse)

theme_set(theme_bw())


# Hospitales - ranking ----------------------------------------------------

# Load the posterior simulations from Stan
load("hospital_posterior.RData")
hosp_post

probs <- hosp_post[, 1:12]

# Calculate the rank for every simulation (row)
ranks <- as.data.frame(t(apply(probs, 1, rank)))
names(ranks) <- LETTERS[1:12]

# Plot
ranks %>% 
  gather("Hospital", "Rank") %>% 
  ggplot() +
    geom_bar(aes(Rank)) +
    facet_wrap(vars(Hospital), nrow = 3)

# Plot 2: frequencies
p <- ranks %>% 
  gather("Hospital", "Rank") %>% 
  ggplot() +
  geom_bar(aes(Rank, y = (..count..)/sum(..count..))) +
  facet_wrap(vars(Hospital), nrow = 3)
p

# Plot 3: Percent
library(scales)
p + scale_y_continuous(name = "Prob", labels = percent)



