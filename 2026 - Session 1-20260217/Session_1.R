# Name : Leonel Fernando Nabaza Ruibal
## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

# 1. Statistical model

# Plot binomial distribution
df <- tibble(x = 0:20)
ggplot(df, aes(x))+
  stat_function(fun = dbinom,
                args = list(size = 20, prob = 0.5),
                geom = "bar")
  


# Exercise 1.5
#Choose a Bayesian model for the number of goals scored in a match in the first division of the Spanish soccer league, and argue in favor of it.
# use and plot poisson distribution
#plot poisson distribution
df <- tibble(x = 0:10)
ggplot(df, aes(x))+
  stat_function(fun = dpois,
                args = list(lambda = 2),
                geom = "bar")



# 3. Bayesian model

# Exercise coin
ggplot() +
  stat_function(fun = dbeta, 
                args = list(shape1 = 100, shape2 = 100)) +
  xlim(c(0, 1))



# Exercise world
ggplot() +
  stat_function(fun = dbeta, 
                args = list(shape1 = 190, shape2 = 75)) +
  xlim(c(0, 1))

## ------------------------------------------------------------------------

# 4. Likelihood function

# Exercise likelihood:

df <- tibble(
  omega = seq(0, 1, 0.01), 
  likelihood = dbinom(12, 20, omega)) 

ggplot(df) +
  geom_line(aes(omega, likelihood))



## ------------------------------------------------------------------------
## Predictive simulation

# Simulate 10000 draws from prior density: p_sim as Beta (a = 200, b = 200)
# random vector where each compontnent is a random draw from beta(200,200)
p_sim <- rbeta(10000, 200, 200)

# plot p_sim via geom density
ggplot(tibble(p_sim), aes(x = p_sim)) +
  geom_density() +
  xlab("Probability of tails in a coin flip") +
  ylab("Density")

# Simulate 10000 draws from the predictive density: y_sim
y_sim <- rbinom(10000, 20, p_sim)

# Plot the prior predictive density
ggplot(tibble(y_sim), aes(x = y_sim)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab("Number of tails in 20 flips") +
  ylab("Frequency")

# Compute the probability of less than 5 tails
mean(y_sim < 5)

# Find a 90 percent prediction interval for the number of tails
quantile(y_sim, c(0.05,0.95))



# Exercise 1.4

# a) Choose the parameters of a conjugate prior distribution (gamma), 
# and explain why you choose them (it might be useful to draw the 
# prior predictive distribution to back your choice up).



# b) Draw in the same graph the prior distribution and the 
# likelihood function.



# c) Draw the prior predictive distribution.



# Now, assume that the members of the association know nothing 
# about the number of weekly visitors:
# d) Choose the parameters of a conjugate prior distribution in 
# that case.

