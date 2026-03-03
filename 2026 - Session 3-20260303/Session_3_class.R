# Session 3: Comparison of 2 treatments

install.packages("tidyr")   # solo la primera vez
library(tidyr)
install.packages("ggplot2")  # solo la primera vezplot")
library(ggplot2)
install.packages("dplyr")   # solo la primera vez
library(dplyr)
# Exercise 3.1: Burns

# a) Choose a priori distribution for every treatment according to the statement

#http://www.wolframalpha.com/
#solve(a/(a+b)=, sqrt((ab)/((a+b)^2*(a+b+1)))=)

## Prior distributions
delta_p <- 0.01
p <- seq(0, 1, delta_p)

# Conventional prior: 0.4 < p < 0.8
prior_par_c <- c(alpha = 159/5, beta = 106/5)
prior_c <- dbeta(p, prior_par_c[1], prior_par_c[2])

# Experimental prior: 0.6 < p < 0.9
prior_par_e <- c(alpha = 1977/64, beta = 659/64)
prior_e <- dbeta(p, prior_par_e[1], prior_par_e[2])
  
# Plot prior comparison
fortheplot <- tibble(p, prior_c, prior_e) %>%
  pivot_longer(cols = c(prior_c, prior_e), names_to = "treatment", values_to = "density")
ggplot(fortheplot, aes(p, density, color = treatment, linetype = treatment)) +
  geom_line() +  labs(x = "Probability of improvement", y = "Density") +
  scale_color_manual(values = c("blue", "red"), labels = c("Conventional", "Experimental")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Conventional", "Experimental")) +
  theme_minimal() +
  theme(legend.title = element_blank())
  
#delete df
rm(df)

# b) Draw the prior distribution, the posterior distribution and the likelihood
# function for every treatment in the same graph.

## Data
n <- 40
y_c <- 24
y_e <- 30


## Likelihood distributions
likelihood_c <- dbinom(y_c, n, p)
likelihood_e <- dbinom(y_e, n, p)
## Posterior distributions
posterior_c <- dbeta(p, prior_par_c[1] + y_c, prior_par_c[2] + n - y_c) # looking at wikipedia formula
posterior_e <- dbeta(p, prior_par_e[1] + y_e, prior_par_e[2] + n - y_e)
#Plots of each treatment together
std_dist <- function(x, delta) {
  x / (sum(x) * delta)
}
## Posterior distributions
# b.1) Grid solution:
# Standardize likelihoods
# Calculate products
# Posteriors: standardize products
df_b <- tibble(
  p = p,
  prior_c = prior_c,
  prior_e = prior_e,
  likelihood_c = std_dist(likelihood_c, delta_p),
  likelihood_e = std_dist(likelihood_e, delta_p)
) %>%
  mutate(
    product_c = prior_c * likelihood_c,
    product_e = prior_e * likelihood_e,
    posterior_c = std_dist(product_c, delta_p),
    posterior_e = std_dist(product_e, delta_p)
  )

# Plot
ggplot(df_b) +
  geom_line(aes(x = p, y = prior_c, linetype = "Conventional", color = "Prior")) +
  geom_line(aes(x = p, y = prior_e, linetype = "Experimental", color = "Prior")) +
  geom_line(aes(x = p, y = likelihood_c, linetype = "Conventional", color = "Likelihood")) +
  geom_line(aes(x = p, y = likelihood_e, linetype = "Experimental", color = "Likelihood")) +
  geom_line(aes(x = p, y = posterior_c, linetype = "Conventional", color = "Posterior")) +
  geom_line(aes(x = p, y = posterior_e, linetype = "Experimental", color = "Posterior")) +
  labs(title = "Prior, Likelihood and Posterior", x = "p", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_blank())
# > Exercise:
# Instead of using 2 aesthetics (linetype and color), 
# use facet_wrap to compare distributions.
df_long <- df_b %>%
  select(-c(product_c, product_e)) %>%
  gather(distribution, density,-p) %>%
  separate(distribution, into = c("distribution", "treatment"), sep = "_")
ggplot(df_long) +
  geom_line(aes(x = p, y = density, linetype = treatment, col = distribution))
ggplot(df_long) +
  geom_line(aes(x = p, y = density, col = distribution)) +
  facet_wrap(~ treatment, ncol = 1)

# Facet by distribution
ggplot(df_long) +
  geom_line(aes(x = p, y = density, col = treatment)) +
  facet_wrap(~ distribution, ncol = 1)


# c) Draw the posterior distribution of the difference between rates of
# improvement.
n_sim<-10000
df_b%>%
  select(p,posterior_c,posterior_e)

post_sim_c<-sample(p,size=n_sim,replace=TRUE,prob=df_b$posterior_c)
post_sim_e<-sample(p,size=n_sim,replace=TRUE,prob=df_b$posterior_e)
gamma<-post_sim_e-post_sim_c
#c.2)Simulatefromanalyticalsolution(conjugate)
n_sim<-10000
post_sim_c<-rbeta(n_sim,posterior_par_c[1],posterior_par_c[2])
post_sim_e<-rbeta(n_sim,posterior_par_e[1],posterior_par_e[2])
gamma_2<-post_sim_e-post_sim_c
#c.3)Plot
fig<-ggplot()+
  geom_density(data=tibble(gamma), aes(gamma))+
  geom_density(data=tibble(gamma_2),aes(gamma_2))
fig


# d) Compute the probability that the probability to improve using the
# experimental treatment is larger than using the conventional treatment.
fig +
  geom_vline(xintercept = 0, color = "red")

mean(gamma > 0)


# e) Compute and draw the posterior distribution for the Odds Ratio and give a
# 95% credible interval for it. Interpret the result.

# odds_ratio = p / (1-p)
odds_c <- post_sim_c / (1 - post_sim_c)
odds_e <- post_sim_e / (1 - post_sim_e)

# 95% credible interval
ci95_c <- quantile(odds_c, probs = c(0.025, 0.975))
ci95_e <- quantile(odds_e, probs = c(0.025, 0.975))

# Plot
odds <- tibble(odds_c, odds_e) %>%
  gather(treatment, odds)

ggplot(odds) +
  geom_density(aes(odds, col = treatment))



# > Optional exercise:
#
# Redo the calculations using a normal priori
