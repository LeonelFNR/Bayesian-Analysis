---
title: "Bayesian Analysis Task III"
author: "Leonel Fernando Nabaza Ruibal"
date: "11/04/2026"
output:
  pdf_document:
    toc: true
    number_sections: true
geometry: margin=2.5cm
fontsize: 12pt
lang: en
header-includes:
  - '\usepackage{amsmath,amssymb,amsthm}'
  - '\usepackage{booktabs}'
  - '\usepackage{float}'
---



# Wording and data

We want to study whether the final grades in the *Bayesian Analysis* course are the same for boys and girls.

The observed data are:


``` r
boys  <- c(9.6, 7.0, 5.0, 8.0, 8.4, 6.4)
girls <- c(6.1, 9.1, 8.8, 5.7, 8.9, 6.1, 6.5)

df <- data.frame(
  grade = c(boys, girls),
  sex   = c(rep(1, length(boys)), rep(0, length(girls))),
  group = c(rep("boys", length(boys)), rep("girls", length(girls)))
)

knitr::kable(df, col.names = c("grade", "sex", "group"), booktabs = TRUE)
```



| grade| sex|group |
|-----:|---:|:-----|
|   9.6|   1|boys  |
|   7.0|   1|boys  |
|   5.0|   1|boys  |
|   8.0|   1|boys  |
|   8.4|   1|boys  |
|   6.4|   1|boys  |
|   6.1|   0|girls |
|   9.1|   0|girls |
|   8.8|   0|girls |
|   5.7|   0|girls |
|   8.9|   0|girls |
|   6.1|   0|girls |
|   6.5|   0|girls |

The statement proposes the model
\[
y_i \mid \beta_0, \beta_1, \sigma \sim \mathcal N(\beta_0 + \beta_1\, \text{sex}_i, \sigma),
\]
where \(\text{sex}_i = 1\) for boys and \(\text{sex}_i = 0\) for girls.

# Initial exploration


``` r
summary_by_group <- aggregate(grade ~ group, data = df, function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
summary_by_group
```

```
##   group  grade.n grade.mean grade.sd
## 1  boys 6.000000   7.400000 1.619877
## 2 girls 7.000000   7.314286 1.534523
```


``` r
library(ggplot2)

ggplot(df, aes(x = group, y = grade)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0) +
  labs(title = "Grades by group", x = "Group", y = "Grade")
```



\begin{center}\includegraphics{bayesian_grades_stan_corrected_files/figure-latex/data-boxplot-1} \end{center}

At first glance, the sample means are very similar, but the dispersion does not appear to be exactly the same in both groups. Moreover, grades are bounded between 0 and 10, so a pure normal model does not strictly respect the support of the variable.

# a) Full Bayesian model

## Model proposed in the statement

The linear model in the statement can be written as
\[
y_i \mid \beta_0, \beta_1, \sigma \overset{ind}{\sim} \mathcal N(\beta_0 + \beta_1 x_i, \sigma), \qquad x_i \in \{0,1\}.
\]

Here:

- \(\beta_0\) is the mean grade for girls.
- \(\beta_0 + \beta_1\) is the mean grade for boys.
- \(\beta_1\) is the difference in means, boys minus girls.
- \(\sigma\) is the common standard deviation for both groups.

A reasonable choice of weakly informative priors is
\[
\beta_0 \sim \mathcal N(7, 2^2), \qquad
\beta_1 \sim \mathcal N(0, 2^2), \qquad
\sigma \sim \text{Half-Normal}(0, 2).
\]

### Justification of the priors

- Grades are on a 0--10 scale, so centering \(\beta_0\) around 7 is plausible and still weakly informative.
- For \(\beta_1\), centering at 0 expresses that *a priori* we do not favor either group having better grades.
- The half-normal prior for \(\sigma\) guarantees positivity and avoids excessively heavy tails in such a small sample.

The corresponding posterior distribution is given by
\[
p(\beta_0, \beta_1, \sigma \mid y)
\propto
\left[\prod_{i=1}^n \mathcal{N}(y_i \mid \beta_0 + \beta_1 x_i, \sigma)\right]
p(\beta_0)\, p(\beta_1)\, p(\sigma).
\]

## Prior plots


``` r
par(mfrow = c(1, 3))
curve(dnorm(x, mean = 7, sd = 2), from = -1, to = 12,
      main = expression(paste("Prior for ", beta[0])), xlab = expression(beta[0]), ylab = "density")
curve(dnorm(x, mean = 0, sd = 2), from = -6, to = 6,
      main = expression(paste("Prior for ", beta[1])), xlab = expression(beta[1]), ylab = "density")
xs <- seq(0, 6, length.out = 500)
plot(xs, 2*dnorm(xs, mean = 0, sd = 2), type = "l",
     main = expression(paste("Prior for ", sigma)), xlab = expression(sigma), ylab = "density")
```



\begin{center}\includegraphics{bayesian_grades_stan_corrected_files/figure-latex/prior-plots-statement-model-1} \end{center}

``` r
par(mfrow = c(1, 1))
```

# b) Discussion of the model and alternative proposal

The model in the statement is useful as a starting point, but it has two important limitations:

1. **Common variance**. It imposes the same standard deviation for boys and girls. With so few observations, this restriction may be too strong.
2. **Unbounded support**. The normal distribution allows values below 0 and above 10, which does not perfectly match the nature of grades.

## Proposed alternative model

I propose using a model with **different means and different standard deviations** for each group:
\[
y_{g,j} \mid \mu_g, \sigma_g \sim \mathcal N(\mu_g, \sigma_g),
\qquad
y_{b,k} \mid \mu_b, \sigma_b \sim \mathcal N(\mu_b, \sigma_b).
\]

With weakly informative priors:
\[
\mu_g \sim \mathcal N(7, 2^2), \qquad
\mu_b \sim \mathcal N(7, 2^2), \qquad
\sigma_g, \sigma_b \sim \text{Half-Normal}(0, 2).
\]

This model remains simple, but it is more flexible than the original one. If we wanted to be even more realistic with the support \([0,10]\), we could use a truncated normal model or a beta model after rescaling the grades to \((0,1)\). However, for this sample size, the normal model with separate variances is a very reasonable and easy option to implement in Stan.

# Chosen model for Stan

## Stan code

Below we use the model with separate means and separate standard deviations. In addition, in `generated quantities` we request:

- the difference in means \(\delta = \mu_b - \mu_g\),
- a posterior predictive draw for a new girl, `y_new_girl`.


``` r
stan_code <- '
data {
  int<lower=1> N_boys;
  int<lower=1> N_girls;
  vector[N_boys] y_boys;
  vector[N_girls] y_girls;
}
parameters {
  real mu_boys;
  real mu_girls;
  real<lower=0> sigma_boys;
  real<lower=0> sigma_girls;
}
model {
  // priors
  mu_boys ~ normal(7, 2);
  mu_girls ~ normal(7, 2);
  sigma_boys ~ normal(0, 2);
  sigma_girls ~ normal(0, 2);

  // likelihood
  y_boys ~ normal(mu_boys, sigma_boys);
  y_girls ~ normal(mu_girls, sigma_girls);
}
generated quantities {
  real delta;
  real y_new_girl;
  delta = mu_boys - mu_girls;
  y_new_girl = normal_rng(mu_girls, sigma_girls);
}
'
writeLines(stan_code, "bayesian_grades_model.stan")
cat(stan_code)
```

```
## 
## data {
##   int<lower=1> N_boys;
##   int<lower=1> N_girls;
##   vector[N_boys] y_boys;
##   vector[N_girls] y_girls;
## }
## parameters {
##   real mu_boys;
##   real mu_girls;
##   real<lower=0> sigma_boys;
##   real<lower=0> sigma_girls;
## }
## model {
##   // priors
##   mu_boys ~ normal(7, 2);
##   mu_girls ~ normal(7, 2);
##   sigma_boys ~ normal(0, 2);
##   sigma_girls ~ normal(0, 2);
## 
##   // likelihood
##   y_boys ~ normal(mu_boys, sigma_boys);
##   y_girls ~ normal(mu_girls, sigma_girls);
## }
## generated quantities {
##   real delta;
##   real y_new_girl;
##   delta = mu_boys - mu_girls;
##   y_new_girl = normal_rng(mu_girls, sigma_girls);
## }
```

## Fitting with `rstan`


``` r
library(rstan)
library(bayesplot)
library(posterior)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_data <- list(
  N_boys  = length(boys),
  N_girls = length(girls),
  y_boys  = boys,
  y_girls = girls
)

fit <- stan(
  file = "bayesian_grades_model.stan",
  data = stan_data,
  chains = 4,
  iter = 400,
  warmup = 100,
  seed = 1234,
  refresh = 0
)

print(fit, pars = c("mu_boys", "mu_girls", "sigma_boys", "sigma_girls", "delta", "y_new_girl"),
      probs = c(0.025, 0.5, 0.975))
```

```
## Inference for Stan model: anon_model.
## 4 chains, each with iter=400; warmup=100; thin=1; 
## post-warmup draws per chain=300, total post-warmup draws=1200.
## 
##             mean se_mean   sd  2.5%  50% 97.5% n_eff Rhat
## mu_boys     7.34    0.03 0.69  5.99 7.36  8.61   424 1.01
## mu_girls    7.27    0.03 0.65  5.84 7.29  8.48   662 1.00
## sigma_boys  1.84    0.02 0.59  1.05 1.74  3.37  1044 1.00
## sigma_girls 1.71    0.01 0.49  1.03 1.60  2.93  1113 1.00
## delta       0.06    0.04 0.97 -1.78 0.07  1.96   473 1.00
## y_new_girl  7.33    0.06 1.86  3.43 7.37 11.18  1117 1.00
## 
## Samples were drawn using NUTS(diag_e) at Sat Apr 11 16:46:43 2026.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```

# c) 95% credible interval for a new female student

We seek the 95% credible interval for the grade of a **new girl** who did not take the exam. Under the chosen model, this corresponds to the posterior predictive distribution of `y_new_girl`.

## Approximate analytical summary

As a quick approximation, using only the girls' data:


``` r
n_g <- length(girls)
mean_g <- mean(girls)
sd_g <- sd(girls)

c(n_girls = n_g, mean_girls = mean_g, sd_girls = sd_g)
```

```
##    n_girls mean_girls   sd_girls 
##   7.000000   7.314286   1.534523
```

Under the normal model with the classical noninformative prior, the posterior predictive distribution for a new female observation is a Student \(t\):
\[
y_{\text{new},g} \mid y \sim t_{n_g-1}\left(\bar y_g,\; s_g\sqrt{1+\frac{1}{n_g}}\right).
\]


``` r
scale_pred <- sd_g * sqrt(1 + 1 / n_g)
ci_analytical <- mean_g + qt(c(0.025, 0.975), df = n_g - 1) * scale_pred
ci_analytical
```

```
## [1]  3.300189 11.328382
```

This interval is a useful reference, but the main answer to the exercise should be based on the Bayesian model fitted in Stan.

Note that the upper bound of the interval exceeds 10. This is a consequence of the normality assumption, since the normal distribution has unbounded support, while grades are restricted to the interval \([0,10]\).

## Interval from Stan

Once the model has been fitted, the interval is obtained directly from the posterior quantiles of `y_new_girl`:


``` r
post <- rstan::extract(fit)

ci_stan <- quantile(post$y_new_girl, probs = c(0.025, 0.5, 0.975))
ci_stan
```

And we can visualize the posterior predictive distribution:


``` r
hist(post$y_new_girl, breaks = 40,
     main = "Posterior predictive distribution for a new girl",
     xlab = "y_new_girl")
abline(v = ci_stan[c(1, 3)], lty = 2)
```

# Final interpretation

## On the equality between boys and girls

With these data, the observed difference between means is small. In the Bayesian model fitted with Stan, the key parameter is
\[
\delta = \mu_b - \mu_g.
\]

To assess whether grades are "equal", it is useful to examine:

- whether 0 lies inside the 95% credible interval for \(\delta\),
- how much posterior mass lies above or below 0,
- and whether a small difference would have practical relevance.

Useful code to summarize this is:


``` r
ci_delta <- quantile(post$delta, probs = c(0.025, 0.5, 0.975))
prob_boys_higher <- mean(post$delta > 0)

ci_delta
prob_boys_higher
```

From the Stan output, the 95% credible interval for \(\delta\) is approximately
\[
(-1.78,\; 1.96).
\]

Since this interval clearly contains 0, there is no strong evidence of a difference between the mean grades of boys and girls.

Moreover, the posterior mean of \(\delta\) is close to zero, indicating that any difference is small relative to the variability in the data.

If the interval for \(\delta\) contains 0 comfortably, the evidence in favor of differences between sexes will be weak.

## Answer

1. The model in the statement is
   \[
   y_i \sim \mathcal N(\beta_0 + \beta_1 x_i, \sigma),
   \]
   with weakly informative priors such as
   \(\beta_0 \sim \mathcal N(7,2^2)\),
   \(\beta_1 \sim \mathcal N(0,2^2)\),
   \(\sigma \sim \text{Half-Normal}(0,2)\).

2. That model is reasonable, but it can be improved by allowing different variances in each group. Therefore, we propose
   \[
   y_{b,k} \sim \mathcal N(\mu_b, \sigma_b), \qquad
   y_{g,j} \sim \mathcal N(\mu_g, \sigma_g),
   \]
   with weakly informative priors on \(\mu_b, \mu_g, \sigma_b, \sigma_g\).

3. Under this model, the 95% credible interval for the grade of a new female student is obtained from the posterior predictive variable `y_new_girl` generated in Stan:
   \[
   \bigl[q_{0.025}(y_{\text{new},g}),\; q_{0.975}(y_{\text{new},g})\bigr].
   \]
   As a quick analytical approximation using the girls' data only, the interval is:


``` r
knitr::kable(
  data.frame(
    method = "Analytical t approximation",
    lower = round(ci_analytical[1], 3),
    upper = round(ci_analytical[2], 3)
  ),
  booktabs = TRUE,
  col.names = c("Method", "Lower bound", "Upper bound")
)
```



|Method                     | Lower bound| Upper bound|
|:--------------------------|-----------:|-----------:|
|Analytical t approximation |         3.3|      11.328|

In conclusion, the data do not provide strong evidence that boys and girls have different grades. Any observed difference is small compared to the overall variability, and is not practically significant.
