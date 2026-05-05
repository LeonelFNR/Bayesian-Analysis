# Bayesian Analysis 2026 - Datathon ---------------------------------------

# Nombre1: Leonel Fernando Nabaza Ruibal
# Nombre2: Meritxell Torra Merin
# Nombre3: Anna Bagaria Rusiñol

load("data/datathon_2026.RData")

library(tidyverse)
library(rstan)
library(rstanarm)

eps <- 0.01

# ---------------------------
# Preprocesado
# ---------------------------

data_train <- data_train %>% ungroup()
data_test  <- data_test %>% ungroup()

data_train <- data_train %>%
  mutate(
    any_acces = as.factor(any_acces),
    sex = ifelse(sex == "H", 0, 1)
  )

data_test <- data_test %>%
  mutate(
    any_acces = as.factor(any_acces),
    sex = ifelse(sex == "H", 0, 1)
  )

# Respuesta
y_train <- log(data_train$nota_estad + eps)
y_test  <- data_test$nota_estad

# ---------------------------
# Grupos coherentes train/test
# ---------------------------

data_train$group_factor <- interaction(
  data_train$any_acces,
  data_train$sex,
  drop = TRUE
)

data_test$group_factor <- interaction(
  data_test$any_acces,
  data_test$sex,
  drop = TRUE
)

data_test$group_factor <- factor(
  data_test$group_factor,
  levels = levels(data_train$group_factor)
)

data_train$group <- as.numeric(data_train$group_factor)
data_test$group  <- as.numeric(data_test$group_factor)

# Comprobar que no hay grupos nuevos en test
if (any(is.na(data_test$group))) {
  stop("Hay grupos en data_test que no existen en data_train.")
}

group_train <- data_train$group
group_test  <- data_test$group

J <- length(levels(data_train$group_factor))

# Media de nota_access por grupo, ordenada por group
u <- data_train %>%
  group_by(group) %>%
  summarise(u = mean(nota_access, na.rm = TRUE), .groups = "drop") %>%
  arrange(group) %>%
  pull(u)

# ---------------------------
# Features
# ---------------------------

features <- c(
  "nota_access",
  "nota_inform",
  "nota_graf",
  "n_matr_por_asig"
)

X_train <- data_train %>%
  select(all_of(features)) %>%
  as.matrix()

X_test <- data_test %>%
  select(all_of(features)) %>%
  as.matrix()

# Comprobar que todo es numérico
storage.mode(X_train) <- "double"
storage.mode(X_test)  <- "double"

# ---------------------------
# Datos para Stan
# ---------------------------

N <- nrow(X_train)
K <- ncol(X_train)

data_list <- list(
  N = N,
  J = J,
  K = K,
  X = X_train,
  y = as.vector(y_train),
  group = group_train,
  u = as.vector(u)
)

# ---------------------------
# Ajustar modelo
# ---------------------------

fit <- stan(
  file = "stan_models/model.stan",
  data = data_list,
  iter = 500,
  chains = 4,
  seed = 123
)

# ---------------------------
# Predicción sobre train
# ---------------------------

post <- rstan::extract(fit)

n_sims <- nrow(post$alpha)
N_train <- nrow(X_train)

y_pred_log_sims <- matrix(NA, nrow = n_sims, ncol = N_train)

for (s in 1:n_sims) {
  mu <- post$alpha[s, group_train] + as.vector(X_train %*% post$beta[s, ])
  y_pred_log_sims[s, ] <- rnorm(N_train, mean = mu, sd = post$sigma[s])
}

# Media posterior en escala log
y_pred_log <- apply(y_pred_log_sims, 2, mean)

# Volver a escala original
y_pred <- exp(y_pred_log) - eps

# Evitar predicciones negativas
y_pred <- pmax(y_pred, 0)

# Guardar predicción en data_train
data_train$pred_nota_estad <- y_pred

# ---------------------------
# Evaluación del modelo
# ---------------------------

mape <- mean(abs((exp(y_train) - y_pred) / exp(y_train + eps))) * 100
print(mape)

tibble(
  real = exp(y_train),
  pred = y_pred
) %>%
  ggplot(aes(x = real, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Predicción vs Real")


# ---------------------------
# Predicción sobre test
# ---------------------------

post <- rstan::extract(fit)

n_sims <- nrow(post$alpha)
N_test <- nrow(X_test)

y_pred_log_sims <- matrix(NA, nrow = n_sims, ncol = N_test)

for (s in 1:n_sims) {
  mu <- post$alpha[s, group_test] + as.vector(X_test %*% post$beta[s, ])
  y_pred_log_sims[s, ] <- rnorm(N_test, mean = mu, sd = post$sigma[s])
}

# Media posterior en escala log
y_pred_log <- apply(y_pred_log_sims, 2, mean)

# Volver a escala original
y_pred <- exp(y_pred_log) - eps

# Evitar predicciones negativas
y_pred <- pmax(y_pred, 0)

#eliminar dos ultimas columnas
data_test <- data_test %>%
  select(-group_factor, -group)

# Guardar predicción en data_test
data_test$nota_estad <- y_pred


# ---------------------------
# Guardar resultado
# ---------------------------

save(data_test, file = "prediccion.RData")
