library(SHAPforxgboost)
library(data.table)
library(shapr)


# Model A -----------------------------------------------------------------

set.seed(2020)
N <- 100
m <- 4
sds <- runif(4, 0.5, 1.5)
pars <- runif(7, -1, 1)
mu_A <- rep(0, m)
X_1 <- rnorm(N, sd = sds[1])
Z <- rnorm(N, 1)
X_2 <- X_1 * pars[1] + Z * pars[2] + rnorm(N, sd = sds[2])
X_3 <- X_1 * pars[3] + Z * pars[4] + rnorm(N, sd = sds[3])
Y <- X_1 * pars[5] + X_2 * pars[6] + X_3 * pars[7] + rnorm(N, sd = sds[4])

X_A <- cbind(X_1, X_2, X_3)

dat_A <- cbind(X_A, Y)
cov_A <- cov(dat_A)

dat_test <- as.matrix(MASS::mvrnorm(10, mu_A, cov_A))
cnms <- colnames(cov_A)
index_given <- c()
ordering <- list(1, c(2, 3), 4)
confounding <- TRUE

shap_obs <- sample_causal(1, N, mu_A, cov_A, m, dat_test, ordering, confounding)

model <- lm(Y ~ . + 0 , data = as.data.frame(dat_A))
explainer <- shapr(X_A, model)

p <- mean(Y)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  dat_A,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

sina_plot(explanation)

explanation_c1 <- explain(
  dat_A,
  approach = "causal",
  explainer = explainer,
  prediction_zero = p,
  confounding = FALSE
)
sina_plot(explanation_c1)

explanation_c2 <- explain(
  dat_A,
  approach = "causal",
  explainer = explainer,
  prediction_zero = p,
  confounding = TRUE
)

sina_plot(explanation_c2)

# Trying to rebuilt the sampling ------------------------------------------



m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
index_given <- c(4, 7)
ordering <- list(c(1,2,3),c(4,5,6),c(7,8,9,10))
confounding <- FALSE


mod_A = xgboost::xgboost(
  data = X_A, label = Y, gamma = 0, eta = 1,
  lambda = 0, nrounds = 200, verbose = TRUE)
pred_A <- predict(mod_A, X_A)

# shap.values(model, X_dataset) returns the SHAP
# data matrix and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_A, X_train = X_A)
shap_values$mean_shap_score
# shap_values_iris <- shap_values$shap_score

#shap_long <- shap.prep(xgb_model = mod_A, X_train = X_A)
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_A)

shap.plot.summary(shap_long, scientific = FALSE)

# 5 is just 2^2 + 1, we get all conditioning sets
C_cond <- matrix(c(
  6, 0, 0,
  -2, -2, -2,
  -1, 2, 1, 
  -1, -1, 2,
  -2, 1, 1,
  0, 6, 0, 
  -2, -2, -2,
  2, -1, -1,
  -1, -1, 2,
  1, -2, 1,
  0, 0, 6,
  -2, -2, -2,
  2, -1, -1,
  -1, 2, -1,
  1, 1, -2
),  m-1, (m-1) * (2^(m-2) + 1)) / 6

predict(mod_A, )


