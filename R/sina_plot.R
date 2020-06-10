#' Make a sina plot of the Shapley values computed using shapr.
#'
#' @param explanation shapr list containing an explanation produced by shapr::explain.
#'
#' @return ggplot2 object containing the sina plot.
#' @export
#'
#' @examples
#' # set parameters and random seed
#' set.seed(2020)
#' N <- 1000
#' m <- 4
#' sds <- runif(4, 0.5, 1.5)
#' pars <- runif(7, -1, 1)
#' 
#' # Create data from a structural equation model
#' X_1 <- rnorm(N, sd = sds[1])
#' Z <- rnorm(N, 1)
#' X_2 <- X_1 * pars[1] + Z * pars[2] + rnorm(N, sd = sds[2])
#' X_3 <- X_1 * pars[3] + Z * pars[4] + rnorm(N, sd = sds[3])
#' Y <- X_1 * pars[5] + X_2 * pars[6] + X_3 * pars[7] + rnorm(N, sd = sds[4])
#'
#' # collecting data
#' mu_A <- rep(0, m)
#' X_A <- cbind(X_1, X_2, X_3)
#' dat_A <- cbind(X_A, Y)
#' cov_A <- cov(dat_A)
#' 
#' model <- lm(Y ~ . + 0 , data = as.data.frame(dat_A))
#' explainer <- shapr::shapr(X_A, model)
#' y_mean <- mean(Y)
#' 
#' explanation_classic <- shapr::explain(dat_A, approach = "gaussian", explainer = explainer, prediction_zero = y_mean)
#' sina_plot(explanation_classic)
#' 
#' explanation_causal <- shapr::explain(dat_A, approach = "causal", explainer = explainer, prediction_zero = y_mean, ordering = list(1, c(2, 3)))
#' sina_plot(explanation_causal)
#' 
#' @seealso \link[SHAPforxgboost]{shap.plot.summary}
#' 
#' @details Function adapted from \link[SHAPforxgboost]{shap.plot.summary}. 
#' Copyright © 2020 - Yang Liu & Allan Just
#' 
sina_plot <- function(explanation) {
  
  library(tidyverse)
  library(ggplot2)
  library(data.table)
  
  shapley_values <- explanation$dt[, -1, drop = FALSE]
  X_values <- explanation$x_test
  
  data_long <- explanation$x_test %>%
    as.data.table() %>%
    pivot_longer(everything()) %>%
    bind_cols(explanation$dt %>%
                select(-none) %>%
                pivot_longer(everything()) %>%
                select(-name) %>%
                rename(shap = value)) %>%
    mutate(name = factor(name, levels = rev(names(explanation$dt)))) %>%
    group_by(name) %>%
    arrange(name) %>%
    mutate(mean_value = mean(value)) %>%
    mutate(std_value = (value - min(value)) / (max(value) - min(value)))
  
  x_bound <- max(abs(max(data_long$shap)), abs(min(data_long$shap)))
  
  ggplot(data = data_long) + 
    coord_flip(ylim = c(-x_bound, x_bound)) + 
    geom_hline(yintercept = 0) + 
    ggforce::geom_sina(
      aes(x = name, y = shap, color = std_value), 
      method = "counts", maxwidth = 0.7, alpha = 0.7
    ) +
    theme_minimal() + theme(
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "top", 
      legend.title = element_text(size = 16), legend.text = element_text(size = 14), 
      axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, vjust = -1), axis.text.x = element_text(size = 14)
    ) +
    scale_color_gradient(
      low = "dark green"  , high = "sandybrown" , 
      breaks = c(0, 1), labels = c(" Low", "High "), 
      guide = guide_colorbar(barwidth = 12, barheight = 0.3)
    ) +
    labs(y = "Causal Shapley value (impact on model output)", 
         x = "", color = "Scaled feature value  ")
}
