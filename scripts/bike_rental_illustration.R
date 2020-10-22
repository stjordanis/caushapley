# Set to true in order to save plots in the main folder
save_plots <- TRUE

# 0 - Load Packages and Source Files --------------------------------------

library(tidyverse)
library(data.table)
library(xgboost)
library(ggpubr)
library(shapr) # NOTE: must be installed by running build.R in the root directory

# For sina plotting capabilities
source("R/sina_plot.R")

if (save_plots && !dir.exists("figures")) {
  dir.create("figures")
}

# 1 - Prepare and Plot Data -----------------------------------------------

# Data source: https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset
 
bike <- read.csv("inst/extdata/day.csv")
# Difference in days, which takes DST into account
bike$trend <- as.numeric(difftime(bike$dteday, bike$dteday[1], units = "days"))
# bike$trend <- as.integer(difftime(bike$dteday, min(as.Date(bike$dteday)))+1)/24
bike$cosyear <- cospi(bike$trend/365*2)
bike$sinyear <- sinpi(bike$trend/365*2)
# Unnormalize variables (see data set information in link above)
bike$temp <- bike$temp * (39 - (-8)) + (-8)
bike$atemp <- bike$atemp * (50 - (-16)) + (-16)
bike$windspeed <- 67 * bike$windspeed
bike$hum <- 100 * bike$hum

bike_plot <- ggplot(bike, aes(x = trend, y = cnt, color = temp)) + 
  geom_point(size = 0.75) + scale_color_gradient(low = "blue", high = "red") + 
  labs(colour = "temp") + 
  xlab( "Days since 1 January 2011") + ylab("Number of bikes rented") +
  theme_minimal() + 
  theme(legend.position = "right", legend.title = element_text(size = 10))

if (save_plots) {
  ggsave("figures/bike_rental_plot.pdf", bike_plot, width = 4.5, height = 2)
} else {
  print(bike_plot)
}

x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# NOTE: Encountered RNG reproducibility issues across different systems, 
# so we saved the training-test split.
# set.seed(2013)
# train_index <- caret::createDataPartition(bike$cnt, p = .8, list = FALSE, times = 1)
train_index <- readRDS("inst/extdata/train_index.rds")

# Training data
x_train <- as.matrix(bike[train_index, x_var])
y_train_nc <- as.matrix(bike[train_index, y_var]) # not centered
y_train <- y_train_nc - mean(y_train_nc) 

# Test data
x_test <- as.matrix(bike[-train_index, x_var])
y_test_nc <- as.matrix(bike[-train_index, y_var]) # not centered
y_test <- y_test_nc - mean(y_train_nc) 

# Fit an XGBoost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 100,
  verbose = FALSE
)
# caret::RMSE(y_test, predict(model, x_test))

message("1. Prepared and plotted data, trained XGBoost model")

# 2 - Compute Shapley Values ----------------------------------------------

explainer_symmetric <- shapr(x_train, model)                    
p <- mean(y_train)

# a. We compute the causal Shapley values on a given partial order (see paper)
partial_order <- list(1, c(2, 3), c(4:7))

explanation_causal <- explain(
  x_test,
  approach = "causal",
  explainer = explainer_symmetric,
  prediction_zero = p,
  ordering = partial_order,
  confounding = c(FALSE, TRUE, FALSE),
  seed = 2020
)

sina_causal <- sina_plot(explanation_causal)
# save limits of sina_causal plot for comparing against marginal and asymmetric
ylim_causal <- sina_causal$coordinates$limits$y

if (save_plots) {
  ggsave("figures/sina_plot_causal.pdf", sina_causal, height = 6.5, width = 6.5)
} else {
  print(sina_causal)
}

message("2a. Computed and plotted causal Shapley values")


# b. For computing marginal Shapley values, we assume one component with confounding
explanation_marginal <- explain(
  x_test,
  approach = "causal",
  explainer = explainer_symmetric,
  prediction_zero = p,
  ordering = list(c(1:7)),
  confounding = TRUE,
  seed = 2020
)

sina_marginal <- sina_plot(explanation_marginal) +
  coord_flip(ylim = ylim_causal) + ylab("Marginal Shapley value (impact on model output)")

if (save_plots) {
  ggsave("figures/sina_plot_marginal.pdf", sina_marginal, height = 6.5, width = 6.5)
} else {
  print(sina_marginal)
}

message("2b. Computed and plotted marginal Shapley values")


# c. Finally, we compute the asymmetric Shapley values for the same partial order
explainer_asymmetric <- shapr(x_train, model, asymmetric = TRUE, ordering = partial_order)
p <- mean(y_train)

explanation_asymmetric <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer_asymmetric,
  prediction_zero = p,
  ordering = partial_order,
  asymmetric = TRUE,
  seed = 2020
)

sina_asymmetric <- sina_plot(explanation_asymmetric) +
  coord_flip(ylim = ylim_causal) + ylab("Asymmetric conditional Shapley value (impact on model output)")

if (save_plots) {
  ggsave("figures/sina_plot_asymmetric.pdf", sina_asymmetric, height = 6.5, width = 6.5)
} else {
  print(sina_asymmetric)
}

message("2c. Computed and plotted asymmetric conditional Shapley values")

# d. Asymmetric causal Shapley values (very similar to the conditional ones)

explanation_asymmetric_causal <- explain(
  x_test,
  approach = "causal",
  explainer = explainer_asymmetric,
  prediction_zero = p,
  asymmetric = TRUE,
  ordering = partial_order,
  confounding = c(FALSE, TRUE, FALSE),
  seed = 2020
)

sina_asymmetric_causal <- sina_plot(explanation_asymmetric_causal) +
  coord_flip(ylim = ylim_causal) + ylab("Asymmetric causal Shapley value (impact on model output)")

if (save_plots) {
  ggsave("figures/sina_plot_asymmetric_causal.pdf", sina_asymmetric_causal, height = 6.5, width = 6.5)
} else {
  print(sina_asymmetric_causal)
}

message("2d. Computed and plotted asymmetric conditional Shapley values")


# 3 - Shapley value scatter plots (Figure 3) ------------------------------

sv_correlation_df <- data.frame(
  valtemp = x_test[, "temp"],
  sv_marg_cosyear = explanation_marginal$dt$cosyear,
  sv_caus_cosyear = explanation_causal$dt$cosyear,
  sv_marg_temp = explanation_marginal$dt$temp,
  sv_caus_temp = explanation_causal$dt$temp
)

scatterplot_topleft <- 
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_marg_cosyear, color = valtemp)) + 
  geom_point(size = 1)+xlab("MSV temp")+ylab( "MSV cosyear")+
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500))  + 
  scale_color_gradient(low="blue", high="red") +
  theme_minimal() + 
  theme(text = element_text(size = 12), 
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(), axis.title.x = element_blank())

scatterplot_topright <- 
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_marg_cosyear, color = valtemp)) + 
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") +
  xlab("CSV cosyear") + ylab("MSV cosyear") + 
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-500, 500), breaks = c(-500, 0, 500)) + 
  theme_minimal() +
  theme(text = element_text(size=12), axis.title.x = element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

scatterplot_bottomleft <- 
  ggplot(sv_correlation_df, aes(x = sv_marg_temp, y = sv_caus_temp, color = valtemp)) +
  geom_point(size = 1) + scale_color_gradient(low="blue", high="red") + 
  ylab( "CSV temp") + xlab("MSV temp") +  
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  + 
  theme_minimal() +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

scatterplot_bottomright <- 
  ggplot(sv_correlation_df, aes(x = sv_caus_cosyear, y = sv_caus_temp, color = valtemp)) +
  geom_point(size = 1) + ylab("CSV temp") + xlab( "CSV cosyear") + 
  scale_x_continuous(limits = c(-1500, 1000), breaks = c(-1000, 0, 1000)) +
  scale_y_continuous(limits = c(-1000, 1000), breaks = c(-500, 0, 500))  + 
  scale_color_gradient(low="blue", high="red")+
  theme_minimal() +
  theme(text = element_text(size=12), axis.text.x=element_text(size=12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

grid_top <- ggarrange(scatterplot_topleft, scatterplot_topright, legend = "none")
grid_bottom <- ggarrange(scatterplot_bottomleft, scatterplot_bottomright, legend = "none")

if (save_plots) {
  ggsave("figures/scatter_plots_top.pdf", grid_top, width = 5, height = 1)
  ggsave("figures/scatter_plots_bottom.pdf", grid_bottom, width = 5, height = 2)
} else {
  print(ggarrange(grid_top, grid_bottom, nrow = 2))
}

message("3. Produced scatter plots comparing marginal and causal Shapley values on the test set")


# 4 - Shapley value bar plots (Figure 4) ----------------------------------

# Get test set index for two data points with similar temperature
# 1. 2012-10-09 (October)
# 2. 2012-12-03 (December)

october <- which(as.integer(row.names(x_test)) == which(bike$dteday == "2012-10-09"))
december <- which(as.integer(row.names(x_test)) == which(bike$dteday == "2012-12-03"))

# predicted values for the two points
# predict(model, x_test)[c(october, december)] + mean(y_train_nc)

dt_marginal <- explanation_marginal$dt %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Marginal')

dt_causal <- explanation_causal$dt %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Causal')

dt_asymmetric <- explanation_asymmetric$dt %>%
  dplyr::slice(c(october, december)) %>%
  select(cosyear, temp) %>%
  mutate(date = c("2012-10-09", "2012-12-03"), type = 'Asymmetric')

dt_all <- dt_marginal %>% pivot_longer(c(cosyear, temp)) %>%
  rbind(dt_causal %>% pivot_longer(c(cosyear, temp))) %>%
  rbind(dt_asymmetric %>% pivot_longer(c(cosyear, temp)))

bar_plots <- ggplot(dt_all, aes(x = name, y = value, group = interaction(date, name), 
                         fill = date, label = round(value, 2))) +
  geom_col(position = "dodge") +
  theme_classic() + ylab("Shapley value") +
  facet_wrap(vars(type)) + theme(axis.title.x = element_blank()) +
  scale_fill_manual(values = c('indianred4', 'ivory4')) + 
  theme(legend.position = c(0.75, 0.25), axis.title = element_text(size = 20),
        legend.title = element_text(size = 16), legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14))


if (save_plots) {
  ggsave("figures/bar_plots.pdf", bar_plots, width = 6, height = 3)
} else {
  print(bar_plots)
}

message("4. Produced bar plots comparing marginal, causal, and asymmetric conditional Shapley values")
