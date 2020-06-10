# Loading necessary packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xgboost, data.table, ggpubr)

# Installing the causally enhanced shapr package from source
install.packages("shapr", clean = TRUE, repos = NULL, type = "source")
