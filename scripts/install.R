# Load necessary packages and install them if needed
load_packages <- function(names) {
  for (name in names) {
    if (!require(name, character.only = TRUE)) {
      install.packages(name, repos = "https://cran.r-project.org")
      library(name, character.only = TRUE)
    }
  }
}

package_list <- c("devtools", "tidyverse", "xgboost", "data.table", "ggpubr")
load_packages(package_list)

# Installing the causally enhanced shapr package from source
devtools::install_local("shapr", dependencies = TRUE)
