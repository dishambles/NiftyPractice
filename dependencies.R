# dependencies.R

# Function to check and install missing packages
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c("shiny", "quantmod", "DT")

# Install and load required packages
check_and_install(required_packages)
