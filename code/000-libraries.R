#
#   0. Libraries and Setup
#
#   Load all packages and set various options. If necessary, install
#   all required packages.
#
# *****************************************************************************

# Init
rm(list = ls())
gc(reset = TRUE)

# Set workspace options
options(scipen = 99999)

# Load `pacman` for pkg management
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

# Packages to use
pkgs <- c("tidyverse",    # General data manipulation
          "dplyr",        # General data manipulation
          "dbplyr",       # Interface for databases
          "haven",        # Load Stata datasets
          "RSQLite",      # SQL
          "survey",       # Survey data analysis
          "srvyr")        # Survey data analysis

# Install packages as needed
if(!sum(!p_isinstalled(pkgs)) == 0){
  p_install(package = pkgs[!p_isinstalled(pkgs)], character.only = TRUE)
}

# Load the packages
p_load(pkgs, character.only = TRUE)
rm(pkgs)

### EOF ###
