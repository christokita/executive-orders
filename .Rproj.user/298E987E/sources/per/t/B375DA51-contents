##################################################################################################
#
# Utility Master Script
#
##################################################################################################
# This script sources all other utility funciton scripts and loads the relevant packages

####################
# Install missing packages function
####################
install_missing_packages <- function(list_of_packages) {
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repo = 'https://cloud.r-project.org/')
  } 
}

####################
# Load packages
####################
required_packages <- c("dplyr", "stringr", "tidyr", "tidytext", "textmineR", "tm")
install_missing_packages(required_packages)
lapply(required_packages, require, character.only = T)


####################
# Source other function scripts
####################