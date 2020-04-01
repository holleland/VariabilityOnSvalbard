# For cleaning environment: 
rm(list=ls())
# If you opened R in the R-folder, set the wd to root: 
if("1_model_fitting.R" %in% list.files())
  setwd("../")

# Fitting the models
source("R/1_model_fitting.R")

# All the following scripts depend on 1_model_fitting.R

# making figures 
source("R/2_making_figures.R")

# Set to RUN_BOOT to TRUE to run new bootstraps (will overwrite pre-run 
#   and take about 1 hr with 7 cores)
# Set to FALSE to use the pre-run results
RUN_BOOT <- FALSE
source("R/3_bootstrap.R")

# To make the gif-animations, additional software is necessary. 
source("R/4_make_animation_pictures.R")
