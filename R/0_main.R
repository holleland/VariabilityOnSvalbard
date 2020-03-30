setwd("../")
# Fitting the models
source("R/1_model_fitting.R")
# making figures
source("R/2_making_figures.R")
# Set to TRUE to run new bootstraps (will overwrite pre-run)
# Set to FALSE to use the pre-run results
RUN_BOOT <- FALSE 
source("R/3_bootstrap.R")

# Make figure of yearly mean temperatures from 1899 to 2019: 
source("R/4_yearly_mean_from_1899_to_2019.R")

# Need to be run after 1 and 2.
# To make the gif-animations, additional software is necessary. 
source("R/5_make_animation_pictures.R")
