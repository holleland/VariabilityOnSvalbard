# For cleaning environment: 
rm(list=ls())

list.of.packages <- c("TMB", "ggplot2", "zoo", "lubridate", "reshape2",
                      "forecast", "xtable", "doParallel", "mapsdata",
                      "plyr", "dplyr", "ggpubr", "fGarch")
# The following code will install the missing packages: 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

# If you opened R in the R-folder, set the wd to root: 
if(!("readme.me" %in% list.files()))
  setwd("../")

# Fitting the models
source("R/1_model_fitting.R")

# All the following scripts depend on 1_model_fitting.R
# Making the figures 
source("R/2_making_figures.R")

# Set to RUN_BOOT to TRUE to run new bootstraps 
# (will overwrite pre-run and take about 1 hr with 7 cores)
# Set to FALSE to use the pre-run results
RUN_BOOT <- FALSE
set.seed(1) # seed used in the paper version (for RUN_BOOT = TRUE)
source("R/3_bootstrap.R")


# Set to RUN_MC to TRUE to run new bootstraps 
# (will overwrite pre-run and take about 1 hr with 7 cores)
# Set to FALSE to use the pre-run results
RUN_MC <- FALSE
set.seed(1) # seed used in the paper version (for RUN_MC = TRUE)
source("R/4_MC_with_fig11.R")

# To create the png files used to make the animations -- 
source("R/5_making_animations.R")
# To make the gif-animations, additional software is necessary. 

# Check when nonstochastic give a negative variance: 
source("R/6_check_when_negative_variance.R")
