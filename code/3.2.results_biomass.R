

rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/1.first_script.R")
source("code/palettes_labels.R")


palette <- palette5
labels <- labels3


source("code/meta_function/meta_function.R")

meta_function(biomass_imp_dynamics, "biomass", "treatment")
gg_stats_variable
gg_dunn_variable 
gg_ttest_variable
gg_dunn_0
gg_all1n
gg_facet
gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv
gg_RR
gg_delta_RR
gg_sigma_RR
gg_RR_wp 
gg_delta_RR_wp 
gg_sigma_RR_wp