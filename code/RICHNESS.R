



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/1.first_script.R")
source("code/palettes_labels.R")

palette <- palette5
labels <- labels3


source("code/plots_functions_flora/meta_function.R")

variables <- c("richness", "abundance", "Y_zipf", "mu_log", "sigma_log")
                 # 1          # 2         # 3       # 4       # 5     

i = 3



meta_function(ab_rich_dynamics, variables[i], "treatment")

gg_dunn # Por qué no salen las significancias? 
gg_ttest


gg_all1n
gg_facet

gg_dunn_cv
gg_ttest_cv      ### Mirar que este plot sale siempre igual que el los boxplots normales (gg_dunn o gg_ttest)
gg_dynamics_cv

gg_RR
gg_delta_RR
gg_sigma_RR


gg_RR_wp 
gg_delta_RR_wp 
gg_sigma_RR_wp 


# BIOMASS

meta_function(biomass_imp_dynamics, "biomass", "treatment")

gg_dunn
gg_ttest


gg_all1n
gg_facet

gg_dunn_cv
gg_ttest_cv
gg_dynamics_cv

gg_RR
gg_delta_RR
gg_sigma_RR


gg_RR_wp 
gg_delta_RR_wp 
gg_sigma_RR_wp 


