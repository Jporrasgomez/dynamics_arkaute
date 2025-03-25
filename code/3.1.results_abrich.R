



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/1.first_script.R")
source("code/palettes_labels.R")


palette <- palette5
labels <- labels3



source("code/meta_function/meta_function.R")

variables <- c("richness", "abundance", "Y_zipf", "mu_log", "sigma_log")
                 # 1          # 2         # 3       # 4       # 5     
{i = 3
meta_function(ab_rich_dynamics, variables[i], "treatment")}

gg_stats_variable
gg_dunn_variable 
gg_ttest_variable
gg_dunn_0
gg_all1n

gg_facet
gg_delta_RR
gg_delta_RR_wp 
gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv

{
# Storing results: 


richness_list <- list()
abundance_list <- list()
Y_zipf_list <- list()
mu_log_list <- list()
sigma_log_list <- list()



meta_list <- list(richness_list, abundance_list, Y_zipf_list, mu_log_list, sigma_log_list)



for(i in 1:5){
  
  meta_function(ab_rich_dynamics, variables[i], "treatment")


meta_list[[i]][[1]] <- gg_stats_variable
meta_list[[i]][[2]] <- gg_dunn_variable 
meta_list[[i]][[3]] <- gg_ttest_variable
meta_list[[i]][[4]] <- gg_dunn_0
meta_list[[i]][[5]] <- gg_all1n
meta_list[[i]][[6]] <- gg_facet
meta_list[[i]][[7]] <- gg_stats_cv
meta_list[[i]][[8]] <- gg_dunn_cv
meta_list[[i]][[9]] <- gg_ttest_cv  
meta_list[[i]][[10]] <- gg_dynamics_cv
meta_list[[i]][[11]] <- gg_RR
meta_list[[i]][[12]] <- gg_delta_RR
meta_list[[i]][[13]] <- gg_sigma_RR
meta_list[[i]][[14]] <- gg_RR_wp 
meta_list[[i]][[15]] <- gg_delta_RR_wp 
meta_list[[i]][[16]] <- gg_sigma_RR_wp 

}


richness_list <- meta_list[[1]]
abundance_list <- meta_list[[2]]
Y_zipf_list <- meta_list[[3]]
mu_log_list <- meta_list[[4]]
sigma_log_list <- meta_list[[5]]



}








