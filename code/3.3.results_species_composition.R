




rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos

source("code/1.first_script.R")
source("code/palettes_labels.R")


palette <- palette5
labels <- labels3

source("code/4.species_composition_NMDSbray.R")


ggarrange(
  gglist1[[2]],
  gglist1[[1]],
  gglist1[[3]],
  gglist1[[4]], 
  ncol = 2, nrow = 2)

ggnmds_alltreatments

ggNMDS1_dynamics_sampling
ggNMDS2_dynamics_sampling
gg_samplings

# Matrix at sampling level

source("code/meta_function/stats_function.R")
stats(nmds_df_sampling, "NMDS1", "treatment")
gg_stats_variable
gg_dunn_variable 
gg_ttest_variable

stats(nmds_df_sampling, "NMDS2", "treatment")
gg_stats_variable
gg_dunn_variable 
gg_ttest_variable

source("code/meta_function/sampling_0.R")
sampling_0(nmds_df_sampling, "NMDS1", "treatment")
gg_dunn_0
sampling_0(nmds_df_sampling, "NMDS2", "treatment")
gg_dunn_0




#Matrix at plot level


source("code/meta_function/meta_function.R")


variables <- c("NMDS1", "NMDS2", "NMDS3")

i <- 2
#Comprobar resultados para CV y los effect size
meta_function(nmds_df_plot, variables[i], "treatment")
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




