

# Este script no sirve para mucho ya


## CALCULAT indices BETA



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


cor1 <- cor(distance_matrix_bc, dist(nmds_bc$points[,1]), method = "pearson") 
cor2 <- cor(distance_matrix_bc, dist(nmds_bc$points[,2]), method = "pearson") 

# Compute percentage explained by each axis
explained_NMDS1 <- cor1^2 / (cor1^2 + cor2^2) * 100
explained_NMDS2 <- cor2^2 / (cor1^2 + cor2^2) * 100


source("code/meta_function/stats_function.R")
stats(nmds_df_sampling, "NMDS1", "treatment")
gg_stats
gg_dunn
gg_ttest

stats(nmds_df_sampling, "NMDS2", "treatment")
gg_stats
gg_dunn
gg_ttest

source("code/meta_function/sampling_0.R")
sampling_0(nmds_df_sampling, "NMDS1", "treatment")
gg_dunn_0
sampling_0(nmds_df_sampling, "NMDS2", "treatment")
gg_dunn_0


#Matrix at plot level


source("code/meta_function/meta_function.R")

cor1 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,1]), method = "pearson")
cor2 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,2]), method = "pearson")
cor3 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,3]), method = "pearson")






print(explained_NMDS1 <- (cor1^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)

meta_function(nmds_df_plot, "NMDS1", "treatment")
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


print(explained_NMDS2 <- (cor2^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)

meta_function(nmds_df_plot, "NMDS2", "treatment")
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


print(explained_NMDS3 <- (cor3^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)

meta_function(nmds_df_plot, "NMDS3", "treatment")
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






