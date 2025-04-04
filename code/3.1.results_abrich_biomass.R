



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


source("code/1.first_script.R")
source("code/palettes_labels.R")


palette <- palette5
labels <- labels3



source("code/meta_function/meta_function.R")

variables <- c("richness", "abundance", "Y_zipf")
                 # 1          # 2         # 3         
{i = 3
meta_function(ab_rich_dynamics, variables[i], "treatment")}

gg_stats_variable

gg_dunn_variable 
gg_ttest_variable

gg_dunn_0
mean_0 

gg_all1n
gg_facet

gg_delta_RR
gg_delta_RR_wp 

gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv



meta_function(biomass_imp_dynamics012, "biomass", "treatment")
gg_stats_variable
gg_dunn_variable 
gg_ttest_variable
gg_dunn_0
mean_0 
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

RR_biomass <- RR_treatment %>% 
  select(date, treatment, sampling, variable, delta_RR, se_delta_RR)

RR_biomass_wp <- RR_wp_vs_treatment %>% 
  select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)


# Storing results: 

  list <- list()
  list_wp <- list()
  
  
  for(i in 1:3){
    
    meta_function(ab_rich_dynamics, variables[i], "treatment")
    
    list[[i]] <- RR_treatment %>% 
      select(date, treatment, sampling, variable, delta_RR, se_delta_RR)
    
    list_wp[[i]] <- RR_wp_vs_treatment %>% 
      select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
    
  }
  
{RR_whole <- rbind(list[[1]], list[[2]])
RR_whole <- rbind(RR_whole, list[[3]])
RR_whole <- rbind(RR_whole, RR_biomass) %>% 
  mutate(variable = as.factor(variable)) %>%
  mutate(variable = factor(variable, levels = c("richness", "abundance", "Y_zipf", "biomass")))}
  

ggplot(RR_whole, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ treatment, scales = "free_y",
             labeller = labeller(
               treatment = as_labeller(labels_RR), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
             )) +  
  geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                    ymax = delta_RR + se_delta_RR,
                    color = treatment), alpha = 0.5) +
  geom_point(aes(color = treatment), size = 1.2) + 
  geom_line(aes(color = treatment)) +
  scale_color_manual(values = palette) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")



{RR_whole_wp <- rbind(list_wp[[1]], list_wp[[2]])
RR_whole_wp <- rbind(RR_whole_wp, list_wp[[3]])
RR_whole_wp <- rbind(RR_whole_wp, RR_biomass_wp) %>% 
  mutate(variable = as.factor(variable)) %>%
  mutate(variable = factor(variable, levels = c("richness", "abundance", "Y_zipf", "biomass")))}



ggplot(RR_whole_wp, aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR_wp), 
               variable = function(x) ifelse(x == "Y_zipf", "RAD coefficient (γ-Zipf)", str_to_title(x))
             )) +  
  geom_errorbar(aes(ymin = delta_RR - se_delta_RR,
                    ymax = delta_RR + se_delta_RR,
                    color = RR_descriptor), alpha = 0.5) +
  geom_point(aes(color = RR_descriptor), size = 1.2) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_wp_vs_treatment) +
  geom_hline(yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none")







