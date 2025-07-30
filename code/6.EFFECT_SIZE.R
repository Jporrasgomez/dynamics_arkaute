





rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak, effsize) #Cargamos los paquetes que necesitamos

source("code/palettes_labels.R")

palette <- palette_CB
labels <- labels3


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment)) 

arkaute_no0 <- arkaute %>% 
  filter(sampling != "0")



variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012",
               "NMDS1", "NMDS2", "PC1", "PC2", "SLA", "LDMC", "leafN")

LES_variables <- c("SLA", "LDMC", "leafN")



source("code/functions/eff_size_function.R")

list_eff <- list()
for (i in seq_along(variables)){
  
  effect_size(arkaute_no0, variables[i])
  
  list_eff[[i]] <- effsize_data
  
  rm(effsize_data)
}

effect_size_aggregated <- do.call(rbind, list_eff) 




source("code/functions/eff_size_dynamics_function.R")

list_eff_dyn <- list()
for (i in seq_along(variables)){
  
  effect_size_dynamics(arkaute, variables[i])
  
  list_eff_dyn[[i]] <- effsize_dynamics_data
  
  rm(effsize_dynamics_data)
}

effect_size_dynamics <- do.call(rbind, list_eff_dyn)



# Plots #


source("code/functions/gg_aggregated_function.R")
source("code/functions/gg_dynamics_function.R")

# Variables

agg <- effect_size_aggregated %>% 
  filter(!variable %in% LES_variables)


gg_eff_agg_c <- agg %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
  filter(variable != "biomass") %>%
  mutate(eff_descriptor = factor(eff_descriptor,
                                 levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))
  ) %>% 
  ggagg(palette_RR_CB, # using my function
        labels_RR2,
        "grey20",
        position   = position_dodge2(width = 0.3, preserve = "single")) 

gg_eff_agg_wp <- agg %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  filter(variable != "biomass") %>%
  ggagg(palette_RR_wp,
        labels_RR_wp,
        p_CB, 
        position   = position_dodge2(width = 0.1, preserve = "single"))



dyn <- effect_size_dynamics %>% 
  filter(!variable %in% LES_variables)

gg_eff_dynamics_c <- dyn %>% 
  filter(eff_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable != "biomass") %>% 
  mutate(
    variable = factor(variable, 
                      levels = limits_variables, 
                      labels = labels_variables)) %>% 
  
  ggdyn(palette_RR_CB,
        labels_RR2, 
        "grey20",
        position = position_dodge2(width = 12, preserve = "single")) 

gg_eff_dynamics_wp <- dyn %>% 
  filter(eff_descriptor %in% c("wp_vs_p")) %>% 
  filter(!variable == "biomass") %>% 
  mutate(variable = factor(variable, 
                           levels = limits_variables, 
                           labels = labels_variables)) %>% 
  ggdyn(palette_RR_wp,
        labels_RR_wp2,
        p_CB,
        position = position_dodge2(width = 4, preserve = "single"))


#gg_eff_agg_c
#gg_eff_dynamics_c + theme_minimal() + theme(legend.position = NULL)
#
#gg_eff_agg_wp
#gg_eff_dynamics_wp + theme_minimal() + theme(legend.position = NULL)


library(patchwork)

gg_Warming_Effect_hedges <- 
  (gg_eff_agg_wp   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wp + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    title = "Hedge's g effect size",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Warming_Effect_hedges)
ggsave("results/Plots/protofinal/1.Warming_Effect_hedges.png", plot = gg_Warming_Effect_hedges, dpi = 300)


gg_Results_hedges <- 
  ggarrange(
    gg_eff_agg_c   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_eff_dynamics_c + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol   = 2, 
    widths = c(1, 4)) +
  plot_annotation(
    title = "Hedge's g effect size",
    theme = theme( plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))
print(gg_Results_hedges)
ggsave("results/Plots/protofinal/1.Results_hedges.png", plot = gg_Results_hedges, dpi = 300)




# LES Variables


limits_variables <- c("LDMC", "leafN", "SLA")
labels_variables <- c("LDMC", "Leaf N", "SLA")


agg_LES <- effect_size_aggregated %>% 
  filter(variable %in% LES_variables)


gg_eff_agg_c_LES <- agg_LES %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
  filter(variable != "biomass") %>%
  mutate(eff_descriptor = factor(eff_descriptor,
                                 levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))
  ) %>% 
  ggagg(palette_RR_CB, # using my function
        labels_RR2,
        "grey20",
        position   = position_dodge2(width = 0.2, preserve = "single")) 

gg_eff_agg_wp_LES <- agg_LES %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  filter(variable != "biomass") %>%
  ggagg(palette_RR_wp,
        labels_RR_wp,
        p_CB, 
        position   = position_dodge2(width = 0.1, preserve = "single"))



dyn_LES <- effect_size_dynamics %>% 
  filter(variable %in% LES_variables)

gg_eff_dynamics_c_LES <- dyn_LES %>% 
  filter(eff_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
  filter(variable != "biomass") %>% 
  mutate(
    variable = factor(variable, 
                      levels = limits_variables, 
                      labels = labels_variables)) %>% 
  ggdyn(palette_RR_CB,
        labels_RR2, 
        "grey20",
        position = position_dodge2(width = 12, preserve = "single")) 

gg_eff_dynamics_wp_LES <- dyn_LES %>% 
  filter(eff_descriptor %in% c("wp_vs_p")) %>% 
  filter(!variable == "biomass") %>% 
  mutate(variable = factor(variable, 
                           levels = limits_variables, 
                           labels = labels_variables)) %>% 
  ggdyn(palette_RR_wp,
        labels_RR_wp2,
        p_CB,
        position = position_dodge2(width = 4, preserve = "single")) 

gg_eff_agg_c_LES
gg_eff_dynamics_c_LES + theme_minimal() + theme(legend.position = NULL)

gg_eff_agg_wp_LES
gg_eff_dynamics_wp_LES + theme_minimal() + theme(legend.position = NULL)




library(patchwork)

gg_Warming_Effect_hedges_LES <- 
  (gg_eff_agg_wp_LES   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wp_LES + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    title = "Hedge's g effect size",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Warming_Effect_hedges_LES)
ggsave("results/Plots/protofinal/1.Warming_Effect_hedges_LES.png", plot = gg_Warming_Effect_hedges_LES, dpi = 300)


gg_Results_hedges_LES <- 
  ggarrange(
    gg_eff_agg_c_LES   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_eff_dynamics_c_LES + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol   = 2, 
    widths = c(1, 4)) +
  plot_annotation(
    title = "Hedge's g effect size",
    theme = theme( plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))
print(gg_Results_hedges_LES)
ggsave("results/Plots/protofinal/1.Results_hedges_LES.png", plot = gg_Results_hedges_LES, dpi = 300)



