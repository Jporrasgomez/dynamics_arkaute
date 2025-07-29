



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



variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")


source("code/eff_size_function.R")

list_eff <- list()
for (i in seq_along(variables)){
  
  effect_size(arkaute_no0, variables[i])
  
  list_eff[[i]] <- effsize_data
  
  rm(effsize_data)
}

effect_size_aggregated <- do.call(rbind, list_eff) 




source("code/eff_size_dynamics_function.R")

list_eff_dyn <- list()
for (i in seq_along(variables)){
  
  effect_size_dynamics(arkaute, variables[i])
  
  list_eff_dyn[[i]] <- effsize_dynamics_data
  
  rm(effsize_dynamics_data)
}

effect_size_dynamics <- do.call(rbind, list_eff_dyn)






# Plots #

source("code/gg_aggregated_function.R")


## All Cohen's d

cohens_data <- effect_size_aggregated %>% 
  filter(analysis == "cohen's_d")

gg_eff_agg_c <- cohens_data %>% 
    filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>%
    mutate(eff_descriptor = factor(eff_descriptor,
                                  levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))
           ) %>% 
  ggagg(palette_RR_CB, # using my function
        labels_RR2,
        "grey20",
        position   = position_dodge2(width = 0.5, preserve = "single")) 
  
gg_eff_agg_wp <- cohens_data %>% 
    filter(eff_descriptor == "wp_vs_p") %>% 
    filter(variable != "biomass") %>%
  ggagg(palette_RR_wp,
      labels_RR_wp,
      p_CB, 
      position   = position_dodge2(width = 0.2, preserve = "single"))


source("code/gg_dynamics_function.R")

hedges_data <-  effect_size_dynamics %>% 
  filter(analysis == "hedge's_g")


gg_eff_dynamics_c <- hedges_data %>% 
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

gg_eff_dynamics_wp <- hedges_data %>% 
  filter(eff_descriptor %in% c("wp_vs_p")) %>% 
  filter(!variable == "biomass") %>% 
  mutate(variable = factor(variable, 
                           levels = limits_variables, 
                           labels = labels_variables)) %>% 
  ggdyn(palette_RR_wp,
      labels_RR_wp2,
      p_CB,
      position = position_dodge2(width = 4, preserve = "single"))

library(patchwork)

gg_Warming_Effect_cohens <- 
  (gg_eff_agg_wp   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wp + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    title = "Effect size: Cohen's d with Hedge's g correction for dynamics",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Warming_Effect_cohens)
ggsave("results/Plots/protofinal/1.Warming_Effect_cohen.png", plot = gg_Warming_Effect_cohens, dpi = 300)


gg_Results_cohens <- 
  ggarrange(
    gg_eff_agg_c   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_eff_dynamics_c + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol   = 2, 
    widths = c(1, 4)) +
  plot_annotation(
    title = "Effect size: Cohen's d with Hedge's g correction for dynamics",
    theme = theme( plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))
print(gg_Results_cohens)
ggsave("results/Plots/protofinal/1.Results_cohen.png", plot = gg_Results_cohens, dpi = 300)







## LRR + Cohens'd/Hedges'g for NMDS and PC

sc_ft_var <- c("NMDS1","NMDS2","PC1","PC2")

data_lrr_cohen <- effect_size_aggregated %>% 
  filter(
    (variable %in% sc_ft_var  & analysis == "cohen's_d") |
      (!(variable %in% sc_ft_var) & analysis == "LRR"))

gg_eff_agg_mix <- data_lrr_cohen %>% 
  filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
  filter(variable != "biomass") %>%
  mutate(eff_descriptor = factor(eff_descriptor,
                                 levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))) %>% 
  ggagg(palette_RR_CB,
      labels_RR2,
      "grey20",
      position   = position_dodge2(width = 0.5, preserve = "single")) 

gg_eff_agg_wp_mix <- data_lrr_cohen %>% 
  filter(eff_descriptor == "wp_vs_p") %>% 
  filter(variable != "biomass") %>% 
  ggagg(palette_RR_wp,
        labels_RR_wp,
        p_CB, 
        position   = position_dodge2(width = 0.2, preserve = "single"))


data_lrr_hedges <- effect_size_dynamics %>% 
  filter(
    (variable %in% sc_ft_var  & analysis == "hedge's_g") |
      (!(variable %in% sc_ft_var) & analysis == "LRR"))


gg_eff_dynamics_c_mix <- data_lrr_hedges %>% 
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


gg_eff_dynamics_wp_mix <- data_lrr_hedges %>% 
  filter(eff_descriptor %in% c("wp_vs_p")) %>% 
  filter(!variable == "biomass") %>% 
  mutate(variable = factor(variable, 
                           levels = limits_variables, 
                           labels = labels_variables)) %>% 
  ggdyn(palette_RR_wp,
        labels_RR_wp2,
        p_CB,
        position = position_dodge2(width = 4, preserve = "single")) 


gg_Warming_Effect_mix <- 
  (gg_eff_agg_wp_mix   + theme(plot.margin = margin(5,5,5,5))) +
  (gg_eff_dynamics_wp_mix + theme(plot.margin = margin(5,5,5,5))) +
  plot_layout(
    ncol   = 2,
    widths = c(1, 4)) +
  plot_annotation(
    title = "Effect size: LRR for all variables but SC and FT (Cohen and Hedge)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))

print(gg_Warming_Effect_mix)
ggsave("results/Plots/protofinal/1.Warming_Effect_mix.png", plot = gg_Warming_Effect_mix, dpi = 300)


gg_Results_mix <- 
  ggarrange(
    gg_eff_agg_mix  + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_eff_dynamics_c_mix + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol   = 2, 
    widths = c(1, 4)) +
  plot_annotation(
    title = "Effect size: LRR for all variables but SC and FT (Cohen and Hedge)",
    theme = theme( plot.title = element_text(face = "bold", size = 10, hjust = 0.5)))
print(gg_Results_mix)
ggsave("results/Plots/protofinal/1.Results_mix.png", plot = gg_Results_mix, dpi = 300)
