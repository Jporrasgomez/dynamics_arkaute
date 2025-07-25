



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
}

effect_size_aggregated <- do.call(rbind, list_eff) %>% 
  mutate(
    eff_value = if_else(variable == "Y_zipf" & analysis == "LRR", -1 * eff_value, eff_value))



source("code/eff_size_dynamics_function.R")

list_eff_dyn <- list()
for (i in seq_along(variables)){
  
  effect_size_dynamics(arkaute, variables[i])
  
  list_eff_dyn[[i]] <- effsize_dynamics_data
}

effect_size_dynamics <- do.call(rbind, list_eff_dyn) %>% 
mutate(
  eff_value = if_else(variable == "Y_zipf" & analysis == "LRR", -1 * eff_value, eff_value))




# Plots #


limits_variables <- c("richness",
                      "abundance",
                      "Y_zipf",
                      #"biomass",
                      "biomass012",
                      "NMDS1",
                      "NMDS2",
                      "PC1",
                      "PC2")

labels_variables <- c("richness" = "Richness",
                      "abundance" = "Cover",
                      "Y_zipf" = "Evenness",
                      #"biomass" = "Biomass",
                      "biomass012" = "Biomass",
                      "NMDS1" = "SC1",
                      "NMDS2" = "SC2",
                      "PC1" = "FT1", 
                      "PC2" = "FT2")
                      





{gg_RR_agg_c <- 
    effect_size_aggregated %>% 
    filter(analysis == "cohen's_d") %>% 
    filter(eff_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>%
    mutate(eff_descriptor = factor(eff_descriptor,
                                  levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))) %>%
    
    ggplot(aes(y = variable, x = eff_value, color = eff_descriptor)) + 
    
    geom_vline(xintercept = 0, linetype = "dashed",
               #color = c_CB,
               color = "grey20",
               linewidth = 0.6) +
    
    geom_errorbar(
      aes(xmin = lower_limit,
          xmax = upper_limit),
      linewidth = 0.5,
      position = position_dodge(width = 0.5),
      width = 0.1
    ) +  
    
    geom_point(position = position_dodge(width = 0.5), size = 1.5) + 
    
   # geom_text(
   #   aes(
   #     y = variable,
   #     x = ifelse(eff_value < 0, lower_limit - scale/8,  upper_limit + scale/8),
   #     label = ifelse(null_effect == "NO", "*", NA_character_),
   #     color = eff_descriptor,
   #     size = 13
   #   ),
   #   position = position_dodge2(width = 0.45, preserve = "single")
   # )+
    
    scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    
    labs(x = NULL, y = NULL) +
    
    gg_RR_theme
  
  
  print(gg_RR_agg_c)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}



{gg_RR_agg_wp <- 
    effect_size_aggregated %>% 
    filter(analysis == "cohen's_d") %>% 
    filter(eff_descriptor == "wp_vs_p") %>% 
    filter(variable != "biomass") %>%
    
    ggplot(aes(y = variable, x = eff_value, color = eff_descriptor)) + 
    
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = p_CB, 
               linewidth = 0.6) +
    
    geom_errorbar(
      aes(xmin = lower_limit,
          xmax = upper_limit),
      linewidth = 0.6,
      width = 0.1
    ) +  
    
    geom_point(size = 1.5) + 
    
   # geom_text(
   #   aes(
   #     y = variable,
   #     x = lower_limit - scale/8,     
   #     label = ifelse(null_effect == "NO", "*", NA_character_),
   #     color = eff_descriptor,
   #     size = 13
   #   ))+
   # 
    scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
    
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    
    scale_x_continuous(breaks = scales::breaks_pretty(n = 2)) +
    
    labs(x = NULL, y = NULL) +
    
    gg_RR_theme 
  
  
  print(gg_RR_agg_wp)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}
