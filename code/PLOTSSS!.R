


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos


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



source("code/palettes_labels.R")

palette <- palette_CB
labels <- labels3


source("code/meta_function/meta_function.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")


variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")

####################### RR DATA ########################
 
###   1. Aggregated analysis 

list_c <- list()
list_wp <- list()

for (i in seq_along(variables)){
  RR_treatment_c(arkaute_no0, variables[i])
  list_c[[i]] <- RR_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
 
  RR_treatment_wp(arkaute_no0, variables[i])
  list_wp[[i]] <- RR_wp_vs_treatment %>%
    select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
}

RR_c_agg <- do.call(rbind, list_c) %>% 
  select(-RR_perc, -se_RR_perc)
RR_wp_agg <- do.call(rbind, list_wp)

RR_whole_aggregated <- 
  rbind(RR_c_agg, RR_wp_agg) %>% 
  mutate(
    variable = as.factor(variable)
    ) %>% 
  mutate(
    RR = if_else(variable == "Y_zipf", -1 * RR, RR)
    ) %>%
  mutate(
    upper_limit = RR + se_RR * 1.96,   # calculating interval of confidence
    lower_limit = RR - se_RR * 1.96
    ) %>% 
  mutate(
    null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"))

# Adding * significance label
scales_data <- list()
for(i in seq_along(variables)){
  
  scale_i <- RR_whole_aggregated %>% 
    filter(variable == variables[i]) %>% 
    mutate(
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))/100
    )
  
  scales_data[[i]] <- scale_i
  
}

RR_whole_aggregated <- do.call(rbind, scales_data)
    


### 2. Temporal dynamics analysis

list_c_dyn <- list()
list_wp_dyn <- list()
for(i in seq_along(variables)){
  
  meta_function(arkaute, variables[i], "treatment")
  
  list_c_dyn[[i]] <- RR_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  list_wp_dyn[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
}

RR_c_dyn <- do.call(rbind, list_c_dyn)
RR_wp_dyn <- do.call(rbind, list_wp_dyn)

RR_whole_dynamics <- rbind(RR_c_dyn, RR_wp_dyn) %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(
    delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR)) %>% 
  mutate(
    upper_limit = delta_RR + se_delta_RR * 1.96,   # calculating interval of confidence
    lower_limit = delta_RR - se_delta_RR * 1.96
      ) %>% 
  mutate(
    null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"))

scales_data <- list()
for(i in seq_along(variables)){
  
  scale_i <- RR_whole_dynamics %>% 
    filter(variable == variables[i]) %>% 
    mutate(
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))/100
    )
  
  scales_data[[i]] <- scale_i
  
}

RR_whole_dynamics <- do.call(rbind, scales_data)
    


limits_variables <- c(
  "richness",
  "abundance",
  "Y_zipf",
  #"biomass",
  "biomass012",
  "NMDS1",
  "NMDS2",
  "PC1",
  "PC2")
labels_variables <- c(
  "richness" = "Richness",
  "abundance" = "Cover",
  "Y_zipf" = "Evenness",
  #"biomass" = "Biomass",
  "biomass012" = "Biomass",
  "NMDS1" = "SC1",
  "NMDS2" = "SC2",
  "PC1" = "FT1", 
  "PC2" = "FT2")


gg_theme <- 
  theme(
  panel.grid      = element_blank(),
  strip.background = element_blank(),
  strip.text      = element_text(face = "bold"),
  text            = element_text(size = 13),
  legend.position = "none",
  axis.text.y     = element_text(face = "bold",
                                 angle = 90,
                                 hjust = 0.5))


{gg_RR_agg_c <- 
    RR_whole_aggregated %>% 
    filter(RR_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>%
    ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
    geom_vline(xintercept = 0, linetype = "dashed",
               #color = c_CB,
               color = "grey20",
               linewidth = 0.6) +
    geom_errorbar(
      aes(xmin = lower_limit,
          xmax = upper_limit),
      linewidth = 0.8,
      position = position_dodge(width = 0.5),
      width = 0.2
    ) +  
    geom_point(position = position_dodge(width = 0.5), size = 2) + 
    #geom_text(
    #  aes(
    #    y = variable,
    #    x = lower_limit - scale/10,     
    #    label = ifelse(null_effect == "NO", "*", NA_character_),
    #    color = RR_descriptor 
    #    ))+
    scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    #labs(x = "Log Response Ratio", y = NULL) +
    labs(x = NULL, y = NULL) +
    gg_theme
  
  print(gg_RR_agg_c)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}


{gg_RR_agg_wp <- 
    RR_whole_aggregated %>% 
    filter(RR_descriptor == "wp_vs_p") %>% 
    filter(variable != "biomass") %>%
    ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = p_CB, 
               linewidth = 0.6) +
    geom_errorbar(
      aes(xmin = lower_limit,
          xmax = upper_limit),
      linewidth = 0.8,
      position = position_dodge(width = 0.3),
      width = 0.2
    ) +  
    geom_point(position = position_dodge(width = 0.3), size = 2) + 
    geom_text(
      aes(
        y = variable,
        x = lower_limit - scale/10,     
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor,
        size = 12
        ))+
    scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    #labs(x = "Log Response Ratio", y = NULL) +
    gg_theme
    
  
  print(gg_RR_agg_wp)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}







