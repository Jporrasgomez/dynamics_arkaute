


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak) #Cargamos los paquetes que necesitamos


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

#RR_whole_aggregated %>% write.csv("results/RR_aggregated_results.csv")


    


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

#RR_whole_dynamics %>% write.csv("results/RR_dynamics_results.csv")


RR_whole_dynamics <- RR_whole_dynamics %>% 
  mutate(
    year = lubridate::year(date),
    sampling_date = as.factor(date)
  ) %>% 
  mutate(
    label_line = paste0(RR_descriptor, year)
  )

############### PLOTS ###########################


gg_theme <- 
  theme(
  panel.grid      = element_blank(),
  strip.background = element_blank(),
  strip.text      = element_text(face = "bold"),
  text            = element_text(size = 13
                                 ),
  legend.position = "none",
  axis.text.y     = element_text(face = "bold",
                                 angle = 90,
                                 hjust = 0.5))



limits_variables <- c("richness",
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



# 1. Aggregated analysis

{gg_RR_agg_c <- 
    RR_whole_aggregated %>% 
    filter(RR_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>%
    mutate(RR_descriptor = factor(RR_descriptor,
                                  levels = c("p_vs_c", "w_vs_c", "wp_vs_c"))) %>%
    
    ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
    
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
    
    geom_text(
      aes(
        y = variable,
        x = ifelse(RR < 0, lower_limit - scale/8,  upper_limit + scale/8),
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor,
        size = 13
      ),
      position = position_dodge2(width = 0.45, preserve = "single")
      )+

    scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
 
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
      linewidth = 0.6,
      width = 0.1
    ) +  
    
    geom_point(size = 1.5) + 
    
    geom_text(
      aes(
        y = variable,
        x = lower_limit - scale/8,     
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor,
        size = 13
        ))+
    
    scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
    
    scale_y_discrete(
      limits = rev(limits_variables),
      labels = labels_variables
    ) +
    
    scale_x_continuous(breaks = scales::breaks_pretty(n = 2)) +
    
    labs(x = NULL, y = NULL) +
    
    gg_theme 
    
  
  print(gg_RR_agg_wp)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}




 # 2. Temporal dynamics analysis

{gg_RR_dynamics_wp <- RR_whole_dynamics %>% 
    filter(RR_descriptor %in% c("wp_vs_p")) %>% 
    filter(!variable == "biomass") %>% 
    mutate(variable = factor(variable, 
                             levels = limits_variables, 
                             labels = labels_variables)) %>%
    
    ggplot(aes(x = date, y = delta_RR)) + 
    
    facet_grid(variable ~ year, scales = "free",
               labeller = labeller(
                 RR_descriptor = as_labeller(labels_RR_wp2), 
               )) +  
    
    #facet_grid( variable ~ RR_descriptor, scales = "free",
    #           labeller = labeller(
    #             RR_descriptor = as_labeller(labels_RR_wp2), 
    #           )) + 
    
    geom_hline(yintercept= 0, linetype = "dashed", color = "#00CAFF",
               linewidth = 0.5) +
    
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40",
               linewidth = 0.5)+
    
    geom_errorbar(aes(ymin = lower_limit,
                      ymax = upper_limit,
                      color = RR_descriptor),
                  alpha = 0.5,
                  width = 5,
                  linewidth = 0.5) +
    
    geom_point(aes(color = RR_descriptor), 
               size = 1.1) + 
    
    geom_line(aes(color = RR_descriptor),
              linewidth = 0.5) +
    
    geom_text(
      aes(
        x = date,
        y = ifelse(delta_RR < 0, lower_limit - 8 * scale, upper_limit + 8 * scale),   
        label = ifelse(null_effect == "NO", "*", NA_character_)),
      inherit.aes = FALSE,  # heredamos sólo date y color
      size  = 5,
      color = wp_CB)  +
 
    scale_x_date(
      date_labels = "%Y-%m-%d"  
    ) + 
    
    scale_y_continuous(
      breaks      = scales::pretty_breaks(n = 2),
      minor_breaks = NULL) +
    
    scale_color_manual(values = palette_RR_wp) +
    scale_fill_manual(values = palette_RR_wp) +
    
    labs(y = "Log Response Ratio") +
    
    gg_theme +
    
    theme(
      strip.text.y            = element_blank(),
      strip.background        = element_blank(),
      strip.text.x = element_blank()) + 
    
    labs(x = NULL, y = NULL) 
    
  print(gg_RR_dynamics_wp)
  #ggsave("results/Plots/protofinal/3.globalchange_dynamics_wide.png", plot = gg_dynamics_wp, dpi = 300)
  #saveRDS(gg_dynamics_wp, file = "results/plots/gg_dynamics_wp.rds")
}





{gg_RR_dynamics <- 
    RR_whole_dynamics %>% 
    filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>% 
    # 1) re-defino el factor con el orden deseado:
    mutate(
      RR_descriptor = factor(RR_descriptor, 
                             levels = c("wp_vs_c", "w_vs_c", "p_vs_c")),
      variable = factor(variable, 
                        levels = limits_variables, 
                        labels = labels_variables)
    ) %>%
    ggplot(aes(x = date, y = delta_RR, color = RR_descriptor, group = RR_descriptor)) + 
    
    facet_grid(
      variable ~ RR_descriptor, 
      scales = "free_y",
      labeller = labeller(RR_descriptor = as_labeller(labels_RR2))
    ) +  
    
    geom_hline(yintercept = 0, linetype="dashed", color = "gray25", linewidth=0.5) +
    geom_vline(xintercept = as.Date("2023-05-11"),
               linetype = "dashed",
               color="gray40",
               linewidth = 0.7) +
    
    geom_errorbar(aes(ymin = lower_limit,
                      ymax = upper_limit),
                  alpha = 0.5,
                  linewidth = 0.5) +
    
    geom_point(position = position_dodge2(width = 0.7, preserve = "single"),
               size = 1.1) +
    
    #geom_line(aes(group = year), linewidth = 0.5) +
    geom_line(linewidth = 0.5) +
    
    geom_text(aes(
      x = date,
      y = ifelse(delta_RR < 0, lower_limit - 8* scale, upper_limit  + 8 * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_)
    ),
    position = position_dodge2(width = 0.7, preserve = "single"),
    size = 5,
    show.legend = FALSE
    ) +
    
    scale_color_manual(values = palette_RR_CB) +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = 2)) +
    
    labs(x = NULL, y = "Log Response Ratio") +
    
    gg_theme +
    
    theme(
      strip.text.y            = element_blank(),
      strip.background        = element_blank(),
      strip.text.x = element_blank()) + 
    
    labs(x = NULL, y = NULL)
  
  print(gg_RR_dynamics)
  #ggsave("results/Plots/protofinal/2.dynamics.png", plot = gg_dynamics, dpi = 300)
  #saveRDS(gg_dynamics, file = "results/plots/gg_dynamics_wp.rds")
}




{gg_RR_dynamics_together <- 
    RR_whole_dynamics %>% 
    filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
    filter(variable != "biomass") %>% 
    mutate(
      RR_descriptor = factor(RR_descriptor, 
                             levels = c("wp_vs_c", "w_vs_c", "p_vs_c")),
      variable = factor(variable, 
                        levels = limits_variables, 
                        labels = labels_variables)
    ) %>%
    ggplot(aes(x = date, y = delta_RR, color = RR_descriptor, group = RR_descriptor)) + 
    
    
    facet_grid(
      variable ~ year,
      scales = "free", 
      labeller = labeller(RR_descriptor = as_labeller(labels_RR2))
    )+
    
    geom_hline(yintercept = 0, linetype="dashed", color = "gray25", linewidth=0.5) +
    geom_vline(xintercept = as.Date("2023-05-11"),
               linetype = "dashed",
               color="gray40",
               linewidth = 0.7) +
    
    geom_errorbar(aes(ymin = lower_limit,
                      ymax = upper_limit),
                  alpha = 0.5,
                  linewidth = 0.5,
                  position = position_dodge2(width = 12, preserve = "single")) +
    
    geom_point(position = position_dodge2(width = 12, preserve = "single"),
               size = 1.1) +
    
    #geom_line(aes(group = label_line), linewidth = 0.5) +
    geom_line(linewidth = 0.5,
              position = position_dodge2(width = 12, preserve = "single")) +
    
    geom_text(aes(
      x = date,
      y = ifelse(delta_RR < 0, lower_limit - 8 * scale, upper_limit  + 8 * scale),
      label = ifelse(null_effect == "NO", "*", NA_character_)
    ),
    position = position_dodge2(width = 12, preserve = "single"),
    size = 5,
    show.legend = FALSE
    ) +
    
    scale_x_date(
      date_labels = "%Y-%m-%d"  
    ) + 
    
    scale_color_manual(values = palette_RR_CB) +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = 2)) +
    
    labs(x = NULL, y = "Log Response Ratio") +
    
    gg_theme +
    
    theme(
      strip.text.y            = element_blank(),
      strip.background        = element_blank(),
      strip.text.x = element_blank()) + 
    
    labs(x = NULL, y = NULL)
  
  print(gg_RR_dynamics_together)
  
  #ggsave("results/Plots/protofinal/2.dynamics.png", plot = gg_dynamics, dpi = 300)
  #saveRDS(gg_dynamics, file = "results/plots/gg_dynamics_wp.rds")
}





## JOINING

gg_Warming_Effect <- 
  ggarrange(
    gg_RR_agg_wp   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_RR_dynamics_wp + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol     = 2, 
    widths   = c(1, 4)    # A ocupará 1/(1+2)=1/3 del ancho, B 2/3
  )
print(gg_Warming_Effect)
ggsave("results/Plots/protofinal/1.Warming_Effect.png", plot = gg_Warming_Effect, dpi = 300)




gg_Results <- 
  ggarrange(
    gg_RR_agg_c   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_RR_dynamics + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol     = 2, 
    widths   = c(1, 5)    # A ocupará 1/(1+2)=1/3 del ancho, B 2/3
  )
print(gg_Results)
ggsave("results/Plots/protofinal/1.Results.png", plot = gg_Results, dpi = 300)


gg_Results_2 <- 
  ggarrange(
    gg_RR_agg_c   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_RR_dynamics_together_gap + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol     = 2, 
    widths   = c(1, 5)    # A ocupará 1/(1+2)=1/3 del ancho, B 2/3
  )
print(gg_Results_2)
ggsave("results/Plots/protofinal/1.Results_3.png", plot = gg_Results_2, dpi = 300)

