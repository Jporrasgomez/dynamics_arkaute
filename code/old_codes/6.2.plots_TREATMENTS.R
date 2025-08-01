


rm(list = ls(all.names = TRUE))
library(dplyr) 
library(tidyverse) 
library(ggplot2) 


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment)) %>% 
  filter(sampling != "0")

     
                     
source("code/palettes_labels.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")



variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")
                 # 1         # 2         # 3         # 4          # 5       # 6      # 7      # 8    # 9  

list_c <- list()
gglist_c <- list()
list_wp <- list()
gglist_wp <- list()

 for (i in seq_along(variables)){
    RR_treatment_c(arkaute, variables[i])
    list_c[[i]] <- RR_treatment %>%
      select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
    gglist_c[[i]] <- gg_RR
    
    RR_treatment_wp(arkaute, variables[i])
    list_wp[[i]] <- RR_wp_vs_treatment %>%
      select(RR_descriptor, variable, starts_with("RR"), starts_with("se_RR"))
    gglist_wp[[i]] <- gg_RR_wp
  }
  
i = 4
gglist_c[[i]]
gglist_wp[[i]]

RR_c <- do.call(rbind, list_c)
RR_wp <- do.call(rbind, list_wp)



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

limits_variables <- rev(limits_variables)
labels_variables <- rev(labels_variables)



z = 1.96

{gg_RR_c <- 
  RR_c %>% 
    filter(!variable == "biomass") %>% 
  mutate(
    RR = if_else(variable == "Y_zipf", -1 * RR, RR), 
    se_RR = if_else(variable == "Y_zipf", -1 * se_RR, se_RR)) %>% 
  ## Multiplying by -1 gamma zipf in order to have positive values and being able to read the plots as
  ## evenness
  ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "#12D08C", linewidth = 1.1) +
  geom_errorbar(
    aes(ymin = RR - z * se_RR,
        ymax = RR + z * se_RR),
    linewidth = 1.1,
    position = position_dodge(width = 0.3),
    width = 0.2
  ) +  
  geom_point(position = position_dodge(width = 0.3), size = 2.5) + 
  scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
  #scale_y_continuous(labels = function(y) round((exp(y) - 1) * 100, 0)) +
  scale_x_discrete(
    limits = limits_variables,
    
    labels = labels_variables) +
  
  
  labs(x = NULL, y = "LRR", color = NULL) +
    theme(
      axis.title.x = element_blank(),  # Ajusta la distancia aquí
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 16),
      legend.position = "bottom"
    )
print(gg_RR_c)

#ggsave("results/Plots/protofinal/1.treatment_effects_SIBECOL.png", plot = gg_RR_c, dpi = 300)
}


{gg_RR_c <- 
    RR_c %>% 
    filter(variable != "biomass") %>% 
    mutate(
      RR    = if_else(variable == "Y_zipf", -1 * RR,    RR), 
      se_RR = if_else(variable == "Y_zipf", -1 * se_RR, se_RR)
    ) %>% 
    ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "#12D08C", linewidth = 0.9) +
    geom_errorbar(
      aes(xmin = RR - z * se_RR,
          xmax = RR + z * se_RR),
      linewidth = 0.9,
      position = position_dodge(width = 0.3),
      width = 0.2
    ) +  
    geom_point(position = position_dodge(width = 0.3), size = 2.5) + 
    scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    scale_y_discrete(
      limits = limits_variables,
      labels = labels_variables
    ) +
    labs(x = "Log Response Ratio", y = NULL) +
    theme(
      panel.grid      = element_blank(),
      strip.background = element_blank(),
      strip.text      = element_text(face = "bold"),
      text            = element_text(size = 13),
      legend.position = "none",
      axis.text.y     = element_text(face = "bold",
                                     angle = 90,
                                     hjust = 0.5)
    )
  
  print(gg_RR_c)
  
  ggsave("results/Plots/protofinal/1.treatment_effects_vertical.png", plot = gg_RR_c, dpi = 300)
}


{gg_RR_c <- 
    RR_c %>% 
    filter(!variable == "biomass") %>% 
    mutate(
      RR_perc = if_else(variable == "Y_zipf", -1 * RR_perc, RR_perc), 
      se_RR_perc = if_else(variable == "Y_zipf", -1 * se_RR_perc, se_RR_perc)) %>% 
    ## Multiplying by -1 gamma zipf in order to have positive values and being able to read the plots as
    ## evenness
    ggplot(aes(x = variable, y = RR_perc, color = RR_descriptor)) + 
    geom_errorbar(
      aes(ymin = RR_perc - z * se_RR_perc,
          ymax = RR_perc + z * se_RR_perc),
      linewidth = 1.1,
      position = position_dodge(width = 0.3),
      width = 0.2
    ) +  
    geom_point(position = position_dodge(width = 0.3), size = 2.5) + 
    scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    #scale_y_continuous(labels = function(y) round((exp(y) - 1) * 100, 0)) +
    scale_x_discrete(
      limits = limits_variables,
      
      labels = labels_variables) +
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(x = NULL, y = "LRR (% change)", color = NULL) +
    theme(
      axis.title.x = element_blank(),  # Ajusta la distancia aquí
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 16),
      legend.position = "bottom"
    )
  print(gg_RR_c)
  
  #ggsave("results/Plots/protofinal/1.treatment_effects.png", plot = gg_RR_c, dpi = 300)
}



 {gg_RR_wp <- 
RR_wp %>%
    filter(!variable == "biomass") %>% 
    filter(RR_descriptor == "wp_vs_p") %>% 
  mutate(
    RR = if_else(variable == "Y_zipf", -1 * RR, RR),
    se_RR = if_else(variable == "Y_zipf", -1 * se_RR, se_RR)) %>% 
  #filter(RR_descriptor == "wp_vs_p") %>% 
ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "#00CAFF", linewidth = 1.1) +
  geom_errorbar(aes(ymin = RR - z * se_RR,
                    ymax = RR + z * se_RR,
                    color = RR_descriptor), 
                linewidth = 1.1,
                position = position_dodge(width = 0.2),
                width = 0.1) +  
  geom_point(aes(color = RR_descriptor), position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp3) +
    scale_x_discrete(
      limits = limits_variables,
      
      labels = labels_variables) +
  
  labs(x = NULL, y = "LRR", color = NULL) +
  theme(
    legend.position = "bottom",
   # strip.text = element_blank(),  # 🔹 Quita el título del facet_wrap
    axis.text.x = element_blank(), # 🔹 Oculta etiquetas del eje X en todos los paneles
    panel.spacing = unit(0.5, "lines") # 🔹 Espaciado entre paneles
  ) +
  theme(
    axis.text.x = element_text() # 🔹 Vuelve a mostrar las etiquetas del último panel
  )  +
  theme(
       axis.title.x = element_blank(),  # Ajusta la distancia aquí
       panel.grid = element_blank(),
       strip.background = element_blank(),
       strip.text = element_text(face = "bold"),
       text = element_text(size = 16),
       legend.position = "bottom")

print(gg_RR_wp)

#ggsave("results/Plots/protofinal/2.globalchange_effects_SIBECOL.png", plot = gg_RR_wp, dpi = 300)
}









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

data_list <- list(arkaute_no0, arkaute_norm)

# Richness, abundance and evenness

variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")
                 # 1         # 2         # 3         # 4          # 5       # 6      # 7      # 8    # 9      
{i = 7
 j = 1
meta_function(data_list[[j]], variables[i], "treatment")
RR_treatment_c(data_list[[j]], variables[i])
RR_treatment_wp(data_list[[j]], variables[i])
}

gg_stats_variable

# Differences at treatment level
gg_dunn_variable 
gg_ttest_variable
gg_RR_dynamics
gg_RR_dynamics_wp

# Dynamics differences
gg_all1n
gg_facet
gg_delta_RR
gg_delta_RR_wp 

# Coefficient of variation
gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv



# Storing results: 

results_list <- list()
results_list_wp <- list()


for(i in seq_along(variables)){
  
  meta_function(arkaute, variables[i], "treatment")
  
  results_list[[i]] <- RR_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
  
  results_list_wp[[i]] <- RR_wp_vs_treatment %>% 
    select(date, RR_descriptor, sampling, variable, delta_RR, se_delta_RR)
}

RR_data <- do.call(rbind, results_list)
RR_data_wp <- do.call(rbind, results_list_wp)

RR_whole <- rbind(RR_data, RR_data_wp) %>% 
  mutate(variable = as.factor(variable)) %>% 
  mutate(
    delta_RR = if_else(variable == "Y_zipf", -1 * delta_RR, delta_RR),         ## To make evenness data easily interpretable
    
    
    variable = fct_recode(variable,
                          "Richness" = "richness",
                          "Cover" = "abundance",
                          "Evenness" = "Y_zipf",
                          "Biomass*" = "biomass",
                          "Biomass" = "biomass012",
                          "SC1" = "NMDS1",
                          "SC2" = "NMDS2",
                          "FT1" = "PC1", 
                          "FT2" = "PC2"),
    
    variable = fct_relevel(variable,
                           "Richness",
                           "Cover",
                           "Evenness",
                           "Biomass*",
                           "Biomass",
                           "SC1",
                           "SC2",
                           "FT1", 
                           "FT2")) 
  
  RR_whole <- RR_whole %>% 
  mutate(
    upper_limit = delta_RR + se_delta_RR * 1.96,   # calculating interval of confidence
    lower_limit = delta_RR - se_delta_RR * 1.96
  ) %>% 
    mutate(
      null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"))  # Which lines have a nule effect? this is: which lines cross the 0? 
    
    
Variables <-  c("Richness","Cover","Evenness","Biomass*","Biomass","SC1","SC2","FT1", "FT2")
scales_data <- list()
for(i in seq_along(variables)){
  
  scale_i <- RR_whole %>% 
    filter(variable == Variables[i]) %>% 
    mutate(
      scale = (max(abs(RR_richness$upper_limit)) + max(abs(lower_limit)))/100
    )
  
  scales_data[[i]] <- scale_i
  
}

RR_whole <- do.call(rbind, scales_data)

  
 




{gg_dynamics <- RR_whole %>% 
    filter(RR_descriptor %in% c("w_vs_c", "p_vs_c", "wp_vs_c")) %>% 
    filter(variable != "Biomass*") %>% 
ggplot(aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR2), 
             )) +  
  geom_hline(yintercept= 0, linetype = "dashed",
             #color = "#12D08C",
             color = "gray25",
             linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_errorbar(aes(ymin = lower_limit,
                    ymax = upper_limit,
                    color = RR_descriptor), alpha = 0.5, linewidth = 0.7) +
  geom_point(aes(color = RR_descriptor), size = 1.1) + 
    geom_text(
      aes(
        x = date,
        y = lower_limit - 10*scale,     
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor
      ),
      inherit.aes = FALSE,  # heredamos sólo date y color
      size  = 5
    )  +
    
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 3)
   #   labels = function(y) round((exp(y) - 1) * 100, 0)
    )+
  geom_line(aes(color = RR_descriptor), linewidth = 0.7) +
  scale_color_manual(values = palette_RR_CB) +
  labs(y = "Log Response Ratio") +
    theme(
      axis.title.x = element_blank(),  # Ajusta la distancia aquí
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 12),
      legend.position = "none"
    )
print(gg_dynamics)
#ggsave("results/Plots/protofinal/2.dynamics.png", plot = gg_dynamics, dpi = 300)
saveRDS(gg_dynamics, file = "results/plots/gg_dynamics_wp.rds")
}


{gg_dynamics_wp <- RR_whole %>% 
    filter(RR_descriptor %in% c("wp_vs_p")) %>% 
    filter(!variable == "Biomass*") %>% 
    #filter(variable == "Richness") %>% 
    #filter(variable %in% sibecol1) %>% 
ggplot(aes(x = date, y = delta_RR)) + 
  facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR_wp2), 
             )) +  
  geom_hline(yintercept= 0, linetype = "dashed", color = "#00CAFF",
             linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40",
             linewidth = 0.5)+
    
  geom_errorbar(aes(ymin = lower_limit,
                    ymax = upper_limit,
                    color = RR_descriptor), alpha = 0.5, linewidth = 0.5) +
    
  geom_point(aes(color = RR_descriptor), size = 1.1) + 
  
  geom_line(aes(color = RR_descriptor), linewidth = 0.5) +
    
    geom_text(
      aes(
        x = date,
        y = lower_limit - 10*scale,     
        label = ifelse(null_effect == "NO", "*", NA_character_)
      ),
      inherit.aes = FALSE,  # heredamos sólo date y color
      size  = 5,
      color = wp_CB
    )  +
  #geom_smooth(method = "lm", aes(color = RR_descriptor, fill = RR_descriptor), alpha = 0.3) +
  #  scale_y_continuous(
  #    breaks = scales::breaks_pretty(n = 3))+
    
    scale_y_continuous(
      breaks      = scales::pretty_breaks(n = 4),
      minor_breaks = NULL
    ) +
  scale_color_manual(values = palette_RR_wp) +
  scale_fill_manual(values = palette_RR_wp) +
    labs(y = "Log Response Ratio") +
    theme(
      axis.title.x = element_blank(),  # Ajusta la distancia aquí
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 12),
      legend.position = "none"
    )
print(gg_dynamics_wp)
#ggsave("results/Plots/protofinal/3.globalchange_dynamics_SIBECOL1.png", plot = gg_dynamics_wp, dpi = 300)
saveRDS(gg_dynamics_wp, file = "results/plots/gg_dynamics_wp.rds")
}











 