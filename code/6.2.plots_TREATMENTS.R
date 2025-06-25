


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
  "PC1" = "CWM1", 
  "PC2" = "CWM2")





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

 