



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos

#Scripts 
source("code/first_script.R")
radcoeff_df <- read.csv("data/radcoeff_df.csv")

# Organizing database ####

# Database for abundance and richness analysis



ab_rich_dynamics <- merge(flora, radcoeff_df)   ## Algo pasa aquí. Se reduce la base de datos. REVISAR!

ab_rich_dynamics <- ab_rich_dynamics %>% 
  group_by(treatment, sampling, date, month) %>% 
  mutate(mean_richness = mean(richness),
         sd_richness = sd(richness),
         mean_abundance = mean(abundance_community),
         sd_abundance = sd(abundance_community),
         mean_Y_zipf = mean(Y_zipf),
         sd_Y_zipf = sd(Y_zipf),
         mean_mu_log = mean(mu_log),
         sd_mu_log = sd(mu_log),
         mean_sigma_log = mean(sigma_log),
         sd_sigma_log = sd(sigma_log)) %>% 
  ungroup()



# Database for biomass

biomass_dynamics_cleaned <- flora_cleanedbiomass %>% 
  filter(!sampling %in% c("0", "1", "2", "12")) %>% 
  group_by(treatment, sampling, date, month) %>% 
  mutate(mean_biomass = mean(biomass_community, na.rm = T),
         sd_biomass = sd(biomass_community, na.rm = T)) %>% 
  ungroup()


biomass_dynamics <- flora %>%
  filter(!sampling %in% c("0", "1", "2", "12")) %>% 
  group_by(treatment, sampling, date, month) %>% 
  mutate(mean_biomass = mean(biomass_community, na.rm = T),
         sd_biomass = sd(biomass_community, na.rm = T)) %>% 
  ungroup()






theme_set(theme_bw() +
            theme(axis.title.x = element_blank(),
                  legend.position = "right",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")





library(ggplot2)
library(grid)


a_abundance <-
  ggplot(ab_rich_dynamics,
         aes(x = date, y = abundance_community)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_abundance + sd_abundance, ymin = mean_abundance - sd_abundance, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_abundance, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  #geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = "Abundance", x = NULL) #

# Box plot
b_abundance <- 
  ggplot(ab_rich_dynamics, aes(y = abundance_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_abundance <- a_abundance +
  annotation_custom(
    grob = ggplotGrob(b_abundance),
    xmin = as.Date("2023-12-01"), # Adjust position: left boundary
    xmax = as.Date("2024-06-01"), # Adjust position: right boundary
    ymin = 5, # Adjust position: bottom boundary
    ymax = 50 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_abundance





a_richness <-
  ggplot(ab_rich_dynamics,
         aes(x = date, y = richness)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_richness + sd_richness, ymin = mean_richness - sd_richness, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_richness, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  #geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = "Richness", x = NULL) #

# Box plot
b_richness <- 
  ggplot(ab_rich_dynamics, aes(y = richness)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_richness <- a_richness +
  annotation_custom(
    grob = ggplotGrob(b_richness),
    xmin = as.Date("2024-07-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-20"), # Adjust position: right boundary
    ymin = 18, # Adjust position: bottom boundary
    ymax = 24 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_richness





a_biomass <-
  ggplot(biomass_dynamics,
         aes(x = date, y = biomass_community)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_biomass + sd_biomass, ymin = mean_biomass - sd_biomass, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_biomass, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  #geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  scale_y_log10() +
  
  labs(y = "log(Community biomass)", x = NULL) #

# Box plot
b_biomass <- 
  ggplot(biomass_dynamics, aes(y = log(biomass_community))) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_biomass <- a_biomass +
  annotation_custom(
    grob = ggplotGrob(b_biomass),
    xmin = as.Date("2023-12-01"), # Adjust position: left boundary
    xmax = as.Date("2024-04-20"), # Adjust position: right boundary
    ymin = 10, # Adjust position: bottom boundary
    ymax = 1000 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_biomass






a_biomass_cleaned <-
  ggplot(biomass_dynamics_cleaned,
         aes(x = date, y = biomass_community)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_biomass + sd_biomass, ymin = mean_biomass - sd_biomass, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_biomass, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = "Biomass (without outliers)", x = NULL) #

# Box plot
b_biomass_cleaned <- 
  ggplot(biomass_dynamics_cleaned, aes(y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_biomass_cleaned <- a_biomass_cleaned +
  annotation_custom(
    grob = ggplotGrob(b_biomass_cleaned),
    xmin = as.Date("2024-07-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-20"), # Adjust position: right boundary
    ymin = 350, # Adjust position: bottom boundary
    ymax = 500 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_biomass_cleaned

















