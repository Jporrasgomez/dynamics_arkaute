



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos

#Scripts 
source("code/first_script.R")
radcoeff_df <- read.csv("data/radcoeff_df.csv")

# Organizing database ####

# Database for abundance and richness analysis

ab_rich_dynamics <-  flora_abrich %>%
  group_by(sampling, omw_date, date, month, treatment, plot) %>%
  reframe(richness = richness,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(sampling, omw_date, date, month, plot, treatment, richness, abundance)



ab_rich_dynamics <- merge(ab_rich_dynamics, radcoeff_df)   ## Algo pasa aquí. Se reduce la base de datos. REVISAR!

mean_sd_abrich_dynamics<- ab_rich_dynamics %>%
  group_by(treatment, sampling, date, month) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness),
            mean_abundance = mean(abundance),
            sd_abundance = sd(abundance),
            mean_Y_zipf = mean(Y_zipf),
            sd_Y_zipf = sd(Y_zipf),
            mean_mu_log = mean(mu_log),
            sd_mu_log = sd(mu_log),
            mean_sigma_log = mean(sigma_log),
            sd_sigma_log = sd(sigma_log))



# Database for biomass

biomass_dynamics <- flora_biomass_lm %>%
  filter(!sampling %in% c("0", "1", "2", "12"))  %>%
  rename(biomass = biomass_total) %>%
  group_by(sampling, omw_date,  date, month, treatment, plot) %>%
  reframe(biomass = biomass) %>% # total coverage of plot
  distinct(sampling, omw_date, date, month, plot, treatment, biomass)



mean_sd_biomass_dynamics<- biomass_dynamics %>%
  group_by(treatment, sampling, date, month) %>%
  summarize(mean_biomass = mean(biomass),
            sd_biomass = sd(biomass))



#dynamics <- full_join (ab_rich_dynamics, biomass_dynamics)
#
#dynamics <- dynamics %>%
#  pivot_longer(
#    cols = - c(sampling, plot, treatment, sampling_date, omw_date, date, month),
#    names_to = "variable", 
#    values_to = "var_value"
#  )




theme_set(theme_bw() +
            theme(axis.title.x = element_blank(),
                  legend.position = "right",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")


ggplot(ab_rich_dynamics, aes(x = date, y = abundance)) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 # Adjust alpha for transparency
  ) +
  geom_point(aes(color = treatment, shape = treatment)) +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels


ggplot(ab_rich_dynamics, aes(x = date, y = richness)) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.8, alpha = 0.2 # Adjust alpha for transparency
  ) +
  geom_point(aes(color = treatment, shape = treatment)) +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(biomass_dynamics, aes(x = date, y = biomass)) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.8,  alpha = 0.2 # Adjust alpha for transparency
  ) +
  geom_point(aes(color = treatment, shape = treatment)) +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

ggplot(biomass_dynamics, aes(x = date, y = log(biomass))) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess",span = 0.8,  alpha = 0.2 # Adjust alpha for transparency
  ) +
  geom_point(aes(color = treatment, shape = treatment)) +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

# Data characteristics

hist(ab_rich_dynamics$richness)
hist(ab_rich_dynamics$abundance)
hist(biomass_dynamics$biomass)

