



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos

#Scripts 
source("code/1.first_script.R")


source("code/palettes_labels.R")


# Organizing database ####

# Database for abundance and richness analysis






# General differences between treatments

#Statistical tests

# RICHNES 

#ab_rich_dynamics <- ab_rich_dynamics %>% 
#  filter(code %in% species_biomass_lm)


# Shapiro-Wilk test to check normality. If p-value <0.01 that means the distribution is not normal
hist((ab_rich_dynamics$richness), breaks = 20)
shapiro.test(ab_rich_dynamics$richness)


#To check the homoscedasticity (this is, the homogeinity of the variance ((how the variance varies between treatment)))
# We use the leveneTest
car::leveneTest(richness ~ treatment, data = ab_rich_dynamics)
#If p-value < 0.01 there is no homoscedasticity aka there is heterodasticity

# Kruskal-Wallis test for richness
kruskal.test(richness ~ treatment, data = ab_rich_dynamics)

#Since there are significant differences between treatments, we have to check which treatment are different to the others
dunn_rich <- dunn.test::dunn.test(ab_rich_dynamics$richness, ab_rich_dynamics$treatment, method = "bonferroni")

#Visualization of richness

ggplot(ab_rich_dynamics, aes(x = treatment, y = richness)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.6) + # Set the outline color to black
  scale_fill_manual(values = palette6) 

library(rstatix)

# Realizar la prueba de Dunn y agregar la columna y.position
dunn_results <- dunn_test(ab_rich_dynamics, richness ~ treatment, p.adjust.method = "bonferroni")

# Definir posiciones en el eje Y para las etiquetas
dunn_results <- dunn_results %>%
  mutate(y.position = max(ab_rich_dynamics$richness) + seq(1, by = 2, length.out = nrow(dunn_results)))

# Crear el gráfico
ggboxplot(ab_rich_dynamics, x = "treatment", y = "richness", fill = "treatment", alpha = 0.5) +
  #stat_compare_means(method = "kruskal.test", label.y = max(ab_rich_dynamics$richness) + 2) +  # Test global
  stat_pvalue_manual(dunn_results, label = "p.adj.signif", tip.length = 0.01) +  # Acortar las rayas verticales
  scale_fill_manual(values = palette6) +
  scale_x_discrete(labels = labels1) +
  labs(x = NULL, y = "Richness", fill = "Treatment") +
  theme(legend.position = "none", text = element_text(size = 10))



ggboxplot(ab_rich_dynamics, x = "treatment", y = "richness", fill = "treatment", alpha = 0.5) +
  stat_compare_means(comparisons = list(c("c", "w"), c("c", "p"), c("c", "wp"), c("w", "p"), c("w", "wp"), c("p", "wp")),
                     method = "t.test",
                     label = "p.signif",
                     tip.length = 0.01) +  # Show significance stars (*, **, ***)
  scale_fill_manual(values = palette6) +
  scale_x_discrete(labels = labels1) +
  labs( x = NULL, y = "Richness", fill = "Treatment") +
  theme(legend.position = "none")


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
  
  labs(y = "Richness", x = NULL)


# ABUNDANCE

shapiro.test(ab_rich_dynamics$abundance_community)
hist((ab_rich_dynamics$abundance_community), breaks = 50)
shapiro.test(log(ab_rich_dynamics$abundance_community))
hist(log(ab_rich_dynamics$abundance_community), breaks = 50)




car::leveneTest(abundance_community ~ treatment, data = ab_rich_dynamics)
kruskal.test(abundance_community ~ treatment, data = ab_rich_dynamics)
dunn.test(ab_rich_dynamics$abundance_community, ab_rich_dynamics$treatment, method = "bonferroni")

ggplot(ab_rich_dynamics, aes(x = treatment, y = abundance_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 
  #ggsignif::geom_signif(
  #  comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
  #  annotations = c("***", "***", "***", "***", "NS"), # Asterisks for significance
  #  map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
  #  y_position = c(150, 160, 170, 180, 190, 200),  # Adjust bracket positions
  #  tip_length = 0.01,  # Length of bracket tips
  #  textsize = 4)  # Size of asterisks
  #




#biomass_imp_dynamics
hist(biomass_imp_dynamics$biomass_community, breaks = 50)
shapiro.test(biomass_imp_dynamics$biomass_community)

hist(log(biomass_imp_dynamics$biomass_community), breaks = 50)
shapiro.test(log(biomass_imp_dynamics$biomass_community))

car::leveneTest(biomass_community ~ treatment, data = biomass_imp_dynamics)
kruskal.test(biomass_community ~ treatment, data = biomass_imp_dynamics)
dunn.test(biomass_imp_dynamics$biomass_community, biomass_imp_dynamics$treatment, method = "bonferroni")

car::leveneTest(log(biomass_community) ~ treatment, data = biomass_imp_dynamics)
kruskal.test(log(biomass_community) ~ treatment, data = biomass_imp_dynamics)
dunn.test(log(biomass_imp_dynamics$biomass_community), biomass_imp_dynamics$treatment, method = "bonferroni")

ggplot(biomass_imp_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 
  #ggsignif::geom_signif(
  #  comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
  #  annotations = c("NS", "***", "***","***", "**"), # Asterisks for significance
  #  map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
  #  y_position = c(9, 10, 11, 12, 13, 14),  # Adjust bracket positions
  #  tip_length = 0.01,  # Length of bracket tips
  #  textsize = 4  # Size of asterisks
  #   )


#BIOMASS_NOLM
hist(biomass_noimp_dynamics$biomass_community, breaks = 50)
shapiro.test(biomass_noimp_dynamics$biomass_community)

hist(log(biomass_noimp_dynamics$biomass_community), breaks = 50)
shapiro.test(log(biomass_noimp_dynamics$biomass_community))

car::leveneTest(log(biomass_community) ~ treatment, data = biomass_noimp_dynamics)
kruskal.test(log(biomass_community) ~ treatment, data = biomass_noimp_dynamics)
dunn.test(log(biomass_noimp_dynamics$biomass_community), biomass_noimp_dynamics$treatment, method = "bonferroni")

shapiro.test(biomass_noimp_dynamics$biomass_community)
car::leveneTest(biomass_community ~ treatment, data = biomass_noimp_dynamics)
kruskal.test(biomass_community ~ treatment, data = biomass_noimp_dynamics)
dunn.test(biomass_noimp_dynamics$biomass_community, biomass_noimp_dynamics$treatment, method = "bonferroni")

ggplot(biomass_noimp_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 
#ggsignif::geom_signif(
#  comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
#  annotations = c("NS", "NS", "***","***", "***"), # Asterisks for significance
#  map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
#  y_position = c(9, 10, 11, 12, 13, 14),  # Adjust bracket positions
#  tip_length = 0.01,  # Length of bracket tips
#  textsize = 4  # Size of asterisks
#   )


par(mfrow = c(1, 4))
hist(biomass_dynamics$biomass_community[which(biomass_dynamics$treatment == "c")], breaks = 25, main = "c")
hist(biomass_dynamics$biomass_community[which(biomass_dynamics$treatment == "w")], breaks = 25, main = "w")
hist(biomass_dynamics$biomass_community[which(biomass_dynamics$treatment == "p")], breaks = 25, main = "p")
hist(biomass_dynamics$biomass_community[which(biomass_dynamics$treatment == "wp")], breaks = 25, main = "wp")





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
  #geom_line(aes(x = date, y = mean_abundance, color = treatment)) + 
  #
  geom_point(aes(x = date, y = mean_abundance, color = treatment), fill = "white", 
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
  
  labs(y = "Abundance", x = NULL) #

# Box plot
b_abundance <- 
  ggplot(ab_rich_dynamics, aes(y = abundance_community, x = treatment)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("*", "***", "***", "***", "NS"), # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(150, 160, 170, 180, 190, 200),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  ) +
  # Transparent plot background+ 
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
  ggplot(ab_rich_dynamics, aes(x = treatment, y = richness)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("***", "***", "NS", "***", "***"),,  # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(24, 26, 28, 30, 32, 34),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  ) +
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
    ymin = 16, # Adjust position: bottom boundary
    ymax = 24 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_richness





#a_biomass_imp_dynamics <- 
  ggplot(biomass_imp_dynamics,
         aes(x = date, y = biomass_community)) +
  
  geom_smooth(
    se = T, aes(color = treatment, fill = treatment),
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
  #scale_y_log10() +
  
  labs(y = "log(Community biomass)", x = NULL) #

# Box plot
b_biomass_lm <- 
  ggplot(biomass_imp_dynamics, aes(y = log(biomass_community),x = treatment)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("NS", "***", "***","***", "**"), # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(9, 10, 11, 12, 13, 14),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  ) +# Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_biomass <- a_biomass_lm +
  annotation_custom(
    grob = ggplotGrob(b_biomass_lm),
    xmin = as.Date("2023-12-01"), # Adjust position: left boundary
    xmax = as.Date("2024-04-20"), # Adjust position: right boundary
    ymin = 10, # Adjust position: bottom boundary
    ymax = 1000 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_biomass_lm






a_biomass_nolm <-
  ggplot(biomass_noimp_dynamics,
         aes(x = date, y = biomass_community)) + 
  
  geom_smooth(
    se = F, aes(color = treatment, fill = treatment),
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
  #scale_y_log10() +
  
  labs(y = "log(Community biomass)", x = NULL) #

# Box plot
b_biomass_nolm <- 
  ggplot(biomass_molm_dynamics, aes(y = log(biomass_community),x = treatment)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("NS", "***", "***","***", "**"), # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(9, 10, 11, 12, 13, 14),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  ) +# Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_biomass <- a_biomass_nolm +
  annotation_custom(
    grob = ggplotGrob(b_biomass_lm),
    xmin = as.Date("2023-12-01"), # Adjust position: left boundary
    xmax = as.Date("2024-04-20"), # Adjust position: right boundary
    ymin = 10, # Adjust position: bottom boundary
    ymax = 1000 # Adjust position: top boundary
  )

# Render combined plot
ggcomb_biomass_lm









# Log response ratio to see differences dynamically 

# Response ratio ####

lrr <- ab_rich_dynamics 

lrr_richness <- lrr %>% 
  select(plot, sampling, sampling_date, treatment, date, code, richness, mean_richness, sd_richness)
lrr_abundance <- lrr %>% 
  select(plot, sampling, sampling_date, treatment, date, code, abundance_community, mean_abundance, sd_abundance)
lrr_Y_zipf <- lrr %>% 
  select(plot, sampling, sampling_date, treatment, date, code, Y_zipf, mean_Y_zipf, sd_Y_zipf)
lrr_mu_log <- lrr %>% 
  select(plot, sampling, sampling_date, treatment, date, code, mu_log, mean_mu_log, sd_mu_log)
lrr_sigma_log <- lrr %>% 
  select(plot, sampling, sampling_date, treatment, date, code, sigma_log, mean_sigma_log, sd_sigma_log)



#Sampling 2 has several plots with very few species, which generates problems at RADs
# At plot 15, we have only 1 species so there is no RAD
# At plot 13, we have 2 species, so the RAD values are not reliable. Im deleting this data by hand: 
lrr_Y_zipf$Y_zipf[lrr$sampling == "2" & lrr$plot == "13"] <- NA
lrr_mu_log$mu_log[lrr$sampling == "2" & lrr$plot == "13"] <- NA
lrr_sigma_log$sigma_log[lrr$sampling == "2" & lrr$plot == "13"] <- NA


samps <- unique(lrr$sampling)

# Cuantas replicas hay por muestreo y variable ? Vamos a ver los NA


length(lrr_richness$mean_richness[which(is.na(lrr_richness$mean_richness))])
length(lrr_abundance$mean_abundance[which(is.na(lrr_abundance$mean_abundance))])
length(lrr_Y_zipf$mean_Y_zipf[which(is.na(lrr_Y_zipf$mean_Y_zipf))])
length(lrr_mu_log$mean_mu_log[which(is.na(lrr_mu_log$mean_mu_log))])
length(lrr_sigma_log$mean_sigma_log[which(is.na(lrr_sigma_log$mean_sigma_log))])


samps <- unique(lrr$sampling)
treats <- unique(lrr$treatment)


for (i in seq_along(samps)){
  for(j in seq_along(treats)){
    
    samp = samps[i]
    treat = treats[j]
    
  lrr_richness_try <- lrr_richness %>% 
      filter(sampling %in% samp) %>% 
      filter(treatment %in% treat) %>% 
      mutate(!!paste("ref", treat, sep = "_") := mean_richness) %>% 
      full_join(lrr_richness)
    
  }
}


lrr_richness_try <- lrr_richness %>% 
  filter(sampling %in% 14) %>% 
  filter(treatment %in% "p") %>% 
  mutate(!!paste("ref", "p", sep = "_") := mean_richness)



# Iterating over unique sampling values
samps <- unique(lrr$sampling)

for (i in seq_along(samps)) {
  samp <- samps[i]
  
  for (var in c("richness", "abundance", "Y_zipf", "mu_log", "sigma_log")) {
    mean_col <- paste0("mean_", var)
    sd_col <- paste0("sd_", var)
    ref_means <- paste0("ref_", c("c", "w", "p"), "_mean")
    ref_sds <- paste0("ref_", c("c", "w", "p"), "_sd")
    
    for (treat in c("c", "w", "p")) {
      treat_filter <- lrr$treatment == treat & lrr$sampling == samp
      ref_mean_val <- unique(lrr[[mean_col]][treat_filter])
      ref_sd_val <- unique(lrr[[sd_col]][treat_filter])
      
      assign(paste0("lrr_", var), 
             get(paste0("lrr_", var)) %>% 
               mutate(
                 !!ref_means[which(c("c", "w", "p") == treat)] := ref_mean_val,
                 !!ref_sds[which(c("c", "w", "p") == treat)] := ref_sd_val
               )
      )
    }
  }
}

  

lrr <- lrr %>%
  mutate(
    across(
      .cols = c(mean_richness, mean_abundance, mean_Y_zipf, mean_mu_log, mean_sigma_log),
      .fns = ~ log(. / get(paste0("ref_c_", cur_column()))),
      .names = "lrr_{.col}_c"
    )
  )

lrr_richness$lrr_c <- log(lrr_richness$mean_richness/lrr_richness$ref_c_mean)
lrr_abundance$lrr_c <- log(lrr_abundance$mean_abundance/lrr_abundance$ref_c_mean)
lrr_Y_zipf$lrr_c <- log(lrr_Y_zipf$mean_Y_zipf/lrr_Y_zipf$ref_c_mean)
lrr_mu_log$lrr_c <- log(lrr_mu_log$mean_mu_log/lrr_mu_log$ref_c_mean)
lrr_sigma_log$lrr_c <- log(lrr_sigma_log$mean_sigma_log/lrr_sigma_log$ref_c_mean)


# Calculating the varianc of the LRR. 4 is the population number. In our case is  
# 4, since we have 4 replicates to compare. I should check if all samplings contain 4 replicates
# This formula was gotten from Lajeunesse 2015, but it is originally from Hunter and Schmidt 1990. 
lrr$var_lrr_richness_c <-
  ((lrr$sd_richness)^2/(4*(lrr$mean_richness)^2)) + ((lrr$ref_c_sd_richness)^2/(4*(lrr$ref_c_mean_richness)^2))
lrr$var_lrr_abundance_c <-
  ((lrr$sd_abundance)^2/(4*(lrr$mean_abundance)^2)) + ((lrr$ref_c_sd_abundance)^2/(4*(lrr$ref_c_mean_abundance)^2))
lrr$var_lrr_Y_zipf_c <-
  ((lrr$sd_Y_zipf)^2/(4*(lrr$mean_Y_zipf)^2)) + ((lrr$ref_c_sd_Y_zipf)^2/(4*(lrr$ref_c_mean_Y_zipf)^2))
lrr$var_lrr_mu_log_c <-
  ((lrr$sd_mu_log)^2/(4*(lrr$mean_mu_log)^2)) + ((lrr$ref_c_sd_mu_log)^2/(4*(lrr$ref_c_mean_mu_log)^2))
lrr$var_lrr_sigma_log_c <-
  ((lrr$sd_sigma_log)^2/(4*(lrr$mean_sigma_log)^2)) + ((lrr$ref_c_sd_sigma_log)^2/(4*(lrr$ref_c_mean_sigma_log)^2))
  
  
treatment_labs_dynamics <- c("Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("w", "p", "wp")


ggplot(lrr,
       aes(x = date, y = lrr_richness_c)) + 
  facet_wrap(~ treatment, labeller = labeller(treatment = treatment_labs_dynamics, nrow = 1, ncol = 3))+
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5) +
  
  geom_errorbar(aes(ymax = lrr_richness_c + var_lrr_richness_c, ymin = lrr_richness_c - var_lrr_richness_c, color = treatment),
                , alpha = 0.2) + 
  
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  scale_y_log10() +
  
  labs(y = "LRR (Richness)", x = NULL) #



