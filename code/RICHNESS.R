

rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix) #Cargamos los paquetes que necesitamos



source("code/1.first_script.R")

source("code/palettes_labels.R")


# RICHNES 

#ab_rich_dynamics <- ab_rich_dynamics %>% 
#  filter(code %in% species_biomass_lm)

hist((ab_rich_dynamics$richness), breaks = 20)
# Shapiro-Wilk test to check normality. If p-value <0.01 that means the distribution is not normal
shapiro.test(ab_rich_dynamics$richness)
#To check the homoscedasticity (this is, the homogeinity of the variance ((how the variance varies between treatment)))
# We use the leveneTest
car::leveneTest(richness ~ treatment, data = ab_rich_dynamics)
#If p-value < 0.01 there is no homoscedasticity aka there is heterodasticity
# Kruskal-Wallis test for richness
kruskal.test(richness ~ treatment, data = ab_rich_dynamics)
#Since there are significant differences between treatments, we have to check which treatment are different to the others
dunn_rich <- dunn.test::dunn.test(ab_rich_dynamics$richness, ab_rich_dynamics$treatment, method = "bonferroni")


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

# Checking statistical significance using t.test

ggboxplot(ab_rich_dynamics, x = "treatment", y = "richness", fill = "treatment", alpha = 0.5) +
  stat_compare_means(comparisons = list(c("c", "w"), c("c", "p"), c("c", "wp"), c("w", "p"), c("w", "wp"), c("p", "wp")),
                     method = "t.test",
                     label = "p.signif",
                     tip.length = 0.01) +  # Show significance stars (*, **, ***)
  scale_fill_manual(values = palette6) +
  scale_x_discrete(labels = labels1) +
  labs( x = NULL, y = "Richness", fill = "Treatment") +
  theme(legend.position = "none", text = element_text(size = 10))


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
  
  scale_colour_manual(values = palette7) +
  
  scale_fill_manual(values = palette7) +
  
  scale_x_discrete(labels = labels1) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%y-%b-%d" # Customize the date format (e.g., "23-May-04")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",
  ) +
  
  labs(y = "Richness", x = NULL)

  
  
  ggplot(ab_rich_dynamics, aes(x = sampling_date, y = richness)) + 
    facet_wrap(~ treatment ,  nrow = 1, ncol = 4, labeller = labeller(treatment = labels1)) +
    
    geom_point(aes(color = treatment), alpha = 0.5) +
    
    geom_line(group = "treatment", aes(x = sampling_date, y = mean_richness, color = treatment)) +
    
    geom_errorbar(aes(ymax = mean_richness + sd_richness, ymin = mean_richness - sd_richness,
                      color = treatment), alpha = 0.2) + 
    
    geom_point(aes(x = sampling_date, y = mean_richness, color = treatment), fill = "white", 
               shape = 21, size = 2) +
    
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "black", alpha = 0.5) + 
    
    scale_colour_manual(values = palette7) +
    
    scale_fill_manual(values = palette7) +
    
    scale_x_discrete(breaks = levels(ab_rich_dynamics$sampling_date)[c(3, 7, 11, 15, 19)]) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    
    labs(y = "Richness", x = NULL)
  
  

  # Coeficiente de variacion
  
  
  # LRR
  
  
  
  