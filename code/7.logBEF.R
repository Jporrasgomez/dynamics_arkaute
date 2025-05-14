






rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, ggpmisc, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos


#Scripts 
source("code/1.first_script.R")

source("code/plots_functions_flora/plot_BEF.R")

source("code/palettes_labels.R")


richness_dynamics <- ab_rich_dynamics %>% 
  distinct(treatment, sampling, omw_date, one_month_window, date, plot, richness) %>% 
  group_by(treatment, sampling, date) %>% 
  mutate(
    n = n(),
    mean_richness = median(richness, na.rm = T),
    sd_richness = sd(richness, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(treatment, sampling, omw_date, one_month_window, date, n,
         mean_richness, sd_richness) %>% 
  distinct()



biomass_dynamics <- biomass_imp %>% 
  distinct(treatment, sampling, omw_date, one_month_window, date, biomass) %>% 
  group_by(treatment, sampling, date) %>% 
  mutate(
    n = n(),
    mean_biomass = median(biomass, na.rm = T),
    sd_biomass = sd(biomass, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(treatment, sampling, omw_date, one_month_window, date, n,
         mean_biomass, sd_biomass)%>% 
  distinct()


BEF_dynamics <- merge(richness_dynamics, biomass_dynamics)



richness_plot <- ab_rich_dynamics %>% 
  distinct(treatment, sampling, omw_date, one_month_window, date, plot, richness) 


biomass_plot <- biomass_imp %>% 
  distinct(treatment, sampling, omw_date, one_month_window, date, plot, biomass)

evenness_plot <- ab_rich_dynamics %>% 
  distinct(treatment, sampling, omw_date, one_month_window, date, plot, Y_zipf) 


BEF_plot <- right_join(richness_plot, biomass_plot)
BEF_plot <- right_join(BEF_plot, evenness_plot)



# In this script we want to adjust BEF relationships to a power equation model
# Biomass = b*(Richness^a)
# What by applying a logarithm transformation is: log(biomass) = log(b) + a*log(Richness)
# Getting a linear regression where log(b) is the intercept and a is the slope




BEF_dynamics %>% 
  ggplot(aes(x = log(mean_richness))) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 0.1, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Log (Mean Richness)",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 



BEF_dynamics %>% 
  ggplot(aes(x = log10(mean_biomass))) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 0.2, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "log(Mean biomass)",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 


{gg_logBEF_sampling <-  
    ggplot(BEF_dynamics, aes(x = log(mean_richness), y = log(mean_biomass))) +
    
    facet_grid(~ treatment, labeller = labeller(treatment = labels3), scales = "free") +
    
    geom_point(aes(color = treatment)) +
    
    geom_smooth(method = "lm", aes(color = treatment, fill = treatment), alpha = 0.3) +
    
    scale_colour_manual(values = palette5) +
    scale_fill_manual(values = palette5) +
    
    stat_poly_eq(
      aes(label = after_stat(paste(eq.label, rr.label, p.value.label, sep = "~~~"))),
      formula = y ~ x,
      parse = TRUE
    ) +
    
    labs(x = "Ln(Richness)", y = "Ln(Biomass)") + 
      
    theme(legend.position = "NULL")
    
  print(gg_logBEF_sampling)
  ggsave("results/Plots/protofinal/logBEF_sampling.png", plot = gg_logBEF_sampling, dpi = 300)
  }




BEF_plot %>% 
  ggplot(aes(x = log(richness))) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 0.1, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean Richness",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 


BEF_plot %>% 
  ggplot(aes(x = log(biomass))) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 0.1, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean biomass",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 



{gg_logBEF_plot <- 
    ggplot(BEF_plot, aes(x = log(abs(Y_zipf)), y = log(biomass))) +
    
    facet_grid(~ treatment, labeller = labeller(treatment = labels3), scales = "free") +
    
    geom_point(aes(color = treatment), alpha = 0.6) +
    
    geom_smooth(method = "lm", aes(color = treatment, fill = treatment), alpha = 0.3) +
    
    scale_colour_manual(values = palette5) +
    scale_fill_manual(values = palette5) +
    
    stat_poly_eq(
      aes(label = after_stat(paste(eq.label, rr.label, p.value.label, sep = "~~~"))),
      formula = y ~ x,
      parse = TRUE
    ) +
    
    labs(x = "Ln(Evenness)", y = "Ln(Biomass)") +
    theme(legend.position = "NULL")
  print(gg_logBEF_plot)
  #ggsave("results/Plots/protofinal/logBEF_plot.png", plot = gg_logBEF_plot, dpi = 300)
  }






#Intento de ploteo por muestreos


BEF_plot <- BEF_plot %>%
  group_by(omw_date) %>%
  mutate(min_date = min(date)) %>%
  ungroup() %>%
  mutate(omw_date = fct_reorder(omw_date, min_date)) %>% 
  select(-min_date)


treats <- unique(BEF_plot$treatment)
list_logBEF <- list()

for(i in seq_along(treats)){
  
  list_logBEF[[i]] <- 
    BEF_plot %>% 
    filter(treatment == treats[i]) %>% 
    ggplot(aes(x = log(richness), y = log(biomass)))+
    
    facet_wrap(~ omw_date, nrow = 4, ncol = 4) +
    
    geom_point(aes(color = treatment)) +
    
    geom_smooth(method = "lm", aes( color = treatment, fill = treatment), alpha = 0.4) +
    
    # Linear model: Equation
    stat_poly_eq(
      aes(label = after_stat(eq.label)),
      formula = y ~ x,
      parse = TRUE,
      color = "black",
      label.x = 0.1,  # Adjust position on x-axis
      label.y = 0.95  # Set y position high for the equation
    ) +
    
    # Linear model: R-squared
    stat_poly_eq(
      aes(label = after_stat(rr.label)),
      formula = y ~ x,
      parse = TRUE,
      color = "black",
      label.x = 0.1,  
      label.y = 0.85  # Slightly lower y position for R-squared
    ) +
    
    # Linear model: p-value
    stat_poly_eq(
      aes(label = after_stat(p.value.label)),
      formula = y ~ x,
      parse = TRUE,
      color = "black",
      label.x = 0.1,
      label.y = 0.7  # Lower y position for p-value
    ) + 
    
    scale_colour_manual(values = palette5) +
    
    scale_fill_manual(values =  palette5) +
    
    labs( x = "Ln(Richness)", y = "Ln(Biomass)") + 
    theme(legend.position = "NULL")
  
}


print(list_BEF[[1]])
ggsave("results/Plots/protofinal/logBEF_samplings_slopes_W.png", plot = list_logBEF[[1]], dpi = 300)

print(list_BEF[[2]])
ggsave("results/Plots/protofinal/logBEF_samplings_slopes_C.png", plot = list_logBEF[[2]], dpi = 300)

print(list_BEF[[3]])
ggsave("results/Plots/protofinal/logBEF_samplings_slopes_P.png", plot = list_logBEF[[3]], dpi = 300)

print(list_BEF[[4]])
ggsave("results/Plots/protofinal/logBEF_samplings_slopes_WP.png", plot = list_logBEF[[4]], dpi = 300)



dates <- unique(BEF_plot$omw_date)
treats <- unique(BEF_plot$treatment)

BEF_lm_data <- matrix(nrow = length(dates)*length(treats), ncol = 9)
colnames(BEF_lm_data) <- c("omw_date", "date", "treatment", "intercept", "slope",
                           "r_squared", "p_value", "n_points_lm", "shapiro_pvalue")
BEF_lm_data <- as.data.frame(BEF_lm_data)
library(broom)

counter <- 0
BEF_lm_data[] <- NA  # limpia el data.frame por si lo reutilizas

for (i in seq_along(dates)) {
  for (j in seq_along(treats)) {
    
    BEF_plot_i <- BEF_plot %>% 
      filter(treatment == treats[j], omw_date == dates[i]) %>% 
      filter(richness > 0, biomass > 0) %>%  # importante: evita log(0) o log(valores negativos)
      filter(complete.cases(richness, biomass))  # por si hay NA
    
    if (nrow(BEF_plot_i) < 2) next  # si no hay suficientes puntos, salta
    
    # ajusta modelo log-log
    lm_i <- try(lm(log(biomass) ~ log(richness), data = BEF_plot_i), silent = TRUE)
    
    if (inherits(lm_i, "try-error")) next  # evita romper el bucle si lm falla
    
    shapiro_i <- shapiro.test(residuals(lm_i))
    
    # Extraer estadísticas del modelo
    lm_i_summary <- summary(lm_i)
    lm_i_tidy <- tidy(lm_i)
    lm_i_glance <- glance(lm_i)
    
    intercept_i <- lm_i_tidy$estimate[1]
    slope_i <- lm_i_tidy$estimate[2]
    r_squared_i <- lm_i_glance$r.squared
    p_value_i <- lm_i_tidy$p.value[2]
    n_observations_i <- nrow(BEF_plot_i)
    
    counter <- counter + 1
    
    BEF_lm_data$omw_date[counter] <- as.character(dates[i])
    BEF_lm_data$date[counter] <- as.character(min(BEF_plot_i$date, na.rm = TRUE))
    BEF_lm_data$treatment[counter] <- as.character(treats[j])
    BEF_lm_data$intercept[counter] <- round(intercept_i, 2)
    BEF_lm_data$slope[counter] <- round(slope_i, 2)
    BEF_lm_data$r_squared[counter] <- round(r_squared_i, 2)
    BEF_lm_data$p_value[counter] <- round(p_value_i, 4)
    BEF_lm_data$n_points_lm[counter] <- n_observations_i
    BEF_lm_data$shapiro_pvalue[counter] <- round(shapiro_i$p.value, 4)
    
    
    
  }
}



{gg_logBEF_slopes <- 
    BEF_lm_data %>% 
    mutate(date = ymd(date)) %>% 
    filter(!is.na(slope)) %>% 
    ggplot(aes(x = date, y = slope, label = paste(r_squared, p_value, sep = ", "), color = treatment))+
    #ggplot(aes(x = date, y = slope, color = treatment))+
    facet_wrap(~ treatment, labeller = labeller(treatment = labels3)) + 
    geom_point() +
    #geom_path( group = 1, alpha = 0.4) +
    geom_smooth(method = "lm", aes(color = treatment, fill = treatment), alpha = 0.1) + 
    geom_text_repel(
      size = 3,                # Text size
      min.segment.length = 0.1,  # Ensures lines are always drawn
      segment.color = "gray50",# Line color
      segment.size = 0.5, 
      max.overlaps = 15# Line thickness
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) + 
    scale_color_manual(values = palette5) +
    scale_fill_manual(values = palette5) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +  # Ajusta según tus datos
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    labs( x = NULL, y = "Slope of BEF", subtitle = "Labels: R2, p-value",
          title = "Slope of lm(Ln(biomass) ~ Ln(richness)") + 
    theme(legend.position =  "NULL")
  print(gg_logBEF_slopes)
  ggsave("results/Plots/protofinal/logBEF_slopes.png", plot = gg_logBEF_slopes, dpi = 300)
}


#** Con 012 la tendencia es exactamente la misma





