


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
    mean_richness = mean(richness, na.rm = T),
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
    mean_biomass = mean(biomass, na.rm = T),
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


BEF_plot <- right_join(richness_plot, biomass_plot)






BEF_dynamics %>% 
  ggplot(aes(x = mean_richness)) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 1, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean Richness",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 


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
  ggplot(aes(x = mean_biomass)) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 100, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean biomass",
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


{gg_BEF_sampling <-  
ggplot(BEF_dynamics, aes(x = mean_richness, y = mean_biomass)) +
  
  facet_grid(~ treatment, labeller = labeller(treatment = labels3)) +
  
  geom_point(aes(color = treatment)) +
  
  geom_smooth(method = "lm", aes(color = treatment, fill = treatment), alpha = 0.3) +
  
  scale_colour_manual(values = palette5) +
  scale_fill_manual(values = palette5) +
  
  stat_poly_eq(
    aes(label = after_stat(paste(eq.label, rr.label, p.value.label, sep = "~~~"))),
    formula = y ~ x,
    parse = TRUE
  ) +
  
  labs(x = "Richness", y = "Biomass") + 
  theme(legend.position = "NULL")
print(gg_BEF_sampling)
#ggsave("results/Plots/protofinal/BEF_sampling.png", plot = gg_BEF_sampling, dpi = 300)
}



BEF_plot %>% 
  ggplot(aes(x = richness)) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 1, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean Richness",
    y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none") 


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
  ggplot(aes(x = biomass)) +
  facet_wrap(~ treatment) +
  geom_histogram( aes(fill = treatment), binwidth = 100, color = "black", alpha = 0.7) +
  scale_fill_manual(values =  palette5) +
  labs(
    x = "Mean biomass",
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



{gg_BEF_plot <- 
ggplot(BEF_plot, aes(x = richness, y = biomass)) +
  
  facet_grid(~ treatment, labeller = labeller(treatment = labels3)) +
  
  geom_point(aes(color = treatment), alpha = 0.6) +
  
  geom_smooth(method = "lm", aes(color = treatment, fill = treatment), alpha = 0.3) +
  
  scale_colour_manual(values = palette5) +
  scale_fill_manual(values = palette5) +
  
  stat_poly_eq(
    aes(label = after_stat(paste(eq.label, rr.label, p.value.label, sep = "~~~"))),
    formula = y ~ x,
    parse = TRUE
  ) +
  
  labs(x = "Richness", y = "Biomass")
print(gg_BEF_plot)
#ggsave("results/Plots/protofinal/BEF_plot.png", plot = gg_BEF_plot, dpi = 300)
}






#Intento de ploteo por muestreos


BEF_plot <- BEF_plot %>%
  group_by(omw_date) %>%
  mutate(min_date = min(date)) %>%
  ungroup() %>%
  mutate(omw_date = fct_reorder(omw_date, min_date)) %>% 
  select(-min_date)


treats <- unique(BEF_plot$treatment)
list_BEF <- list()

for(i in seq_along(treats)){
  
  list_BEF[[i]] <- 
  BEF_plot %>% 
    filter(treatment == treats[i]) %>% 
    ggplot(aes(x = richness, y = biomass))+
    
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
    
    labs( x = "Richness", y = "Biomass") + 
    theme(legend.position = "NULL")
  
  }


print(list_BEF[[1]])
#ggsave("results/Plots/protofinal/BEF_samplings_slopes_W.png", plot = list_BEF[[1]], dpi = 300)

print(list_BEF[[2]])
#ggsave("results/Plots/protofinal/BEF_samplings_slopes_C.png", plot = list_BEF[[2]], dpi = 300)

print(list_BEF[[3]])
#ggsave("results/Plots/protofinal/BEF_samplings_slopes_P.png", plot = list_BEF[[3]], dpi = 300)

print(list_BEF[[4]])
#ggsave("results/Plots/protofinal/BEF_samplings_slopes_WP.png", plot = list_BEF[[4]], dpi = 300)



dates <- unique(BEF_plot$omw_date)
treats <- unique(BEF_plot$treatment)

BEF_lm_data <- matrix(nrow = length(dates)*length(treats), ncol = 9)
colnames(BEF_lm_data) <- c("omw_date", "date", "treatment", "intercept", "slope",
                       "r_squared", "p_value", "n_points_lm", "shapiro_pvalue")
BEF_lm_data <- as.data.frame(BEF_lm_data)

counter <- 0

library(broom)

for (i in 1:length(dates)) {
  for(j in seq_along(treats)){
    
    
    BEF_plot_i <- BEF_plot %>% 
      filter(treatment == treats[j]) %>% 
      filter(omw_date == dates[i])
    
    if (nrow(BEF_plot_i) < 2) next 
    
    
    lm_i <- lm(biomass ~ richness, data = BEF_plot_i)
    shapiro_i <- shapiro.test(residuals(lm_i))
    
    # Extract coefficients, R^2, and p-value
    lm_i_summary <- summary(lm_i)
    lm_i_tidy <- tidy(lm_i)
    lm_i_glance <- glance(lm_i)
    
    intercept_i <- lm_i_tidy$estimate[1]
    slope_i <- lm_i_tidy$estimate[2]
    r_squared_i <- lm_i_glance$r.squared
    p_value_i <- lm_i_tidy$p.value[2]
    n_observations_i <- nrow(BEF_plot_i)
    

    counter = counter + 1
    
    BEF_lm_data$omw_date[counter] <- paste(dates[i])
    BEF_lm_data$date[counter] <- paste(unique(BEF_plot_i$date))
    BEF_lm_data$treatment[counter] <- paste(treats[j])
    BEF_lm_data$intercept[counter] <- round(intercept_i, 2)
    BEF_lm_data$slope[counter] <- round(slope_i, 2)
    BEF_lm_data$r_squared[counter] <- round(r_squared_i, 2)
    BEF_lm_data$p_value[counter] <- round(p_value_i, 4)
    BEF_lm_data$n_points_lm[counter] <- n_observations_i
    BEF_lm_data$shapiro_pvalue[counter] <- shapiro_i$p.value
    
  }
  
}


{gg_BEF_slopes <- 
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
  scale_color_manual(values = palette5) +
  scale_fill_manual(values = palette5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +  # Ajusta según tus datos
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs( x = NULL, y = "Slope of lm(biomas ~ richness)", title = "Labels: R2, p-value") + 
  theme(legend.position =  "NULL")
print(gg_BEF_slopes)
#ggsave("results/Plots/protofinal/BEF_slopes.png", plot = gg_BEF_slopes, dpi = 300)
}


#** Con 012 la tendencia es exactamente la misma
















#Comparación quadratic vs linear





# glms
linear_glm <- glm(mean_biomass ~ mean_richness, data = BEF_dynamics)
quadratic_glm <- glm(mean_biomass ~ mean_richness + I(mean_richness^2), data = BEF_dynamics)


# get stats summaries
summary(linear_glm)
summary(quadratic_glm)

# extracting the coefficients from the models
coef(linear_glm)
coef(quadratic_glm)



# define colors
colors <- c("deepskyblue3", "coral3")


datas <- list(BEF_dynamics, BEF_plot)

{i = 1
  datas[[i]] %>% 
    ggplot(aes(x = mean_richness, y = mean_biomass)) +
    
    facet_grid(~ treatment, labeller = labeller(treatment = labels3)) +
    
    # Linear model smoothing with equation
    geom_smooth(method = "glm", formula = y ~ x, color = colors[1], fill = colors[1], alpha = 0.3) +
    
    # Linear model: Equation
    stat_poly_eq(
      aes(label = after_stat(eq.label)),
      formula = y ~ x,
      parse = TRUE,
      color = colors[1],
      label.x = 0.1,  # Adjust position on x-axis
      label.y = 0.95  # Set y position high for the equation
    ) +
    
    # Linear model: R-squared
    stat_poly_eq(
      aes(label = after_stat(rr.label)),
      formula = y ~ x,
      parse = TRUE,
      color = colors[1],
      label.x = 0.1,  
      label.y = 0.9  # Slightly lower y position for R-squared
    ) +
    
    # Linear model: p-value
    stat_poly_eq(
      aes(label = after_stat(p.value.label)),
      formula = y ~ x,
      parse = TRUE,
      color = colors[1],
      label.x = 0.1,
      label.y = 0.85  # Lower y position for p-value
    ) +
    
    # Linear model: AIC value
    #annotate("text", x = 0.1, y = 0.8, label = paste("AIC =", round(AIC(linear_glm), 2)), color = colors[1], parse = FALSE, hjust = 0) +
    
    # Quadratic model smoothing with equation
    geom_smooth(method = "glm", formula = y ~ x + I(x^2), color = colors[2], fill = colors[2], alpha = 0.3) +
    
    # Quadratic model: Equation
    stat_poly_eq(
      aes(label = after_stat(eq.label)),
      formula = y ~ x + I(x^2),
      parse = TRUE,
      color = colors[2],
      label.x = 0.1,  
      label.y = 0.75  # Lower y position for quadratic equation
    ) +
    
    # Quadratic model: R-squared
    stat_poly_eq(
      aes(label = after_stat(rr.label)),
      formula = y ~ x + I(x^2),
      parse = TRUE,
      color = colors[2],
      label.x = 0.1,  
      label.y = 0.7  # Position for R-squared of quadratic model
    ) +
    
    # Quadratic model: p-value
    stat_poly_eq(
      aes(label = after_stat(p.value.label)),
      formula = y ~ x + I(x^2),
      parse = TRUE,
      color = colors[2],
      label.x = 0.1,
      label.y = 0.65  # Position for p-value of quadratic model
    ) +
    
    # Quadratic model: AIC value
    #annotate("text", x = 3, y = 0.6, label = paste("AIC =", round(AIC(quadratic_glm), 2)), color = colors[2], parse = FALSE, hjust = 0) +
    
    # Data points
    geom_point(size = 1.2, color = "grey50", alpha = 1) +
    
    # Labels and theme
    labs(x = "Richness", y = "Biomass", title = "Linear and Quadratic Model Fits") +
    scale_color_manual(values = colors) + 
    scale_fill_manual(values = colors)}






