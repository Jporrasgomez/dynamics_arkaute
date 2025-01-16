

rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, ggpmisc, gridExtra, MetBrewer) #Cargamos los paquetes que necesitamos


#Scripts 
source("code/first_script.R")

source("code/plots_functions_flora/plot_BEF.R")

ggBEF <- plot_BEF(flora)
ggBEF_noOutliers <- plot_BEF(flora1)
ggBEF_noextOutliers <- plot_BEF(flora3) #Same result as no outliers



#Comparación quadratic vs linear

# glms
linear_glm <- glm(biomass_total ~ richness, data = flora1)
quadratic_glm <- glm(biomass_total ~ richness + I(richness^2), data = flora1)

# get stats summaries
summary(linear_glm)
summary(quadratic_glm)

# extracting the coefficients from the models
coef(linear_glm)
coef(quadratic_glm)

# define ggplot theme
theme_set(theme_bw() +
            theme(legend.position = "NULL",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

# define colors
colors <- c("deepskyblue3", "coral3")




ggplot(flora1, aes(x = richness, y = biomass_total)) +
  
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
  geom_point(size = 1.2, color = "grey50", alpha = 0.09) +
  
  # Labels and theme
  labs(x = "Richness", y = "Biomass Total", title = "Linear and Quadratic Model Fits") +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors)



#Dividiendo por tratamientos


ggplot(flora1, aes(x = richness, y = biomass_total)) +
  
  # Facet by treatment if needed
  facet_grid(~ treatment) +
  
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
  #annotate("text", x = 0.1, y = 0.6, label = paste("AIC =", round(AIC(quadratic_glm), 2)), color = colors[2], parse = FALSE, hjust = 0) +
  
  # Data points
  geom_point(size = 1.2, color = "grey50", alpha = 0.09) +
  
  # Labels and theme
  labs(x = "Richness", y = "Biomass Total", title = "Linear and Quadratic Model Fits") +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors)


#Intento de ploteo por muestreos

ab_rich_dynamics <-  flora1 %>%
  group_by(omw_date, date, month, treatment, plot) %>%
  reframe(richness = richness,  #total number of species per plot
          abundance = abundance_total) %>% # total coverage of plot
  distinct(omw_date, date, month, plot, treatment, richness, abundance)



mean_sd_abrich_dynamics<- ab_rich_dynamics %>%
  group_by(treatment, omw_date) %>%
  summarize(mean_richness = mean(richness),
            sd_richness = sd(richness),
            mean_abundance = mean(abundance),
            sd_abundance = sd(abundance))

# Database for biomass

biomass_dynamics <- flora1 %>%
  #filter(!omw_date %in% c("0", "1"))  %>%
  rename(biomass = biomass_total) %>%
  group_by(omw_date, date, month, treatment, plot) %>%
  reframe(biomass = biomass) %>% # total coverage of plot
  distinct(omw_date, date, month, plot, treatment, biomass)

mean_sd_biomass_dynamics<- biomass_dynamics %>%
  group_by(treatment, omw_date) %>%
  summarize(mean_biomass = mean(biomass),
            sd_biomass = sd(biomass))


BEF_dynamics <- merge(ab_rich_dynamics, biomass_dynamics)

BEF_dynamics_c <- subset(BEF_dynamics, treatment == "c")
BEF_dynamics_w <- subset(BEF_dynamics, treatment == "w")
BEF_dynamics_p <- subset(BEF_dynamics, treatment == "p")
BEF_dynamics_wp <- subset(BEF_dynamics, treatment == "wp")


ggplot(BEF_dynamics_c, aes(x = richness, y = biomass))+
  
  facet_wrap(~ omw_date, nrow = 4, ncol = 4) +
  
  geom_point(aes(color = treatment)) +
  
  geom_smooth(method = "lm", aes( color = treatment, fill = treatment))+
  
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  scale_fill_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  

  labs( x = "Richness", y = "Biomass")


ggplot(BEF_dynamics_w, aes(x = richness, y = biomass))+
  
  facet_wrap(~ omw_date, nrow = 5, ncol = 4) +
  
  geom_point(aes(color = treatment)) +
  
  geom_smooth(method = "lm", aes( color = treatment, fill = treatment))+
  
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  scale_fill_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  
  labs( x = "Richness", y = "Biomass")

ggplot(BEF_dynamics_p, aes(x = richness, y = biomass))+
  
  facet_wrap(~ omw_date, nrow = 5, ncol = 4) +
  
  geom_point(aes(color = treatment)) +
  
  geom_smooth(method = "lm", aes( color = treatment, fill = treatment))+
  
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  scale_fill_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  
  labs( x = "Richness", y = "Biomass")

ggplot(BEF_dynamics_wp, aes(x = richness, y = biomass))+
  
  facet_wrap(~ omw_date, nrow = 5, ncol = 4) +
  
  geom_point(aes(color = treatment)) +
  
  geom_smooth(method = "lm", aes( color = treatment, fill = treatment))+
  
  scale_colour_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  scale_fill_manual(values = c("c" = "green4", "p" = "blue4", "w" = "red3", "wp" = "purple2")) +
  
  
  labs( x = "Richness", y = "Biomass")
