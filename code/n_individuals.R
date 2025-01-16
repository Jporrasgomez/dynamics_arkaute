
rm(list = ls(all.names = TRUE))
library(dplyr)
library(ggplot2)
library(broom)


nind <- read.csv("data/n_individuals.csv")
#source("code/first_script_old.R")

nind$code <- as.factor(nind$code)

nind <- select(nind, sampling, plot, code, nind_m2, abundance)
#flora_nind <-  flora %>% select(code, family) %>% distinct(code, .keep_all = TRUE)

nind <- nind %>%
  group_by(sampling, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance = sum(abundance))  ## Las especies de asteraceaes que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

#PODEMOS AÑADIR MÁS INFORMACIÓN DE NUMERO DE INDIVIDUOS!!!

flora_raw <- read.csv("data/flora_db_raw.csv")
flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2, -height, - Cb, -Cm, -Db, -Dm)

#Durante todo el muestreo, si había más de 5 individuos, no mediamos más. Por lo que, si hay menos de 5 individuos, ese número es el número de individuos por 
# metro 2 que había. Esto se ha hecho siempre, pero realmente nos interesa solo hasta el muestreo 11, ya que a partir del 12 empezamos a contar individuos. 

flora_nind <- flora_raw %>%
  filter(!sampling %in% c("12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, plot, code) %>%
  mutate(n_individuals = n())

flora_nind <- flora_nind %>%
    group_by(sampling, plot, code,abundance) %>%
  summarize(n_individuals_mean = mean(n_individuals, na.rm = T))  #corrección de asteraceae y poaceae #corrección de asteraceae y poaceae

n_individuals_old_data <- flora_nind %>%
  filter(n_individuals_mean < 5)
names(n_individuals_old_data)[names(n_individuals_old_data) == "n_individuals_mean"] <- "nind_m2"

nind <- bind_rows(nind, n_individuals_old_data)



### Regresión lineal. 


nind$code <- as.character(nind$code)

code_levels <- unique(nind$code)
gglist <- list()

nind_lm_data <- matrix(nrow = length(code_levels), ncol = 6)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_observations")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ abundance, data = nind_i)
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(nind_i)
  
  counter <- counter + 1
  
  gglist[[counter]] <- ggplot(nind_i, aes(x = abundance, y = nind_m2)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linear Relationship for", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R^2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
         "Number of observations", n_observations_i),
         x = "Abundance",
         y = "Numbers of individual per m2") +
    theme_minimal()
  
  nind_lm_data$code[counter] <- code_levels[i]
  nind_lm_data$intercept[counter] <- intercept_i
  nind_lm_data$slope[counter] <- slope_i
  nind_lm_data$r_squared[counter] <- r_squared_i
  nind_lm_data$p_value[counter] <- p_value_i
  nind_lm_data$n_observations[counter] <- n_observations_i

  
}

gglist[[6
        ]]

nind_lm_data$posneg_slope <- ifelse(nind_lm_data$slope < 0, paste("negative"), paste("positive"))
hist(nind_lm_data$slope)


#Let's check results
ggplot(nind_lm_data, aes( x = r_squared, y = p_value, 
                          label = paste(code, n_observations, sep = ", "), color = posneg_slope))+
  geom_point()+ 
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "gray40") +
  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5, 
    max.overlaps = 15# Line thickness
  )
 
#The lm model fits well for a few species: 
species_lm <- c("melu", "apar", "gedi", "rapa", "gemo", "mydi", "cegl", "trfr", "casp", "lapu", "poaceae")

nind_lm_species <- nind_lm_data %>% 
  filter(code %in% species_lm )


# What can we do with the rest of species? 

#Let's try with hoe reliable would be to use a mean value of nind_m2
species_nolm <- nind %>% 
  filter(!code %in% species_lm) %>% 
  group_by(code) %>% 
  summarize(nind_m2_mean = mean(nind_m2, na.rm = T),
            nind_m2_sd = sd(nind_m2, na.rm = T),
            n_observations = n())
species_nolm$cv <- (species_nolm$nind_m2_sd / species_nolm$nind_m2_mean)*100
species_nolm <- species_nolm %>%
  mutate(t_value = ifelse(n_observations > 1, qt(0.975, df = n_observations - 1), NA))

species_nolm <- species_nolm %>%
  mutate(
    margin_of_error = ifelse(n_observations > 1, t_value * (nind_m2_sd / sqrt(n_observations)), NA),
    ci_lower = ifelse(!is.na(margin_of_error), nind_m2_mean - margin_of_error, NA),
    ci_upper = ifelse(!is.na(margin_of_error), nind_m2_mean + margin_of_error, NA)
  )

species_nolm <- species_nolm %>%
  mutate(
    margin_of_error = ifelse(is.na(margin_of_error), Inf, margin_of_error), # Replace NA with Inf for sorting
    reliability_order = reorder(code, margin_of_error)
  )

#Just a few have a reliable mean value, but they have a very low number of observations. However, 
# For this reason (low n) the mean might be sufficient for these species


ggplot(data = species_nolm, aes(x = reliability_order, y = nind_m2_mean, label = n_observations)) +
  geom_point(size = 3, color = "blue") + # Mean values as points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") + # CIs as error bars
  labs(
    title = "Mean Abundance with Confidence Intervals (Sorted by Reliability)",
    x = "Species Code (Sorted by mean reliability)",
    y = "Mean nind/m²"
  ) +
  geom_text_repel(size = 3, min.segment.length = 0.1, segment.color = "gray", segment.size = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) # Rotate x-axis labels for better readability
  )
hist(species_nolm$cv, breaks = 20)

#For the species with low reliability of the mean and high n, we can run other models. 
# For the species with low leliability of the mean and low n... (vear and brasicaceae) we have to think what to do. 







  
print(gglist[1:length(code_levels)])

#Checking species with low info: 

gglist[[which(code_levels == "amsp")]]
gglist[[which(code_levels == "brasiccaceae")]]

#There are some species with negative slope:

gglist[[which(code_levels == "vear")]]
gglist[[which(code_levels == "cadi")]]
gglist[[which(code_levels == "rucr")]]
gglist[[which(code_levels == "kisp")]]
gglist[[which(code_levels == "veof")]]

#What do we do with these? 

# There are some intercepts that are negative. can do we do about these?
# There are some slopes that are negative. 

nind_lm_data <- nind_lm_data %>%
  mutate(slope = ifelse(is.na(slope), 0, slope))

hist(nind_lm_data$slope, 30)
hist(nind_lm_data$r_squared, 30)
hist(nind_lm_data$n_observations, 30)
hist(nind_lm_data$p_value, 30)



nind_lm_data %>% write.csv("data/nind_lm_data.csv", row.names = FALSE)


