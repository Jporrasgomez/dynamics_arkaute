

# Structural equation models
rm(list = ls(all.names = TRUE))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
              car, ggsignif, dunn.test, rstatix)


theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))

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


arkaute_long <- arkaute %>% 
  pivot_longer(
    cols = richness:mean_vwc,          
    names_to = "variable",      
    values_to = "value"          
  )


 ######################  NORMALIZATION AT TREATMENT LEVEL ################################
#### 1. Checking normality of variables ######
source("code/functions/stats_function.R")

variables <- unique(arkaute_long$variable)
explanatory_var <- c("year", "date", "omw_date", "one_month_window", "sampling", "plot", "treatment")


gglist_hist <- list()
gglist_tests <- list()
normality_list1 <- list()

for(i in seq_along(variables)){
  z <- arkaute_long %>% 
   filter(variable == variables[i])
  
  stats(z, "value", "treatment")
  gglist_hist[[i]] <- gg_stats
  gglist_tests[[i]] <- gg_normality_tests
  
  normality_list1[[i]] <- normality_df
  
}


do.call(rbind, normality_list1) %>% 
  ggplot(aes(y = variable, x = p_value)) +
  facet_grid(~ normality_test) + 
  geom_point() + 
  geom_vline(xintercept = 0.05, color = "red", linetype = "dashed")


# No variable follows a normal distribution



########################################################################################
                          #  GENERAL NORMALIZATION AT VARIABLE LEVEL   #

# 1. Checking which transformation fits the variable to a normal distribution

library(bestNormalize)

nloops = 15
results_all <- matrix(ncol = 7, nrow = length(variables) * nloops)
colnames(results_all) <- c("variable", "treatment", "n", "shapiro_pre",
                       "transformation", "shapiro_post", "n_transformation")
results_all <- as.data.frame(results_all, stringsAsFactors = FALSE)

counter <- 0

  for (i in seq_along(variables)) {
    
    # Filtrar los datos
    z <- arkaute_long %>%
      filter(variable == variables[i])
    
    x_clean <- na.omit(z$value)
    n_samples <- length(x_clean)
    
    # Calcular una única vez el p-valor original
    shapiro_pre_pval <- tryCatch(shapiro.test(x_clean)$p.value, error = function(e) NA)
    
    for (k in 1:nloops) {
      counter <- counter + 1
      
      # Normalización
      bn <- bestNormalize(x_clean)
      x_best <- predict(bn)
      shapiro_post_pval <- tryCatch(shapiro.test(x_best)$p.value, error = function(e) NA)
      
      # Guardar resultados
      results_all[counter, ] <- list(
        variable = variables[i],
        treatment = paste0(unique(z$treatment)),
        n = n_samples,
        shapiro_pre = shapiro_pre_pval,
        transformation = class(bn$chosen_transform)[1],
        shapiro_post = shapiro_post_pval,
        n_transformation = k
      )
    }
    
  }


#Checking shapiro-test. If there some "FALSE" it means any of the iterations found a feasible transformation
# to get normality
results_all %>%
  group_by(variable) %>%
  summarize(
    any_above_0_05 = any(as.numeric(shapiro_post) > 0.05, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  print()

possible_transformations <- results_all %>%
  mutate(shapiro_post = as.numeric(shapiro_post)) %>%
  group_by(variable, treatment) %>%
  slice_max(shapiro_post, n = 1, with_ties = FALSE) %>%  
  ungroup()

# 2. Transforming variables according to possible_transformations

arkaute_norm_all <- arkaute %>%
  mutate(
    richness         = predict(orderNorm(richness)),
    abundance        = predict(orderNorm(abundance)),
    biomass          = predict(orderNorm(biomass)),
    biomass012       = predict(orderNorm(biomass012)),
    biomass_lm_plot  = predict(orderNorm(biomass_lm_plot)),
    Y_zipf           = asinh(Y_zipf),  
    NMDS1            = predict(orderNorm(NMDS1)),
    NMDS2            = predict(orderNorm(NMDS2)),
    PC1              = predict(orderNorm(PC1)),
    PC2              = predict(orderNorm(PC2)),
    mean_temperature = predict(orderNorm(mean_temperature)),
    mean_vwc         = predict(orderNorm(mean_vwc)),
    SLA              = predict(orderNorm(SLA)),
    LDMC             = predict(orderNorm(LDMC)),
    leafN            = predict(orderNorm(leafN)),
  
  )
 


# 3. Checking normality

arkaute_norm_all_long <- arkaute_norm_all %>% 
  pivot_longer(
    cols = richness:mean_vwc,          
    names_to = "variable",      
    values_to = "value"          
  )

gglist_hist <- list()
gglist_tests <- list()
normality_list <- list()

for(i in seq_along(variables)){
  z <- arkaute_norm_all_long %>% 
    filter(variable == variables[i])
  
  stats(z, "value", "treatment")
  gglist_hist[[i]] <- gg_stats
  gglist_tests[[i]] <- gg_normality_tests
  
  normality_list[[i]] <- normality_df
  
}

i = 12
gglist_hist[[i]] 


do.call(rbind, normality_list) %>% 
  ggplot(aes(y = variable, x = p_value)) +
  facet_grid(~ normality_test) + 
  geom_point() + 
  geom_vline(xintercept = 0.05, color = "red", linetype = "dashed")


arkaute_norm_all %>%  write.csv("data/arkaute_norm_all.csv", row.names = F)

########################################################################################
                              #  NORMALIZATION AT TREATMENT LEVEL #

# Normality at treatment level?

treats <- unique(arkaute$treatment)

normality_list_treatment <- list()

normality_treat_df <- matrix(ncol = 3, nrow = length(treats) * length(variables))
colnames(normality_treat_df) <- c("variable", "treatment", "shapiro_p_value")
normality_treat_df <- as.data.frame(normality_treat_df)

counter = 0

for(j in seq_along(treats)){
  for (i in seq_along(variables)){
    
    counter = counter + 1
    
    z <- arkaute_long %>% 
      filter(treatment == treats[j]) %>% 
      filter(variable == variables[i])
    
    shapiro_result <- shapiro.test(z$value)
    
    normality_treat_df$shapiro_p_value[counter] <- shapiro_result$p.value
    normality_treat_df$variable[counter]  <- variables[i]
    normality_treat_df$treatment[counter]  <- paste0(unique(z$treatment))
   
    
  }
  
}

normality_treat_df %>% 
  ggplot(aes(x = shapiro_p_value, y = variable)) +
  facet_grid(~ treatment) + 
  geom_point() + 
  geom_vline(xintercept = 0.05, color = "red", linetype = "dashed")

# Some variables are normal for some treatments. 


# Which transformations should we use? 


###### 2. Choosing best transformation ###########

library(bestNormalize) # Automatic best normalization

# This takes a while: 

results <- matrix(ncol = 7, nrow = length(variables) * length(treats) * 10)
colnames(results) <- c("variable", "treatment", "n", "shapiro_pre",
                       "transformation", "shapiro_post", "n_transformation")
results <- as.data.frame(results, stringsAsFactors = FALSE)

counter <- 0

for (j in seq_along(treats)) {
  for (i in seq_along(variables)) {
    
    # Filtrar los datos
    z <- arkaute_long %>%
      filter(variable == variables[i],
             treatment == treats[j])
    
    x_clean <- na.omit(z$value)
    n_samples <- length(x_clean)
    
    # Calcular una única vez el p-valor original
    shapiro_pre_pval <- tryCatch(shapiro.test(x_clean)$p.value, error = function(e) NA)
    
    for (k in 1:15) {
      counter <- counter + 1
      
      # Normalización
      bn <- bestNormalize(x_clean)
      x_best <- predict(bn)
      shapiro_post_pval <- tryCatch(shapiro.test(x_best)$p.value, error = function(e) NA)
      
      # Guardar resultados
      results[counter, ] <- list(
        variable = variables[i],
        treatment = paste0(unique(z$treatment)),
        n = n_samples,
        shapiro_pre = shapiro_pre_pval,
        transformation = class(bn$chosen_transform)[1],
        shapiro_post = shapiro_post_pval,
        n_transformation = k
      )
    }
    
    print(paste("Done:", variables[i], "in", treats[j]))
  }
}


shapiro_check <- results %>%
  group_by(variable, treatment) %>%
  summarize(
    any_above_0_05 = any(as.numeric(shapiro_post) > 0.05, na.rm = TRUE),
    .groups = "drop"
  )

possible_transformations <- results %>%
  mutate(shapiro_post = as.numeric(shapiro_post)) %>%
  group_by(variable, treatment) %>%
  slice_max(shapiro_post, n = 1, with_ties = FALSE) %>%  
  ungroup()
##
##
##  

###### 3. Normalization of variables ###########

# Transforming by following possible_transformations:

arkaute_norm_c <- arkaute %>%
  filter(treatment == "c") %>% 
  mutate(
    richness = predict(orderNorm(richness)),
    abundance = predict(orderNorm(abundance)),
    biomass = predict(orderNorm(biomass)),
    biomass012 = predict(orderNorm(biomass012)),
    biomass_lm_plot = predict(orderNorm(biomass_lm_plot)),
    Y_zipf = predict(yeojohnson(Y_zipf)),
    NMDS1 = predict(orderNorm(NMDS1)),
    NMDS2 = predict(orderNorm(NMDS2)),
    PC1 = predict(orderNorm(PC1)),
    PC2 = predict(orderNorm(PC2)),
    mean_temperature = predict(orderNorm(mean_temperature)),
    mean_vwc         = predict(orderNorm(mean_vwc)), 
    SLA  = predict(orderNorm(SLA)), 
    LDMC = predict(yeojohnson(LDMC)), 
    leafN = predict(orderNorm(leafN))
  )


arkaute_norm_w <- arkaute %>%
  filter(treatment == "w") %>% 
  mutate(
    richness = predict(orderNorm(richness)),  ## Original was boxcox
    abundance = predict(orderNorm(abundance)),
    biomass = predict(boxcox(biomass)),
    biomass012 = predict(boxcox(biomass012)),
    biomass_lm_plot = predict(orderNorm(biomass_lm_plot)),
    Y_zipf = predict(yeojohnson(Y_zipf)),
    NMDS1 = predict(orderNorm(NMDS1)),
    NMDS2 = predict(orderNorm(NMDS2)),
    PC1 = predict(orderNorm(PC1)),
    PC2 = predict(orderNorm(PC2)),
    mean_temperature = predict(orderNorm(mean_temperature)),
    mean_vwc         = predict(orderNorm(mean_vwc)),
    SLA  = predict(orderNorm(SLA)),
    LDMC = predict(orderNorm(LDMC)), 
    leafN = predict(orderNorm(leafN))
  )



arkaute_norm_p <- arkaute %>%
  filter(treatment == "p") %>% 
  mutate(
    richness = predict(orderNorm(richness)),
    abundance = predict(orderNorm(abundance)),
    biomass = predict(orderNorm(biomass)),   # Original transformation was yeojohnson but it was not working
    biomass012 = predict(orderNorm(biomass012)),
    biomass_lm_plot = predict(orderNorm(biomass_lm_plot)),
    Y_zipf = predict(orderNorm(Y_zipf)),
    NMDS1 = predict(orderNorm(NMDS1)),
    NMDS2 = predict(orderNorm(NMDS2)),
    PC1 = predict(orderNorm(PC1)),
    PC2 = predict(orderNorm(PC2)),
    mean_temperature = predict(orderNorm(mean_temperature)),
    mean_vwc         = predict(orderNorm(mean_vwc)), # Original transformation was yeojohnson but it was not working
    SLA  = predict(boxcox(SLA)),
    LDMC = predict(yeojohnson(LDMC)), 
    leafN = predict(yeojohnson(leafN)),
    leafN = predict(yeojohnson(leafN))
  )



arkaute_norm_wp <- arkaute %>%
  filter(treatment == "wp") %>% 
  mutate(
    richness = predict(orderNorm(richness)),
    abundance = predict(orderNorm(abundance)),
    biomass = predict(orderNorm(biomass)),
    biomass012 = predict(orderNorm(biomass012)),
    biomass_lm_plot = predict(orderNorm(biomass_lm_plot)),
    Y_zipf = predict(orderNorm(Y_zipf)),
    NMDS1 = predict(orderNorm(NMDS1)), #### Original was sqrt
    NMDS2 = predict(orderNorm(NMDS2)),
    PC1 = predict(orderNorm(PC1)),
    PC2 = predict(orderNorm(PC2)),
    mean_temperature = predict(orderNorm(mean_temperature)),
    mean_vwc         = predict(orderNorm(mean_vwc)), 
    SLA  = predict(orderNorm(SLA)),
    LDMC = predict(orderNorm(LDMC)), 
    leafN = predict(orderNorm(leafN))
  )


arkaute_norm <- do.call(rbind, list(arkaute_norm_c, arkaute_norm_p, arkaute_norm_w, arkaute_norm_wp))


#### 4. Checking normality of transformed variables ######

gglist_hist <- list()
gglist_tests <- list()

for(i in seq_along(variables)){
  
  z <- arkaute_norm %>% 
    select(treatment, plot, sampling, variables[i]) %>% 
    rename(value = variables[i]) %>% 
    mutate(variable = variables[i]) %>% 
    mutate(value = value)
  
  stats(z, "value", "treatment")
  gglist_hist[[i]] <- gg_stats
  gglist_tests[[i]] <- gg_normality_tests
  
  
}

i = 5
gglist_hist[[i]] 
gglist_tests[[i]]



normality_treat_df <- matrix(ncol = 3, nrow = length(treats) * length(variables))
colnames(normality_treat_df) <- c("variable", "treatment", "shapiro_p_value")
normality_treat_df <- as.data.frame(normality_treat_df)

counter = 0

for(j in seq_along(treats)){
  for (i in seq_along(variables)){
    
    counter = counter + 1
    
    z <- arkaute_norm %>% 
      filter(treatment == treats[j]) %>% 
      select(all_of(explanatory_var), variables[i]) %>% 
      rename(value = variables[i]) %>% 
      mutate(variable = variables[i]) %>% 
      mutate(value = value)
    
    shapiro_result <- shapiro.test(z$value)
    
    normality_treat_df$shapiro_p_value[counter] <- shapiro_result$p.value
    normality_treat_df$variable[counter]  <- variables[i]
    normality_treat_df$treatment[counter]  <- paste0(unique(z$treatment))
    
    
  }
  
}

normality_treat_df %>% 
  ggplot(aes(x = shapiro_p_value, y = variable)) +
  facet_grid(~ treatment) + 
  geom_point() + 
  geom_vline(xintercept = 0.05, color = "red", linetype = "dashed") +
  labs(x = "Shapiro p-value after transformation")


arkaute_norm %>%  write.csv("data/arkaute_norm_treatment.csv", row.names = F)

