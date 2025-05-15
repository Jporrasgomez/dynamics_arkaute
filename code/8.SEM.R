

# Structural equation models
rm(list = ls(all.names = TRUE))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
              car, ggsignif, dunn.test, rstatix, piecewiseSEM, nlme)

arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))

variables <- colnames(arkaute)[8:16]
explanatory_var <- colnames(arkaute)[1:7]

source("code/meta_function/stats_function.R")

gglist_hist <- list()
gglist_tests <- list()

for(i in seq_along(variables)){
  
  z <- arkaute %>% 
    select(explanatory_var, variables[i]) %>% 
    rename(value = variables[i]) %>% 
    mutate(variable = variables[i]) %>% 
    mutate(value = value)
  
  stats(z, "value", "treatment")
  gglist_hist[[i]] <- gg_stats
  gglist_tests[[i]] <- gg_normality_tests
  
  
}

i = 1
gglist_hist[[i]] 
gglist_tests[[i]]


# No variable follows a normal distribution
# Which transformations should we use? 


library(bestNormalize) # Automatic best normalization

results <- matrix(ncol = 5, nrow = length(variables))
colnames(results) <- c("variable", "n", "shapiro_pre","transformation", "shapiro_post")
results <- as.data.frame(results)

for(i in seq_along(variables)){
  
  results$variable[i] <- variables[i]
  results$shapiro_pre[i] <- shapiro.test(arkaute[[variables[i]]])$p.value
 
  x_clean <- na.omit(arkaute[[variables[i]]])
  results$n[i] <- length(x_clean)
  
  bn <- bestNormalize(x_clean)
  x_best <- predict(bn)
  results$shapiro_post[i] <- shapiro.test(x_best)$p.value
  
  print(variables[i])
  bn$chosen_transform
  results$transformation[i] <- class(bn$chosen_transform)[1]
  
}








