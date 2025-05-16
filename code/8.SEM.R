

# Structural equation models
rm(list = ls(all.names = TRUE))

pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
              car, ggsignif, dunn.test, rstatix, piecewiseSEM, nlme)


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

variables <- colnames(arkaute)[8:16]
explanatory_var <- colnames(arkaute)[1:7]


#### 1. Checking normality of variables ######
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


###### 2. Choosing best transformation ###########

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

View(results)
# If we do this several times, the best transformation are ordernor for all variables but for Y_zipf,
# which is asinh tranformation
  

###### 3. Normalization of variables ###########

# Transformin:
arkaute_norm <- arkaute %>%
  mutate(
    richness = predict(orderNorm(arkaute$richness)),
    abundance = predict(orderNorm(arkaute$abundance)),
    biomass = predict(orderNorm(arkaute$biomass)),
    biomass012 = predict(orderNorm(arkaute$biomass012)),
    Y_zipf = asinh(Y_zipf),
    NMDS1 = predict(orderNorm(arkaute$NMDS1)),
    NMDS2 = predict(orderNorm(arkaute$NMDS2)),
    PC1 = predict(orderNorm(arkaute$PC1)),
    PC2 = predict(orderNorm(arkaute$PC2))
  )


#### 4. Checking normality of transformed variables ######

gglist_hist <- list()
gglist_tests <- list()

for(i in seq_along(variables)){
  
  z <- arkaute_norm %>% 
    select(explanatory_var, variables[i]) %>% 
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



arkaute_norm %>%  write.csv("data/arkaute_norm.csv")




