

# To do's
# multigroups function 

##  data(meadows)
##  
##  jutila <- psem(
##    lm(rich ~ elev + mass, data = meadows),
##    lm(mass ~ elev, data = meadows)
##  )
##  
##  jutila.multigroup <- multigroup(jutila, group = "grazed")
##  
##  jutila.multigroup
##  






rm(list = ls(all.names = TRUE))




library(piecewiseSEM)
library(nlme)
library(lme4)
library(tidyverse)
library(corrplot)


arkaute_norm <- read.csv("data/arkaute_norm.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))

arkaute_long <- arkaute_norm %>% 
  pivot_longer(
    cols = richness:PC2,          
    names_to = "variable",      
    values_to = "value"          
  )


variables <- unique(arkaute_long$variable)
explanatory_var <- c("year", "date", "omw_date", "one_month_window", "sampling", "plot", "treatment")


data <- arkaute_norm


data_c <- data %>% 
  filter(treatment == "c") %>% 
  as.data.frame() %>% 
  mutate(treat_label = "Control") %>% 
  na.omit()

data_p <- data %>% 
  filter(treatment == "p")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Perturbed") %>% 
  na.omit()

data_w <- data %>% 
  filter(treatment == "w")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Warming") %>% 
  na.omit()

data_wp <- data %>% 
  filter(treatment == "wp")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Global Change") %>% 
  na.omit()

data_list <- list(data_c, data_w, data_p, data_wp)
                    # 1     # 2     # 3      # 4 

# First data exploration: correlations, multi-collinearities






{i = 3

pairs(data_list[[i]][,variables])

cor(data_list[[i]][,variables], use="pairwise.complete.obs")

corrplot(cor(data_list[[i]][,variables], use="pairwise.complete.obs"), method = "number")}




# Model1: NMDS1, PC1 and abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1 + abundance, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


# Model2: NMDS2, PC2 and abundance

{
mod1 = lme(biomass ~ richness + Y_zipf + PC2 + abundance, random = ~ 1 | plot,  data_list[[i]])
mod2 = lme(PC2 ~ richness + NMDS2, random = ~ 1 | plot, data_list[[i]])
mod3 = lme(NMDS2 ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
mod4 = lme(Y_zipf ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])

global_model <- psem(
  mod1,
  mod2,
  mod3,
  mod4
)

summary(global_model)

plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model3: NMDS1, PC2 and  abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model4: NMDS1, PC1 and no abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model5: NMDS2, PC2 and no abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + NMDS2, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS2 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}






