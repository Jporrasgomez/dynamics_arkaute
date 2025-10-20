






rm(list = ls(all.names = TRUE))




library(piecewiseSEM)
library(nlme)
library(lme4)
library(tidyverse)
library(corrplot)
library(glmmTMB)
library(DHARMa)



arkaute <- read.csv("data/arkaute.csv") %>%
  mutate(
    year             = factor(year),
    date             = ymd(date),
    omw_date         = factor(omw_date),
    one_month_window = factor(one_month_window),
    sampling         = factor(sampling),
    plot             = factor(plot),
    treatment        = factor(treatment)
  ) %>%
  # drop pre-sampling and noisy first sampling for p & wp
  filter(sampling != "0") %>%
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))


arkaute_long <- arkaute %>% 
  pivot_longer(
    cols = richness:PC2,          
    names_to = "variable",      
    values_to = "value"          
  )


variables <- unique(arkaute_long$variable)
explanatory_var <- c("year", "date", "omw_date", "one_month_window", "sampling", "plot", "treatment")


data <- arkaute


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
  mutate(treat_label = "Combines") %>% 
  na.omit()

data_list <- list(data_c, data_w, data_p, data_wp)
# 1     # 2     # 3      # 4 


i = 1



mod1 = glmmTMB (
  biomass012 ~ richness + Y_zipf + PC2 + (1 | plot),
  data   = data_list[[i]],
  family = gaussian(link = "identity")
)

mod2 = glmmTMB ( 
  PC2 ~ Y_zipf + (1 | plot), 
  data = data_list[[i]], 
  family = gaussian(link = "identity")
    )

mod3 = glmmTMB ( 
  Y_zipf ~ richness + (1 | plot), 
  data = data_list[[i]],
  family = gaussian(link = "identity"),
  na.action = na.omit 
  )


global_model_glmm <- psem(
  mod1,
  mod2,
  mod3
)

print(summary(global_model_glmm))
plot(global_model_glmm)

plot(global_model_glmm, show = "unstd", title = paste0(unique(data_list[[i]]$treat_label)))



