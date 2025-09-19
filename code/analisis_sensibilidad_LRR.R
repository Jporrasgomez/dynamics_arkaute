



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak, effsize) #Cargamos los paquetes que necesitamos

source("code/palettes_labels.R")


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment)) 


d <- arkaute %>% 
  select(treatment, plot, sampling, date,  NMDS1, NMDS2) %>% 
  rename(n1 = NMDS1, 
         n2 = NMDS2) %>% 
  mutate(tn1 = n1 + abs(min(n1, na.rm = T)) + 1,
         tn2 = n2 + abs(min(n2, na.rm = T)) + 1) %>% 
  mutate(ttn1 = tn1 + abs(min(tn1, na.rm = T)) + 1,
         ttn2 = tn2 + abs(min(tn2, na.rm = T)) + 1)  %>% 
  mutate(tttn1 = ttn1 + abs(min(ttn1, na.rm = T)) + 1,
         tttn2 = ttn2 + abs(min(ttn2, na.rm = T)) + 1)



d %>% 
  ggplot(aes(x = n1, y = tn1)) + 
  geom_point()

d %>% 
  ggplot(aes(x = n2, y = tn2)) + 
  geom_point()


d %>% 
  ggplot(aes(x = n2, y = tn2)) + 
  geom_point()


min(d$n1, na.rm = T)
min(d$tn1, na.rm = T)
min(d$ttn1, na.rm = T)
min(d$tttn1, na.rm = T)


d %>% 
  ggplot(aes(x = n1, y = n2, color = treatment)) + 
  geom_point() 

d %>% 
  ggplot(aes(x = tn1, y = tn2, color = treatment)) + 
  geom_point() 

d %>% 
  ggplot(aes(x = ttn1, y = ttn2, color = treatment)) + 
  geom_point() 

d %>% 
  ggplot(aes(x = tttn1, y = tttn2, color = treatment)) + 
  geom_point()
  

d_mean1 <- d %>%
  select(-n2, -tn2, -ttn2, -tttn2, -n1) %>% 
  group_by(treatment, sampling, date) %>% 
  mutate(mean_tn1 = mean(tn1, na.rm = T), 
         mean_ttn1 = mean(ttn1, na.rm = T), 
         mean_tttn1 = mean(tttn1, na.rm = T), 
         sd_tn1 = sd(tn1, na.rm = T), 
         sd_ttn1 = sd(ttn1, na.rm = T),
         sd_tttn1 = sd(tttn1, na.rm = T)
         ) %>% 
  ungroup()


data <- d_mean1 %>%

  select(treatment, sampling, date, plot,
         starts_with("mean_"), starts_with("sd_")) %>%
  pivot_longer(
    cols = c(starts_with("mean_"), starts_with("sd_")),
    names_to = c(".value", "variable"),
    names_pattern = "^(mean|sd)_(.*)$")
 



variable = c("tn1", "ttn1", "tttn1")
gglist <- list()
RRlist <- list()


for(i in 1:3){

effect <- data %>% 
  filter(variable == variable[i]) %>% 
  select(date, sampling, treatment, mean, sd) %>% 
  distinct()

effect_c <- effect %>% 
  filter(treatment == "c") %>% 
  select(date, sampling, mean, sd) %>% 
  rename(mean_c = mean,
         sd_c = sd)



n = 4

RR_treatment <- effect %>% 
  filter(treatment != "c") %>% 
  left_join(effect_c, by = c("date", "sampling")) %>% 
  mutate(
    # Cálculo del Log Response Ratio (RR)
    RR = log(mean / mean_c),
    
    # Cálculo de la varianza de RR
    var_RR = (sd^2) / (n * mean^2) + 
      (sd_c^2) / (n * mean_c^2),
    
    se_RR = sqrt(var_RR)  # Error estándar de RR
  ) %>% 
  mutate(
    # Cálculo de delta_RR (ajuste de sesgo)
    delta_RR = RR + 0.5 * (
      (sd^2) / (n * mean^2) - 
        (sd_c^2) / (n * mean_c^2)
    ),
    
    # Varianza de delta_RR
    var_delta_RR = var_RR + 0.5 * (
      (sd^4) / (n^2 * mean^4) + 
        (sd_c^4) / (n^2 * mean_c^4)
    ),
    se_delta_RR = sqrt(var_delta_RR),  # Error estándar de delta_RR
    
  ) %>% 
  mutate(
    variable = paste0(variable[i])
  ) 
RR_treatment <- RR_treatment %>% 
  filter(! RR == "-Inf") %>% 
  rename(RR_descriptor = treatment) %>% 
  mutate(
    RR_descriptor = fct_recode(RR_descriptor,
                               "w_vs_c" = "w",
                               "p_vs_c" = "p", 
                               "wp_vs_c" = "wp"))



RRlist[[i]]<- RR_treatment

z = 1.96


gg_RR <- 
  ggplot(RR_treatment, aes(x = date, y = delta_RR)) + 
  facet_wrap(~ RR_descriptor, labeller = labeller(RR_descriptor = labels_RR)) +
  geom_errorbar(aes(ymin = delta_RR - z * se_delta_RR,
                    ymax = delta_RR + z * se_delta_RR,
                    color = RR_descriptor), alpha = 0.5) +
  geom_point(aes(color = RR_descriptor)) + 
  geom_line(aes(color = RR_descriptor)) +
  scale_color_manual(values = palette_RR_CB) +
  geom_hline( yintercept= 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(legend.position = "none") + 
  labs( y = paste0("RR_", variable[i]), x = NULL)

gglist[[i]] <- gg_RR


}

gglist[[1]]
gglist[[2]]
gglist[[3]]


RR_all <- do.call(rbind, RRlist) %>% 
  select(date, sampling, RR_descriptor, delta_RR, se_delta_RR, variable)


RR_try <- RR_all %>% 
  rename(RR = delta_RR, 
         se_RR = se_delta_RR) %>% 
  mutate(variable = as.factor(variable))


library(dplyr)
library(tidyr)

RR_wide <- RR_try %>%
  pivot_wider(
    id_cols = c(date, sampling, RR_descriptor),   # columnas que se mantienen
    names_from = variable,                        # de aquí salen los sufijos (tn1, ttn1, tttn1)
    values_from = c(RR, se_RR),                   # estas columnas se expanden
    names_glue = "{.value}_{variable}"            # construye nombres como RR_tn1, se_RR_tn1
  )


RR_wide %>% 
  ggplot(aes(x = RR_tn1, y = RR_ttn1)) + 
  geom_point()


RR_pairs <- RR_all %>%
  filter(RR_descriptor == "p_vs_c",
         variable %in% c("tn1", "ttn1")) %>%
  select(date, sampling, variable, delta_RR) %>%
  pivot_wider(names_from = variable, values_from = delta_RR) %>%
  tidyr::drop_na(tn1, ttn1)  # opcional, para quitar pares incompletos

ggplot(RR_pairs, aes(x = tn1, y = ttn1)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "LRR(tn1)", y = "LRR(tttn1)")


ggplot(RR_pairs, aes(x = exp(tn1), y = exp(tttn1))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "exp(LRR(tn1))", y = "exp(LRR(tttn1))")


