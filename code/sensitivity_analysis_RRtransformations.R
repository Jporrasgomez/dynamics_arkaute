





rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak, effsize) #Cargamos los paquetes que necesitamos

#source("code/palettes_labels.R")


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment)) 



d0 <- arkaute %>% 
  select(treatment, plot, sampling, date,  NMDS1, NMDS2) %>% 
  rename(n1 = NMDS1, 
         n2 = NMDS2) %>% 
  mutate(tn1 = n1 + abs(min(n1, na.rm = T)) + 1,
         tn2 = n2 + abs(min(n2, na.rm = T)) + 1)

min(d0$n1, na.rm = T)
min(d0$tn1, na.rm = T)


d0 %>% 
  ggplot(aes(x = n1, y = n2, color = treatment)) + 
  geom_point() 

d0 %>% 
  ggplot(aes(x = tn1, y = tn2, color = treatment)) + 
  geom_point() 




vars <- c("NMDS1", "NMDS2", "PC1", "PC2")
# k            #1        #2      #3     #4

k = 4
Q = 50
int = 0.05


{

d <- arkaute %>% 
  select(treatment, plot, sampling, date, vars[k]) %>% 
  rename(n1 = vars[k])


  
tdlist <- list()
z = 0.00
for(i in 1:Q) {
  
z = z + int

td <- d %>% 
  mutate(
    tn1 = n1 + abs(min(n1, na.rm = T)) + z, 
    z = z
  )
  
tdlist[[i]] <- td


}

td <- do.call(rbind, tdlist) %>% 
  mutate( z = as.factor(z))



d_mean <- td %>%
  select(-n1) %>% 
  group_by(treatment, sampling, date, z) %>% 
  mutate(mean = mean(tn1, na.rm = T), 
         sd = sd(tn1, na.rm = T)) %>% 
  ungroup()



zs <- unique(td$z)

RRlist <- list()


for(i in seq_along(zs)){
  
  effect <- d_mean %>% 
    filter(z == zs[i]) %>% 
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
      z = paste0(zs[i])
    ) %>% 
    filter(! RR == "-Inf") %>% 
    rename(RR_descriptor = treatment) %>% 
    mutate(
      RR_descriptor = fct_recode(RR_descriptor,
                                 "w_vs_c" = "w",
                                 "p_vs_c" = "p", 
                                 "wp_vs_c" = "wp")) %>% 
    select(date, sampling, RR_descriptor, delta_RR, se_delta_RR, z) %>% 
    mutate( IC = se_delta_RR * 1.96) %>% 
    rename( RR = delta_RR, 
            se_RR = se_delta_RR) %>% 
    select(date, sampling, RR_descriptor, RR, se_RR, IC, z)
    
    
  
  RRlist[[i]]<- RR_treatment

}

RR_all <- do.call(rbind, RRlist)

RR_all %>% 
  filter(RR_descriptor == "p_vs_c") %>% 
  ggplot(aes(x = sampling, y = RR, color = z)) + 
  geom_point()


i = 5
RR_all %>% 
  filter(RR_descriptor == "p_vs_c", 
         z == zs[i]) %>% 
  ggplot(aes(x = sampling, y = RR)) + 
  geom_point() + 
  labs( title = paste0("z = ", zs[i]))


RR_ref005 <- RR_all %>% 
  filter(z == 0.05) %>% 
  rename(RR_ref005 = RR,
         se_RR_ref005 = se_RR, 
         IC_ref005 = IC) %>% 
  select(-z)


RR_yokse <- left_join(RR_all, RR_ref005)
}


ggplot(td, aes(x = tn1, y = n1, color = z)) + 
  geom_point() + 
  labs(title = paste0(vars[k]), x = paste0("tn1 (tn1 = n1 + abs(min(n1)) + alpha)"))


RR_yokse %>% 
  ggplot(aes(x = RR, y = RR_ref005, color = z, fill = z)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = T, alpha = 0.3) + 
  geom_point() + 
  labs(title = paste0(vars[k], " - RR"))


RR_yokse %>% 
  ggplot(aes(x = IC, y = IC_ref005, color = z, fill = z)) + 
  geom_smooth(method = "lm", se = T, alpha = 0.3) + 
  geom_point() + 
  labs(title = paste0(vars[k], " - IC"))





library(dplyr)

reg_tab_RR <- RR_yokse %>%
  group_by(z) %>%
  group_modify(~{
    fit <- lm(RR_ref005 ~ RR, data = .x)
    s   <- summary(fit)
    
    # p-value global (test F)
    Fval <- unname(s$fstatistic[1]); df1 <- unname(s$fstatistic[2]); df2 <- unname(s$fstatistic[3])
    p_global <- pf(Fval, df1, df2, lower.tail = FALSE)
    
    tibble(
      intercept = unname(coef(fit)[1]),
      slope     = unname(coef(fit)[2]),
      r2        = s$r.squared,
      p_slope   = s$coefficients["RR","Pr(>|t|)"],  # p de la pendiente
      p_global  = p_global                          # p global del modelo
    )
  }) %>%
  ungroup() %>%
  mutate(z = suppressWarnings(as.numeric(z))) %>%
  arrange(z) %>%
  mutate(across(c(intercept, slope, r2, p_slope, p_global), ~round(., 6)))

reg_tab_RR




reg_tab_IC <- RR_yokse %>%
  group_by(z) %>%
  group_modify(~{
    fit <- lm(IC_ref005 ~ IC, data = .x)
    s   <- summary(fit)
    
    # p-value global (test F)
    Fval <- unname(s$fstatistic[1]); df1 <- unname(s$fstatistic[2]); df2 <- unname(s$fstatistic[3])
    p_global <- pf(Fval, df1, df2, lower.tail = FALSE)
    
    tibble(
      intercept = unname(coef(fit)[1]),
      slope     = unname(coef(fit)[2]),
      r2        = s$r.squared,
      p_slope   = s$coefficients["IC","Pr(>|t|)"],  # p de la pendiente
      p_global  = p_global                          # p global del modelo
    )
  }) %>%
  ungroup() %>%
  mutate(z = suppressWarnings(as.numeric(z))) %>%
  arrange(z) %>%
  mutate(across(c(intercept, slope, r2, p_slope, p_global), ~round(., 6)))

reg_tab_IC







