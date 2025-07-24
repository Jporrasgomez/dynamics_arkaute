



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, gridExtra,
               car, ggsignif, dunn.test, rstatix, ggbreak, effsize) #Cargamos los paquetes que necesitamos


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))

arkaute_no0 <- arkaute %>% 
  filter(sampling != "0")


source("code/palettes_labels.R")

palette <- palette_CB
labels <- labels3


variables <- c("richness", "abundance", "Y_zipf", "biomass", "biomass012", "NMDS1", "NMDS2", "PC1", "PC2")


# 1) dejar estas lineas de código como función a parte
# 2) hacer un condicional: si es NMDS1, NMDS2, PC1 o PC2, hacer Hedges's. Si no, hacer LRR. 



