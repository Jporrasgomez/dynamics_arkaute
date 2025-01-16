



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, broom)

flora_rare <- read.csv("data/flora_rare.csv")

flora_012 <- flora_rare %>% 
  filter(sampling %in% c("0", "1", "2"))%>% 
  select(sampling, plot, treatment, date, code, Ah, Ab, height)


codes_012 <- unique(flora_012$code)
codes_012


flora_agab <- flora_rare %>% 
  filter(sampling %in% c("3", "4", "5", "6", "7", "8", "9", "10", "11", "13", "14", "15", 
                         "16", "17", "18", "19", "20", "21")) %>% 
  filter(code %in% codes_012) %>% 
  select(sampling, plot, treatment, date, code, Ah, Ab, height)

flora_all <- flora_rare %>% 
  filter(sampling %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "13", "14", "15", 
                         "16", "17", "18", "19", "20", "21")) %>% 
  filter(code %in% codes_012) %>% 
  select(sampling, plot, treatment, date, code, Ah, Ab, height)
