
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)
source('code/basicFun.R')


div_ab <- read.csv("data/div_ab.csv")
biomass <- read.csv("data/biomass.csv")

div_ab <- subset(div_ab, select = -date)
div_ab <- subset(div_ab, select = -OBS)

biomass<- subset(biomass, select = -date)
biomass<- subset(biomass, select = -OBS)
biomass<- subset(biomass, select = -individidual_state)
biomass<- subset(biomass, select = -life_cycle)
biomass<- subset(biomass, select = -N_individuals)


summary(div_ab)
summary(biomass)

flora_db <- full_join(biomass, div_ab, relationship = "many-to-many", by = join_by(sampling, plot, treatment, species))
summary(flora_db)
#Hay una diferencia de 108 datapoints entre la db "biomass" y la db "flora_db". 
#Hay que revisar bien plot por plot, muestreo por muestreo, que todas las especies cuadren


View(flora_db[is.na(flora_db$abundance), ])

View(flora_db[is.na(flora_db$height), ])


flora_db$treatment <- as.factor(flora_db$treatment)


summary(flora_db)

flora_db %>% write.csv("data/flora_db.csv")
#Miro los NA's de abundancias y Heights en excel y voy mirando los plots. 


