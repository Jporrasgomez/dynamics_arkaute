#load packages
library(tidyverse)
library(vegan)
library(ggpubr)

#load database
source("code/1.first_script.R")
#subset df sampling (do not run if the whole database is needed)
#flora <- flora[which(flora$sampling == "sampling 2"),]

#reshape df (species as columns, treatments as rows)
flora_rad <- flora_abrich %>% select(plot,sampling, treatment, code, abundance)

#We've detected that "radfit" does not like decimals. THe only decimal numbers that we have in the
#database are between 0 and 1. So we round these numbers to 1. 
flora_rad <- mutate(flora_rad, abundance = ifelse(abundance < 1, 1, abundance))


# Which model we are going to use for the entire dataset? #########

#We have to delete sampling = 1 because there are no species at treatment p and wp. If i delete only s = 1 for t = p and wp
# and I leave t= c and w, the loop doesn't work

flora_no1 <- flora_rad[flora_rad$sampling != "1", ]
#Sampling 1 gives problems since there are no species for p and wp

### Comprobación del AIC para cada valor ###########

rad_dfplot <- matrix(nrow = (length(unique(flora_no1$sampling))*length(unique(flora_no1$plot))), ncol = 6)
colnames(rad_dfplot) <- c("sampling", "plot", "AIC_pree", "AIC_log", "AIC_zipf", "AIC_man")
rad_dfplot <-  as.data.frame(rad_dfplot)

count <- 0
for (i in 1:length(unique(flora_no1$sampling))){
  for (j in 1:length(unique(flora_no1$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_no1, sampling == unique(flora_no1$sampling)[i] & plot == unique(flora_no1$plot)[j])
    subrad <- summarise(group_by(subset_data, code),
                        abundance = round(mean(abundance), 0))
    subrad <- pivot_wider(subrad, names_from = species, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rad_sub <- radfit(subrad)
    
    rad_dfplot$sampling[count] <- unique(flora_no1$sampling)[i]
    rad_dfplot$plot[count] <- unique(flora_no1$plot)[j]
    rad_dfplot$AIC_pree[count] <- rad_sub$models$Preemption$aic
    rad_dfplot$AIC_log[count] <- rad_sub$models$Lognormal$aic
    rad_dfplot$AIC_zipf[count] <- rad_sub$models$Zipf$aic
    rad_dfplot$AIC_man[count] <- rad_sub$models$Mandelbrot$aic
    
  }
}

rad_dfplot <- pivot_longer(rad_dfplot, cols = c("AIC_pree", "AIC_log","AIC_zipf","AIC_man"), 
                           names_to = "model", values_to = "AIC")
ggplot(rad_dfplot, aes(x = model, y = AIC))+
  geom_boxplot()

# No differences. We decide to use zipf because it only has one explanatory coefficient of the curve (gamma)

# Applying Zipf and lognormal to the dataset: ############

radcoeff_df <- matrix(nrow = (length(unique(flora_no1$sampling))*length(unique(flora_no1$plot))), ncol = 5)
colnames(radcoeff_df) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_df <- as.data.frame(radcoeff_df)

count = 0
for(i in 1:length(unique(flora_no1$sampling))){
  for(j in 1:length(unique(flora_no1$plot))){
    
    count <- count + 1
    subset_data <- subset(flora_no1, sampling == unique(flora_no1$sampling)[i] & plot == unique(flora_no1$plot)[j])
    subrad <- summarise(group_by(subset_data, code),
                        abundance = round(mean(abundance), 0)) 
    subrad <- pivot_wider(subrad, names_from = code, values_from = abundance, values_fill = 0)
    subrad <- as.data.frame(subrad)
    rad_sub <- vegan::radfit(subrad)
    
    radcoeff_df$plot[count] <- unique(flora_no1$plot)[j]
    radcoeff_df$sampling[count] <- unique(flora_no1$sampling)[i]
    radcoeff_df$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
    radcoeff_df$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
    radcoeff_df$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
    
    rm(subrad)
    rm(subset_data)
    rm(rad_sub)
  }
}

# *** #mean here is "fake". Actually we keep the same number of abundance per species. However,
# we have several replicates of the same species per plot due to the different morphological measuremens
# for biomass. The mean of the same number is the same number. 

#Plot 15 (t = p) del sampling 2 aparece como NA. Hacer individual dar valores de i = 5 y j = 7
# (*la posicion de los niveles no corresponde con el valor del muestreo o plot). Da valores de "inf". 


# Al hacer el loop los samplings van del 1 al 12 en vez del 0 al 11
radcoeff_df$sampling <- factor(as.integer(radcoeff_df$sampling) - 1,
                           levels = 0:max(as.integer(radcoeff_df$sampling)) - 1)
radcoeff_df$plot <- as.factor(radcoeff_df$plot )
radcoeff_df$sampling <- as.factor(radcoeff_df$sampling)

plots <- read.csv("data/plots.csv") %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(plot = nplot, treatment = treatment_code) %>% 
  select(treatment, plot)


radcoeff_df <- merge(radcoeff_df, plots, by = "plot")

# Adding by hand sampling == 1 and treatments w and c. ######

flora_s1cw <- flora_rad[(flora_rad$sampling == 1 & (flora_rad$treatment == "c" | flora_rad$treatment == "w")), ]

radcoeff_s1cw <- matrix(nrow = (length(unique(flora_s1cw$sampling))*length(unique(flora_s1cw$plot))), ncol = 5)
colnames(radcoeff_s1cw) <- c("plot", "sampling", "Y_zipf", "mu_log", "sigma_log")
radcoeff_s1cw <- as.data.frame(radcoeff_s1cw)

count = 0 
for(i in 1:length(unique(flora_s1cw$plot))){
  count <- count + 1
  subset_data <- subset(flora_s1cw, sampling == "1" & plot == unique(flora_s1cw$plot)[i])
  subrad <- summarise(group_by(subset_data, code),
                      abundance = round(mean(abundance), 0))
  subrad <- pivot_wider(subrad, names_from = code, values_from = abundance, values_fill = 0)
  subrad <- as.data.frame(subrad)
  rad_sub <- radfit(subrad)
  
  radcoeff_s1cw$plot[count] <- unique(flora_s1cw$plot)[i]
  radcoeff_s1cw$sampling[count] <- "1"
  radcoeff_s1cw$Y_zipf[count] <- rad_sub$models$Zipf$coefficients[2]
  radcoeff_s1cw$mu_log[count] <- rad_sub$models$Lognormal$coefficients[1]
  radcoeff_s1cw$sigma_log[count] <- rad_sub$models$Lognormal$coefficients[2]
  
  rm(subrad)
  rm(subset_data)
  rm(rad_sub)
}

#15 warnings due to plot 1. But it works

radcoeff_s1cw$plot <- as.factor(radcoeff_s1cw$plot )
radcoeff_s1cw$sampling <- as.factor(radcoeff_s1cw$sampling)

radcoeff_s1cw <- merge(radcoeff_s1cw, plots, by = "plot")



#Adding muestreo 1 de c y w. 

radcoeff_df <- rbind(radcoeff_df, radcoeff_s1cw)
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment)

radcoeff_df$plot <- factor(radcoeff_df$plot, levels = sort(unique(radcoeff_df$plot)))
radcoeff_df$treatment <- as.factor(radcoeff_df$treatment) 
radcoeff_df$treatment <- factor(radcoeff_df$treatment, levels = c("c", "w", "p", "wp"))
radcoeff_df$sampling <- factor(radcoeff_df$sampling, levels = sort(unique(radcoeff_df$sampling)))  #Sort from lowest to highest


dummy_rows_p <- matrix(nrow = 4, ncol = 6)
  colnames(dummy_rows_p) <- c("sampling", "plot", "treatment", "Y_zipf", "mu_log", "sigma_log")
  dummy_rows_p <- as.data.frame(dummy_rows_p)
  
  dummy_rows_p[] <- NA
  dummy_rows_p[, 1] <- 1
  dummy_rows_p[, 2] <- c(3, 6, 10, 15)
  dummy_rows_p[, 3] <- "p"
  
  
  dummy_rows_wp <- matrix(nrow = 4, ncol = 6)
  colnames(dummy_rows_wp) <- c("sampling", "plot", "treatment", "Y_zipf", "mu_log", "sigma_log")
  dummy_rows_wp <- as.data.frame(dummy_rows_wp)
  
  dummy_rows_wp[] <- NA
  dummy_rows_wp[, 1] <- 1
  dummy_rows_wp[, 2] <- c(4, 5, 12, 13)
  dummy_rows_wp[, 3] <- "wp"
  
  
  dummy_rows <- bind_rows(dummy_rows_p, dummy_rows_wp)%>% 
    mutate(treatment = as.factor(treatment), 
           sampling = as.factor(sampling), 
           plot = as.factor(plot))
 


radcoeff_df <- bind_rows(radcoeff_df, dummy_rows) 


radcoeff_df %>% write.csv("data/radcoeff_df.csv", row.names = FALSE)

rm(flora_rad)
rm(flora_no1)
rm(flora_s1cw)
rm(plots)
rm(radcoeff_s1cw)



