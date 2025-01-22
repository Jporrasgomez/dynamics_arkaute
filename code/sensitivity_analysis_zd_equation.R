
#Script en el que trabajar hasta que el .Rmd funcione (línea 186 corrupta)

library(tidyverse)
library(ggpubr)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)
library(ggpubr)
library(tidyverse)
library(gridExtra)


flora_raw <- read.csv("data/flora_db.csv")
flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))
desired_order <- c("s_0", "s_1", "s_2", "s_3", "s_4", "s_5", "s_6", 
                   "s_7", "s_8", "s_9", "s_10", "s_11")
flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)

#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01

flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm)

flora$cm <- round(ifelse(!is.na(flora$Dm), flora$Dm * pi, flora$Cm), 2)
flora$cb <- round(ifelse(!is.na(flora$Db), flora$Db * pi, flora$Cb), 2)
flora$Ah <- ((flora$cm)^2)/4*pi
flora$Ab <- ((flora$cb)^2)/4*pi

#Application of equation proposed by paper Perrone R. et al. 2020

d_perrone <- 1.96
z_perrone <- 2/3
flora$x <- (flora$height/2)*(flora$Ab + flora$Ah)
flora$biomass <- d_perrone*(flora$x^z_perrone)

par(mfrow = c(1,2))
hist(flora$x)
boxplot(flora$x)


#As we can see, there is a problem with $X$ outliers. If we take a look to the quantiles:
  
quantile(flora$x, na.rm = TRUE, probs = 0)
quantile(flora$x, na.rm = TRUE, probs = 0.25)
quantile(flora$x, na.rm = TRUE, probs = 0.50)
quantile(flora$x, na.rm = TRUE, probs = 0.75)
quantile(flora$x, na.rm = TRUE, probs = 1)

##Removing outliers

flora1 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3 <- flora[which(flora$x < (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 

#List of outliers: 
flora1_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (1.5 * IQR(flora$x, na.rm = TRUE))))),] 
flora3_outl <- flora[which(flora$x > (as.numeric(quantile(flora$x, na.rm = TRUE)[4] + (3 * IQR(flora$x, na.rm = TRUE))))),] 

#Visualización datos con y sin outliers

print(ggarrange(
  ggplot(flora, aes(y = x)) +
    geom_boxplot(), 
  ggplot(flora1, aes(y = x)) +
    geom_boxplot(),
  ggplot(flora3, aes(y = x)) +
    geom_boxplot(),
  labels = c("+outl", "-outl", "-ext.outl"),
  ncol = 3, nrow = 1))

par(mfrow = c(1, 3))
hist(flora$x)
hist(flora1$x)
hist(flora3$x)
par(mfrow = c(1, 1))


### $d$ and $z$ influence on $x$

#These are x values of flora without NAs
x_data <- read.table("data/x_values.txt", header = TRUE, sep = "\t")
flora_noNA<- flora[which(!is.na(flora$x)) , ]$x

#Values
d <- c(0.98, 1.96, 3.92)
xout <- round(as.numeric(quantile(flora1_outl$x)),2)
x1 <- seq(round(min(flora1$x), 0), round(max(flora1$x), 0), 25)
x3 <- seq(round(min(flora3$x), 0), round(max(flora3$x), 0), 25)
x_small <- seq(round(min(flora1$x), 0), round(quantile(flora$x, na.rm = TRUE, probs = 0.25), 0), 0.15)
z <- seq(0.1, 1, 0.01)

#dataframes with different values of X
df_out <- expand.grid(d, xout, z)
colnames(df_out) <- c("d", "x", "z")
df_out$biomass <- df_out$d * (df_out$x ^df_out$z)

df1 <- expand.grid(d, x1, z)
colnames(df1) <- c("d", "x", "z")
df1$biomass <- df1$d * (df1$x ^df1$z)

df3 <- expand.grid(d, x3, z)
colnames(df3) <- c("d", "x", "z")
df3$biomass <- df3$d * (df3$x ^ df3$z)

df_small <- expand.grid(d, x_small, z)
colnames(df_small) <- c("d", "x", "z")
df_small$biomass <- df_small$d * (df_small$x ^ df_small$z)



#Visualization

tilexz_mind <- 
  ggplot(df1[which(df1$d == 0.98),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", "0.98", sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

tilexz_d <- 
  ggplot(df1[which(df1$d == 1.96),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", "1.96", sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

tilexz_maxd <- 
  ggplot(df1[which(df1$d == 3.92),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", "3.92", sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()


ggarrange(tilexz_mind, tilexz_d, tilexz_maxd)



#How z influences values of X (without outliers) with d = 1.96
ggplot(df1[which(df1$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  theme_classic()

ggplot(df_small[which(df_small$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  theme_classic()

ggplot(df_out[which(df_out$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  theme_classic()


