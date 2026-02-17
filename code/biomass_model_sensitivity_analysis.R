
rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, here)


source("code/palettes_labels.R")

flora_medium <-  read.csv("data/data_sensitivity_biomass.csv")

## $d$ and $z$ influence on $x$

quantile(flora_medium$x, na.rm = T)

library(ggdist)
raincloud_plot <- ggplot(flora_medium, aes(x = 1, y = log(x))) +
  
  # Half-violin (the "cloud")
  stat_halfeye(
    adjust = 0.5,
    width = 0.6,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "gray40"
  ) +
  
  
  # Raw data (the "rain")
  geom_jitter(
    width = 0.08,
    alpha = 0.4,
    size = 2
  ) +
  
  # Boxplot (the "box")
  geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    alpha = 0.4,
    linewidth = 0.7
  ) +
  
  coord_flip() +
  scale_x_continuous(breaks = NULL) +
  labs(
    x = NULL,
    y = "log(x)"
  ) +
  
  theme1

print(raincloud_plot)

ggsave("results/Plots/protofinal/x_distribution.png", plot = raincloud_plot, dpi = 300)


d <- c(0.98, 1.96, 3.92)

quantile(flora_medium$x, na.rm = TRUE)[1]
quantile(flora_medium$x, na.rm = TRUE)[2]
quantile(flora_medium$x, na.rm = TRUE)[3]
quantile(flora_medium$x, na.rm = TRUE)[4]
quantile(flora_medium$x, na.rm = TRUE)[5]

x_intermediate <- seq(round(quantile(flora_medium$x, na.rm = TRUE)[2], 3), round(quantile(flora_medium$x, na.rm = TRUE)[4], 3), 1.5)

x_small <- seq(round(min(flora_medium$x, na.rm = T), 3), round(quantile(flora_medium$x, na.rm = TRUE)[3], 3), 0.5)

x_big <- seq(round(quantile(flora_medium$x, na.rm = TRUE)[4], 3), round(max(flora_medium$x, na.rm = T), 3),  10000)




z <- seq(0.1, 1, 0.01)

df_intermediate <- expand.grid(d, x_intermediate, z)
colnames(df_intermediate) <- c("d", "x", "z")
df_intermediate <- df_intermediate %>% 
  mutate(biomass = d*x^z) %>% 
  filter(!is.na(biomass))

df_small <- expand.grid(d, x_small, z)
colnames(df_small) <- c("d", "x", "z")
df_small <- df_small %>% 
  mutate(biomass = d*x^z) %>% 
  filter(!is.na(biomass))

df_big <- expand.grid(d, x_big, z)
colnames(df_big) <- c("d", "x", "z")
df_big <- df_big %>% 
  mutate(biomass = d*x^z) %>% 
  filter(!is.na(biomass))


  ggplot(df_intermediate[which(df_intermediate$d == 0.98),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", 0.98, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

  ggplot(df_intermediate[which(df_intermediate$d == 1.96),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", 1.96, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()


  ggplot(df_intermediate[which(df_intermediate$d == 3.92),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", 3.92, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()


# No cambia nada y esque: 
# Realmente la influencia de D en X nos da igual: 
  # 1) Al ser bulk density promedio de las plantas, es el resultado de las medidas de Perronne
  # 2) la relación es lineal respecto a B. Cuando Mayor sea d, mayor será B. Realmente, por qué z = 2/3?


hist(flora_medium$x, breaks = 100)

perc_intermediate <- round(((flora_medium %>%
  filter(x < max(df_intermediate$x), x > min(df_intermediate$x)) %>%
  nrow()) / nrow(flora_medium))*100, 2)

gg_intermediate <- 
ggplot(df_intermediate[which(df_intermediate$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  labs(x = "z", y = "Biomass", color = "x (intermediate values)") +
  theme1+ 
  theme(legend.position = "right")


perc_small <- round(((flora_medium %>%
          filter(x < max(df_small$x), x > min(df_small$x)) %>%
          nrow()) / nrow(flora_medium))*100, 2)

gg_small <- 
ggplot(df_small[which(df_small$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  labs(x = "z", y = "Biomass", color = "x (small values)") +
  theme1+ 
  theme(legend.position = "right")


perc_big <- round(((flora_medium %>%
          filter(x < max(df_big$x), x > min(df_big$x)) %>%
          nrow()) / nrow(flora_medium))*100, 2)
gg_big <- 
ggplot(df_big[which(df_big$d == 1.96),], aes(x = z, y = biomass, color = as.factor(x))) +
  labs(color = "x", title = paste("d", "=", "1.96", sep = " ")) +
  geom_vline(xintercept = 2/3, linetype = "dashed") +
  geom_point() +
  labs(x = "z", y = "Biomass" , color = "x (big values)") +
  theme1 + 
  theme(legend.position = "right")


library(patchwork)
gg_sens_z <-
  (gg_intermediate + gg_small + gg_big) +
  plot_layout(ncol = 1) +
  plot_annotation(theme = theme(legend.position = "right"),
                  tag_levels = "A")
print(gg_sens_z)

perc_intermediate
perc_small
perc_big

perc_intermediate + perc_small + perc_big

#ggsave("results/Plots/protofinal/senstivity_biomass_z.png", plot = gg_sens_z, dpi = 300)
