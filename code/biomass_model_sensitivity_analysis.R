
rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, here)




flora_medium <-  read.csv("data/data_sensitivity_biomass.csv")

## $d$ and $z$ influence on $x$

quantile(flora_medium$x, na.rm = T)


d <- c(0.98, 1.96, 3.92)


x_all <- flora_medium %>% pull(x)

x_small <- flora_medium %>% 
  filter(x < quantile(flora_medium$x, na.rm = T)[2]) %>% 
  pull(x)

x_big <- flora_medium %>%
  filter(x > quantile(x, na.rm = TRUE)[4]) %>%
  pull(x)

z <- seq(0.1, 1, 0.01)


df_all <- expand.grid(d, x_all, z)
colnames(df_all) <- c("d", "x", "z")
df_all <- df_all %>% 
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


# no funsiona. Mirar

dvalue <- 0.98
tilexz <- 
  ggplot(df_all[which(df_all$d == dvalue),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", dvalue, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

dvalue <- 1.96
tilexz_mind <- 
  ggplot(df_all[which(df_all$d == dvalue),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", dvalue, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
```

```{r tilexz_maxd}
dvalue <- 3.92
tilexz_maxd <- 
  ggplot(df1[which(df1$d == dvalue),], aes(x = x, y = z)) +
  geom_tile(aes(fill = biomass)) +
  labs(fill = "Biomass", title = paste("d", "=", dvalue, sep = " ")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()
```


```{r arrange_d}
ggarrange(tilexz, tilexz_mind, tilexz_maxd)
```