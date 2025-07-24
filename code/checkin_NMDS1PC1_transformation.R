



rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr, tidyr, tidyverse, ggplot2, BIEN, ape, maps, sf, rtry, ggrepel)



ft <-  read.csv("data/pca_cwm_plot.csv") %>% 
  select(-PC3)


sc <-  read.csv("data/nmds_df_plot_hellinger.csv") %>% 
  select(-NMDS3) %>% 
  mutate(
    sc1 = NMDS1 + abs(min(NMDS1)) + 1,
    sc2 = NMDS2 + abs(min(NMDS2)) + 1
  )

sc %>% 
  ggplot(aes(x = NMDS1, y = sc1)) + 
  geom_point()

C <- abs(min(sc$NMDS1)) + 1

sc_c <- sc %>% 
  filter(treatment == "c") %>% 
  group_by(treatment) %>% 
  summarize(mean_sc1_c = mean(sc1),
            mean_NMDS1_c = mean(NMDS1), 
            geo_mean_NMDS1_c = prod(NMDS1 + C)^(1/n()),
            gm_NMDS1_c = exp(mean(log(NMDS1 + C)))
            ) %>% 
  select(-treatment)
sc_p <- sc %>% 
  filter(treatment == "p") %>% 
  group_by(treatment) %>% 
  summarize(mean_sc1_p = mean(sc1),
            mean_NMDS1_p = mean(NMDS1), 
            geo_mean_NMDS1_p = prod(NMDS1 + C)^(1/n()),
            gm_NMDS1_p = exp(mean(log(NMDS1 + C)))
            ) %>% 
  select(-treatment)

sc_w <- sc %>% 
  filter(treatment == "w") %>% 
  group_by(treatment) %>% 
  summarize(mean_sc1_w = mean(sc1),
            mean_NMDS1_w = mean(NMDS1), 
            geo_mean_NMDS1_w = prod(NMDS1 + C)^(1/n()),
            gm_NMDS1_w = exp(mean(log(NMDS1 + C)))
  ) %>% 
  select(-treatment)

sc_wp <- sc %>% 
  filter(treatment == "wp") %>% 
  group_by(treatment) %>% 
  summarize(mean_sc1_wp = mean(sc1),
            mean_NMDS1_wp = mean(NMDS1), 
            geo_mean_NMDS1_wp = prod(NMDS1 + C)^(1/n()),
            gm_NMDS1_wp = exp(mean(log(NMDS1 + C)))
  ) %>% 
  select(-treatment)



sc_check <- cbind(sc_c, sc_p)
sc_check <- cbind(sc_check, sc_w)
sc_check <- cbind(sc_check, sc_wp)


sc_check$mean_sc1_p / sc_check$mean_sc1_c 
sc_check$mean_NMDS1_p / sc_check$mean_NMDS1_c
sc_check$geo_mean_NMDS1_p / sc_check$geo_mean_NMDS1_c
sc_check$gm_NMDS1_p / sc_check$gm_NMDS1_c

sc_check$mean_sc1_w / sc_check$mean_sc1_c
sc_check$mean_NMDS1_w / sc_check$mean_NMDS1_c
sc_check$geo_mean_NMDS1_w / sc_check$geo_mean_NMDS1_c
sc_check$gm_NMDS1_w / sc_check$gm_NMDS1_c

sc_check$mean_sc1_wp / sc_check$mean_sc1_c
sc_check$mean_NMDS1_wp / sc_check$mean_NMDS1_c
sc_check$geo_mean_NMDS1_wp / sc_check$geo_mean_NMDS1_c
sc_check$gm_NMDS1_wp / sc_check$gm_NMDS1_c



a <- 
cohen.d(
  (sc %>% filter(treatment == "w") %>% pull(NMDS1)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS1)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "p") %>% pull(NMDS1)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS1)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "wp") %>% pull(NMDS1)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS1)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "wp") %>% pull(NMDS1)),
  (sc %>% filter(treatment == "p") %>% pull(NMDS1)),
  hedges.correction = TRUE
)





cohen.d(
  (sc %>% filter(treatment == "w") %>% pull(NMDS2)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS2)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "p") %>% pull(NMDS2)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS2)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "wp") %>% pull(NMDS2)),
  (sc %>% filter(treatment == "c") %>% pull(NMDS2)),
  hedges.correction = TRUE
)


cohen.d(
  (sc %>% filter(treatment == "wp") %>% pull(NMDS2)),
  (sc %>% filter(treatment == "p") %>% pull(NMDS2)),
  hedges.correction = TRUE
)

