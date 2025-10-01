


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, DT, viridis, ggrepel, codyn, vegan, eulerr, ggplot2, ggthemes, ggpubr, ggforce )#


sp_wide_plot <- read.csv("data/relative_abudance_species_time.csv") 

spcomp <- sp_wide_plot %>% 
  select(-X) %>% 
  filter(sampling != "0") %>% 
  pivot_longer(5:44, values_to = "abundance", names_to = "code") %>% 
  mutate(abundance = na_if(abundance, 0)) %>%           # 0 -> NA
  group_by(treatment, code) %>% 
  summarise(mean_abundance = mean(abundance, na.rm = TRUE), .groups = "drop") %>% 
  filter(!is.nan(mean_abundance)) %>% 
  mutate(across(where(is.character), as.factor))


spcomp_c <- spcomp %>% 
  filter(treatment == "c") %>% 
  pull(code) %>% 
  droplevels() %>% 
  unique() %>% 
  print()

spcomp_w <- spcomp %>% 
  filter(treatment == "w") %>% 
  pull(code) %>% 
  droplevels() %>% 
  unique() %>% 
  print()

spcomp_p <- spcomp %>% 
  filter(treatment == "p") %>% 
  pull(code) %>% 
  droplevels() %>% 
  unique() %>% 
  print()

spcomp_wp <- spcomp %>% 
  filter(treatment == "wp") %>% 
  pull(code) %>% 
  droplevels() %>% 
  unique() %>% 
  print()



setdiff(spcomp_c, spcomp_w)
setdiff(spcomp_w, spcomp_c)

setdiff(spcomp_c, spcomp_p)
setdiff(spcomp_p, spcomp_c)

setdiff(spcomp_c, spcomp_wp)
setdiff(spcomp_wp, spcomp_c)

setdiff(spcomp_wp, spcomp_p)
setdiff(spcomp_p, spcomp_wp)

setdiff(spcomp_wp, spcomp_w)
setdiff(spcomp_w, spcomp_wp)



library(dplyr); library(tidyr)

exclusivos_tbl <- bind_rows(
  tibble(code = spcomp_c,  treatment = "c"),
  tibble(code = spcomp_w,  treatment = "w"),
  tibble(code = spcomp_p,  treatment = "p"),
  tibble(code = spcomp_wp, treatment = "wp")
) %>%
  distinct() %>%                         # (code, treatment) únicos
  count(code, treatment, name = "n") %>% # presencia por tratamiento
  count(code, name = "n_groups") %>%     # cuántos tratamientos tiene cada code
  inner_join(
    bind_rows(
      tibble(code = spcomp_c,  treatment = "c"),
      tibble(code = spcomp_w,  treatment = "w"),
      tibble(code = spcomp_p,  treatment = "p"),
      tibble(code = spcomp_wp, treatment = "wp")
    ) %>% distinct(),
    by = "code"
  ) %>%
  filter(n_groups == 1) %>%              # exclusivos de un solo tratamiento
  arrange(treatment)

exclusivos_tbl
# Verás los codes exclusivos y a qué tratamiento pertenecen.


sp_wide_plot %>% 
  select(-X) %>% 
  filter(treatment %in% c("p", "wp")) %>% 
  pivot_longer(5:44, values_to = "abundance", names_to = "code") %>% 
  mutate(abundance = na_if(abundance, 0)) %>% 
  filter(code %in% c("amsp", "casp", "chsp", "cisp", "kisp", "lapu", "mean", "cabu")) %>% 
  group_by(treatment, sampling, date, code) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  filter(!is.nan(mean_abundance)) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  ggplot(aes(x = sampling, y = mean_abundance, color = code)) +
  facet_wrap(~ treatment,
             labeller = labeller(treatment = as_labeller(labels1))) +
  geom_point(aes(shape = code)) + 
  geom_path(aes(group = code)) + 
  geom_vline(xintercept = 1.5, linetype = "dashed") + 
  labs(y = "Mean relative abundance of colonizers (%)", 
       x = "Samplings", color = "Species", shape = "Species")




sp_wide_plot %>% 
  select(-X) %>% 
  filter(treatment %in% c("c", "w")) %>% 
  pivot_longer(5:44, values_to = "abundance", names_to = "code") %>% 
  mutate(abundance = na_if(abundance, 0)) %>% 
  filter(code %in% setdiff(spcomp_c, spcomp_w)) %>% 
  group_by(treatment, sampling, code) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  filter(!is.nan(mean_abundance)) %>% 
  #View() %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  ggplot(aes(x = sampling, y = mean_abundance, color = code)) +
  facet_wrap(~ treatment) + 
  geom_point(aes(shape = code)) + 
  geom_path(aes(group = code)) + 
  geom_vline(xintercept = 1.5, linetype = "dashed") + 
  labs(y = "Mean relative abundance of low-heat tolerance species(%)", 
       x = "Samplings")
# According to this, this species were only present on CONTROL. Therefore, the differences of richness
# are due to the momentaneum disappearance of some species at certain moments


sp_wide_plot %>% 
  select(-X) %>% 
  pivot_longer(5:44, values_to = "abundance", names_to = "code") %>% 
  mutate(abundance = na_if(abundance, 0)) %>% 
  group_by(treatment, sampling, code) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  filter(!is.nan(mean_abundance)) %>% 
  group_by(treatment, sampling) %>% 
  mutate(total_abundance = sum(mean_abundance)) %>% 
  ungroup() %>% 
  #filter(mean_abundance > 5) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  ggplot(aes(x = sampling, y = mean_abundance, color = code)) +
  facet_wrap(~ treatment,
             labeller = labeller(treatment = as_labeller(labels1))) +
  geom_point(aes(alpha = 0.5)) + 
  geom_path(aes(group = code, alpha = 0.5, legend = NULL)) + 
  #geom_path(aes(x = sampling, y = total_abundance, color = "Total abundance")) +
  geom_vline(xintercept = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = 12.5, linetype = "dashed", color = "grey50") +  # separación 2023 - 2024
  labs(y = "Mean relative abundance (%)", 
       x = "Samplings", color = "Species")

