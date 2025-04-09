




# probar con log(abundance) u otras transformaciones 



#rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, DT, viridis, ggrepel, codyn, vegan, eulerr, ggplot2, ggthemes, ggpubr, ggforce )#
source("code/1.first_script.R")
#rm(list = setdiff(ls(), c("flora_abrich", "biomass_imp", "biomass_noimp")))

source("code/palettes_labels.R")
palette <- palette5
labels <- labels3


### TURNOVER

# TURNOVER ##### package "codyn"
library(codyn)

# T = (E + C)/R, where T is the percentage turnover rate, E is the number of taxa that went extinct between two time points,
# C is the number of taxa that colonised the community between the same two time points, and R is the total number of species



# TURNOVER AT SAMPLING LEVEL 

species_ab_sampling <- flora_abrich %>% 
  group_by(code, date, sampling, treatment) %>% 
  summarise(abundance = mean(abundance_s, na.rm = T)) %>% 
  mutate(id = paste0(as.character(treatment), "/" , as.character(sampling))) %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))


# (i.e. the richness of the pool of species conformed by the two samples). 

species_ab_sampling$sampling <- as.numeric(species_ab_sampling$sampling) # fuction codyn::turnover needs time variable to be numeric # fuction codyn::turnover needs time variable to be numeric
species_ab_sampling$sampling <- species_ab_sampling$sampling - 1 # when transforming to numeric, it transform factor into their position. And sampling 0 goes into position 1


sp_total_turnover <- species_ab_sampling %>% 
  codyn::turnover(time.var = "sampling",
                  species.var = "code",
                  abundance.var = "abundance",
                  metric = "total",# calculates summed appearances and disappearances relative to total species richness across both time periods.
                  replicate.var = "treatment") %>% 
  mutate(sampling = as.factor(sampling))

sp_appear <- species_ab_sampling %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "appearance", # Calculates the number of species that appeared in the second 
           # time period relative to total species richness across both time periods
           replicate.var = "treatment")

sp_disappear <- species_ab_sampling %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "disappearance", # Calculates the number of species that disappeared in the second
           # time period relative to total species richness across both time periods.
           replicate.var = "treatment")


sp_turnover <-left_join(sp_appear, sp_disappear) %>% 
  pivot_longer(cols = -c(sampling, treatment),
               names_to = "metric",
               values_to = "rate") %>% 
  mutate(sampling = as.factor(sampling))



sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
  mutate(sampling = as.factor(sampling),
         date = ymd(date)) %>% 
  select(sampling, date) %>% 
  mutate(across(where(is.character), as.factor))


sp_turnover <- sp_turnover %>% 
  right_join(sampling_dates, by = join_by(sampling)) %>% 
  filter(!is.na(rate))

sp_total_turnover <- sp_total_turnover %>% 
  right_join(sampling_dates, by = join_by(sampling)) %>% 
  filter(!is.na(total))


ggplot(sp_turnover, aes(x = as.numeric(sampling), y = rate)) +
  facet_grid(~ treatment, labeller = labeller(treatment = labels3)) +
  geom_col(aes(fill = metric)) +
  geom_point(data = sp_total_turnover, aes(y = total)) +
  geom_line(data = sp_total_turnover, aes(y = total)) +
  scale_fill_viridis_d(begin = 0.2) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) + # Por si ponemos sampling
  #scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sampling",
       x = "Sampling",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
        legend.position = "bottom")



# TURNOVER AT PLOT LEVEL. 

#Since we want mean abundance of species at plot, level, "flora_abrich" already give us this information
species_ab_plot <- flora_abrich %>% 
  select(code, date, sampling, plot, treatment, abundance_s) %>% 
  rename(abundance = abundance_s) %>% 
  mutate(id = paste0(as.character(treatment), "/" , as.character(sampling), "/", as.character(plot))) %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp"))) %>% 
  mutate(sampling = as.numeric(sampling)) %>% 
  mutate(sampling = sampling - 1)

list_appear <- list()
list_disappear <- list()
list_total <- list()
plots <- sort(unique(flora_abrich$plot))

for(i in seq_along(plots)){
  
  species_ab_plot_i <- species_ab_plot %>% 
    filter(plot == plots[i])
  
  list_total[[i]] <- species_ab_plot_i %>% 
    codyn::turnover(time.var = "sampling",
                    species.var = "code",
                    abundance.var = "abundance",
                    metric = "total",# calculates summed appearances and disappearances relative to total species richness across both time periods.
                    replicate.var = "treatment") %>% 
    mutate(sampling = as.factor(sampling)) %>% 
    mutate(plot = plots[i])  %>% 
    right_join(sampling_dates, by = join_by(sampling)) %>% 
    filter(!is.na(total)) %>% 
    rename (total_turnover = total)

  
  list_appear[[i]] <- species_ab_plot_i %>% 
    turnover(time.var = "sampling",
             species.var = "code",
             abundance.var = "abundance",
             metric = "appearance", # Calculates the number of species that appeared in the second 
             # time period relative to total species richness across both time periods
             replicate.var = "treatment")%>% 
    mutate(sampling = as.factor(sampling)) %>% 
    mutate(plot = plots[i]) %>% 
    filter(!is.na(appearance)) %>% 
    right_join(sampling_dates, by = join_by(sampling)) 
  
  
  list_disappear[[i]] <- species_ab_plot_i %>% 
    turnover(time.var = "sampling",
             species.var = "code",
             abundance.var = "abundance",
             metric = "disappearance", # Calculates the number of species that disappeared in the second
             # time period relative to total species richness across both time periods.
             replicate.var = "treatment")%>% 
    mutate(sampling = as.factor(sampling)) %>% 
    mutate(plot = plots[i]) %>% 
    filter(!is.na(disappearance)) %>% 
    right_join(sampling_dates, by = join_by(sampling)) 
  
  
  
}

appearance_plot_db <- do.call(rbind, list_appear) %>% 
  filter(!is.na(treatment))
disappearance_plot_db <- do.call(rbind, list_disappear) %>% 
  filter(!is.na(treatment))
total_plot_db <- do.call(rbind, list_total) %>% 
  filter(!is.na(treatment))

turnover_db<- left_join(appearance_plot_db, disappearance_plot_db)
turnover_db <- left_join(turnover_db, total_plot_db) 

turnover_db %>%  write.csv("data/turnover_db.csv")

#turnover_db <- turnover_db %>% 
#  distinct(treatment, plot, sampling, date, .keep_all = TRUE) %>%
#  group_by(sampling, treatment) %>% 
#  mutate(
#    mean_appearance = mean(appearance, na.rm = T), 
#    sd_appearance = sd(appearance, na.rm = T),
#    mean_disappearance = mean(disappearance, na.rm = T), 
#    sd_disappearance = sd(disappearance, na.rm = T),
#    mean_total_turnover = mean(total_turnover, na.rm = T),
#    sd_total_turnover = sd(total_turnover, na.rm = T)
#    ) %>% 
#  mutate(
#    cv_appearance = sd_appearance/mean_appearance,
#    cv_disappearance = sd_disappearance/mean_disappearance,  
#    cv_appearance = sd_appearance/mean_appearance) %>% 
#  ungroup()
#
#
#turnover_treatmeans_db <- turnover_db %>% 
#  group_by(treatment) %>% 
#  summarize(
#    mean_appearance = mean(appearance, na.rm = T), 
#    sd_appearance = sd(appearance, na.rm = T),
#    mean_disappearance = mean(disappearance, na.rm = T), 
#    sd_disappearance = sd(disappearance, na.rm = T),
#    mean_total_turnover = mean(total_turnover, na.rm = T), 
#    sd_total_turnover = sd(total_turnover, na.rm = T),
#    n = n()) 
  


#source("code/meta_function/meta_function.R")
#source("code/meta_function/gg_dynamics.R")
#
#meta_function(appear_plot_db, "appearance", "treatment")
#
#gg_stats_variable
#
#gg_dunn_variable 
#gg_ttest_variable
#
#gg_all1n
#gg_facet
#
#gg_delta_RR
#gg_delta_RR_wp 
#
#gg_stats_cv
#gg_dunn_cv
#gg_ttest_cv  
#gg_dynamics_cv
