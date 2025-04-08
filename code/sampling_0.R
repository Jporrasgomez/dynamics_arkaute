


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)

source("code/1.first_script.R")
rm(list = setdiff(ls(), c("ab_rich_dynamics", "biomass_imp", "biomass_imp012")))


source("code/palettes_labels.R")
palette <- palette5
labels <- labels3

source("code/meta_function/sampling_0_function.R")

sampling_0(ab_rich_dynamics, "richness", "treatment")
gg_dunn_0
mean_0

sampling_0(ab_rich_dynamics, "abundance", "treatment")
gg_dunn_0
mean_0

radcoeff_df <- read.csv("data/radcoeff_df.csv")

sampling_0(radcoeff_df, "Y_zipf", "treatment")
gg_dunn_0
mean_0

