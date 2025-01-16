

rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)


source("code/flora_code.R")
source("code/species_composition_analysis.R")
source("code/micro_code.R")

#theme_set(theme_bw()+ theme(legend.position = "NULL"))


ggdynamics_abundance
ggdynamics_richness
ggdynamics_biomass
ggdynamics_mulog
ggdynamics_sigmalog
ggdynamics_yzipf


gglogRR_abundance
gglogRR_richness
gglogRR_biomass
gglogRR_mulog
gglogRR_sigmalog
gglogRR_yzipf

ggcv_abundance
ggcv_biomass
ggcv_richness
ggcv_mulog
ggcv_sigmalog
ggcv_yzipf


gglogRRcv_abundance
gglogRRcv_biomass
gglogRRcv_richness
gglogRRcv_mulog
gglogRRcv_sigmalog
gglogRRcv_yzipf


ggturnover
ggpcoa_hell
ggpcoa_hell_alltreatments
ggpcoa_clouds 
ggpcoa_cloudspersampling


ggdynamics_prokariota
ggdynamics_fungi
gglogRR_prokariota
gglogRR_fungi



