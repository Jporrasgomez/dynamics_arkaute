

rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr,tidyverse, ggplot2, BIEN, ape, maps, sf, rtry)




###################### TRY ######################
# read individual data of TRY
try.ind <- read.csv("data/traits/indi.trait_longform.csv") # long form data with individuals

# rename traits; the same as the beginning
try.ind$TraitName[try.ind$TraitName == "14_Leaf nitrogen (N) content per leaf dry mass_mg/g"] <- "leafN"
try.ind$TraitName[try.ind$TraitName == "80_Root nitrogen (N) content per root dry mass_mg/g"] <- "rootN"
try.ind$TraitName[try.ind$TraitName == "82_Root tissue density (root dry mass per root volume)_g/cm3"] <- "RTD"
try.ind$TraitName[try.ind$TraitName == "1080_Root length per root dry mass (specific root length, SRL)_cm/g"] <- "SRL"
try.ind$TraitName[try.ind$TraitName == "26_Seed dry mass_mg"] <- "seed.mass"
try.ind$TraitName[try.ind$TraitName == "4_Stem specific density (SSD, stem dry mass per stem fresh volume) or wood density_g/cm3"] <- "SSD"
try.ind$TraitName[try.ind$TraitName == "3107_Plant height generative_m"] <- "generative.height"
try.ind$TraitName[try.ind$TraitName == "3106_Plant height vegetative_m"] <- "vegetation.height"
try.ind$TraitName[try.ind$TraitName == "47_Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)_g/g"] <- "LDMC"
try.ind$TraitName[try.ind$TraitName == "3110_Leaf area (in case of compound leaves: leaf, petiole included)_mm2"] <- "LA.inc"
try.ind$TraitName[try.ind$TraitName == "3108_Leaf area (in case of compound leaves: leaf, petiole excluded)_mm2"] <- "LA.ex"
try.ind$TraitName[try.ind$TraitName == "3112_Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)_mm2"] <- "LA.un"
try.ind$TraitName[try.ind$TraitName == "3111_Leaf area (in case of compound leaves: leaflet, petiole included)_mm2"] <- "LA.leaflet.inc"
try.ind$TraitName[try.ind$TraitName == "3109_Leaf area (in case of compound leaves: leaflet, petiole excluded)_mm2"] <- "LA.leaflet.ex"
try.ind$TraitName[try.ind$TraitName == "3113_Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)_mm2"] <- "LA.leaflet.un"
try.ind$TraitName[try.ind$TraitName == "3114_Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)_mm2"] <- "LA.un.un"
try.ind$TraitName[try.ind$TraitName == "3116_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included_mm2 mg-1"] <- "SLA.inc"
try.ind$TraitName[try.ind$TraitName == "3115_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded_mm2 mg-1"] <- "SLA.ex"
try.ind$TraitName[try.ind$TraitName == "3117_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded_mm2 mg-1"] <- "SLA.un"
try.ind$TraitName[try.ind$TraitName == "3086_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded_mm2 mg-1"] <- "SLA.all.ex"

unique(try.ind$TraitName)

# select traits
try.ind.select = try.ind %>% 
  filter(TraitName %in% c("leafN", "rootN", "RTD", "seed.mass", "SSD", "SRL", "LDMC", 
                          "vegetation.height", "LA.inc", "LA.ex", "LA.un", "SLA.inc", "SLA.ex")) %>%
  select(AccSpeciesName, TraitName, StdValue) %>%
  rename(species = AccSpeciesName, trait_value = StdValue, 
         trait_name = TraitName)

# make the name consistent
try.ind.select$species[try.ind.select$species == "Draba verna"] <- "Erophila verna"   
try.ind.select$species[try.ind.select$species == "CAPSELLA BURSA-PASTORIS"] <- "Capsella bursa-pastoris" 

# add a new column of database name
try.ind.select$database <- "TRY"

###################### BIEN ######################

bien.ind <- read.csv("data/traits/trait_bien_select.csv")
# rename traits in bien
bien.ind$trait_name[bien.ind$trait_name == "seed mass"] <- "seed.mass"
bien.ind$trait_name[bien.ind$trait_name == "leaf dry mass per leaf fresh mass"] <- "LDMC"
bien.ind$trait_name[bien.ind$trait_name == "leaf area per leaf dry mass"] <- "SLA"
bien.ind$trait_name[bien.ind$trait_name == "leaf area"] <- "LA"
bien.ind$trait_name[bien.ind$trait_name == "whole plant height"] <- "height"
bien.ind$trait_name[bien.ind$trait_name == "maximum whole plant height"] <- "mix.height"
bien.ind$trait_name[bien.ind$trait_name == "leaf nitrogen content per leaf dry mass"] <- "leafN"

# select traits
bien.ind.select = bien.ind %>% 
  filter(trait_name != "mix.height") %>%
  select(scrubbed_species_binomial, trait_name, trait_value) %>%
  rename(species = scrubbed_species_binomial)

# add a new column of database name
bien.ind.select$database <- 'BIEN'

# merge data from TRY and BIEN
combine.try.bien <- rbind(try.ind.select, bien.ind.select)

###################### GROOT ######################

groot.ind <- read.csv("data/traits/GrooT_selected_3roottraits.csv")
# rename traits in groot
groot.ind$traitName[groot.ind$traitName == "Root_N_concentration"] <- "rootN"
groot.ind$traitName[groot.ind$traitName == "Root_tissue_density"] <- "RTD"
groot.ind$traitName[groot.ind$traitName == "Specific_root_length"] <- "SRL"

# select traits
groot.ind.select = groot.ind %>%
  select(speciesname, traitName, traitValue) %>%
  rename(species = speciesname, trait_name = traitName, trait_value = traitValue)

#change "Lysimachia arvensis" to "Anagallis arvensis" (But no this species)
groot.ind.select$species[groot.ind.select$species == "Lysimachia arvensis"] <- "Anagallis arvensis" 

# add database column
groot.ind.select$database <- 'GROOT'

# merge groot
combine.try.bien.groot <- rbind(combine.try.bien, groot.ind.select)
# add a column combine trait name and database
combine.try.bien.groot = combine.try.bien.groot %>%
  unite(trait_ID, trait_name, database, remove = F)

# check the units.
# convert g/g in TRY to mg/g
# SRL in GRooT is m/g, in TRY is cm/g. convert TRY cm/g to m/g
combine.indi = combine.try.bien.groot %>% 
  mutate(trait_value = if_else(trait_ID == "LDMC_TRY", trait_value * 1000, trait_value)) %>%
  mutate(trait_value = if_else(trait_ID == "SRL_TRY", trait_value/100, trait_value))

# calculate species mean trait value; long to wide
combine.sp.wide = combine.indi %>% 
  group_by(species, trait_ID) %>% 
  summarise(species.mean.trait = mean(trait_value, na.rm = T)) %>%
  spread(trait_ID, species.mean.trait)

# fill missing values within TRY
# leaf area: select include petiole 3110 (LA.inc), fill the gaps using values from 3108 (LA.ex); see Carmona et al. 2020
# select the species used for filling gaps
sp_used.LA.ex_TRY = combine.sp.wide %>%
  filter(is.na(LA.inc_TRY) & !is.na(LA.ex_TRY)) %>%
  select(species, LA.ex_TRY)

combine.sp.fill = combine.sp.wide %>%
  mutate(LA = if_else(is.na(LA.inc_TRY), LA.ex_TRY, LA.inc_TRY)) %>%
  select(species, LA.inc_TRY, LA.ex_TRY, LA, everything())

# then fill the gap with 3112 (LA.un) 
sp_used.LA.un.TRY = combine.sp.fill %>%
  filter(is.na(LA) & !is.na(LA.un_TRY)) %>%
  select(species, LA.un_TRY)

combine.sp.fill = combine.sp.fill %>%
  mutate(LA = if_else(is.na(LA), LA.un_TRY, LA)) %>%
  select(species, LA.un_TRY, LA, everything())

# SLA: select 3116 (include petiole; SLA.inc), fill the gaps using the values from 3115 (SLA.ex)
sp_used.SLA.ex.TRY = combine.sp.fill %>%
  filter(is.na(SLA.inc_TRY) & !is.na(SLA.ex_TRY)) %>%
  select(species, SLA.ex_TRY)

combine.sp.fill = combine.sp.fill %>%
  mutate(SLA = if_else(is.na(SLA.inc_TRY), SLA.ex_TRY, SLA.inc_TRY)) %>%
  select(species, SLA.inc_TRY, SLA.ex_TRY, SLA, everything())

#fill the missing value in TRY using data from BIEN
#fill vegetation.height with 'whole plant height'
sp_used.height.BIEN = combine.sp.fill %>%
  filter(is.na(vegetation.height_TRY) & !is.na(height_BIEN)) %>%
  select(species, height_BIEN) # no species

# leaf area
sp_used.LA.BIEN = combine.sp.fill %>%
  filter(is.na(LA) & !is.na(LA_BIEN)) %>%
  select(species, LA_BIEN) 

combine.sp.fill = combine.sp.fill %>%
  mutate(LA = if_else(is.na(LA), LA_BIEN, LA)) %>%
  select(species, LA_BIEN, LA, everything())

# SLA
sp_used.SLA.BIEN = combine.sp.fill %>%
  filter(is.na(SLA) & !is.na(SLA_BIEN)) %>%
  select(species, SLA_BIEN) # no species

# LDMC
sp_used.LDMC.BIEN = combine.sp.fill %>%
  filter(is.na(LDMC_TRY) & !is.na(LDMC_BIEN)) %>%
  select(species, LDMC_BIEN)

combine.sp.fill = combine.sp.fill %>%
  mutate(LDMC = if_else(is.na(LDMC_TRY), LDMC_BIEN, LDMC_TRY)) %>%
  select(species, LDMC_BIEN, LDMC, everything())

# leafN
sp_used.leafN.BIEN = combine.sp.fill %>%
  filter(is.na(leafN_TRY) & !is.na(leafN_BIEN)) %>%
  select(species, leafN_BIEN)

combine.sp.fill = combine.sp.fill %>%
  mutate(leafN = if_else(is.na(leafN_TRY), leafN_BIEN, leafN_TRY)) %>%
  select(species, leafN_BIEN, leafN, everything())

# seed mass
sp_used.SM.BIEN = combine.sp.fill %>%
  filter(is.na(seed.mass_TRY) & !is.na(seed.mass_BIEN)) %>%
  select(species, seed.mass_BIEN) # no species

#fill the missing value using data from groot
# root N
sp_used.rootN.GROOT = combine.sp.fill %>%
  filter(is.na(rootN_TRY) & !is.na(rootN_GROOT)) %>%
  select(species, rootN_GROOT)

combine.sp.fill = combine.sp.fill %>%
  mutate(rootN = if_else(is.na(rootN_TRY), rootN_GROOT, rootN_TRY)) %>%
  select(species, rootN_GROOT, rootN, everything())

# RTD
sp_used.RTD.GROOT = combine.sp.fill %>%
  filter(is.na(RTD_TRY) & !is.na(RTD_GROOT)) %>%
  select(species, RTD_GROOT)

combine.sp.fill = combine.sp.fill %>%
  mutate(RTD = if_else(is.na(RTD_TRY), RTD_GROOT, RTD_TRY)) %>%
  select(species, RTD_GROOT, RTD, everything())

# SRL
sp_used.SRL.GROOT = combine.sp.fill %>%
  filter(is.na(SRL_TRY) & !is.na(SRL_GROOT)) %>%
  select(species, SRL_GROOT)

combine.sp.fill = combine.sp.fill %>%
  mutate(SRL = if_else(is.na(SRL_TRY), SRL_GROOT, SRL_TRY)) %>%
  select(species, SRL_GROOT, SRL, everything())

# select the traits
combine.final = combine.sp.fill %>% 
  select(species, leafN, rootN, RTD, seed.mass_TRY, 
         LDMC, SSD_TRY, SRL, LA, SLA, vegetation.height_TRY) %>%
  rename(seed.mass = seed.mass_TRY, SSD = SSD_TRY, height = vegetation.height_TRY)

# species level data; should be the same as 'combined.TRY.BIEN.GRooT' got before
write.csv(combine.final, 'data/traits/combined.TRY.BIEN.GRooT_species_level.csv', row.names = F)

# merge species used in different databases
dfs <- list(sp_used.LA.ex_TRY, sp_used.LA.un.TRY, sp_used.SLA.ex.TRY, 
            sp_used.LA.BIEN, sp_used.LDMC.BIEN, sp_used.leafN.BIEN, 
            sp_used.rootN.GROOT, sp_used.RTD.GROOT, sp_used.SRL.GROOT)

sp_used <- Reduce(function(x, y) full_join(x, y, by = "species"), dfs)

# wide to long
sp_used_long = sp_used %>% 
  pivot_longer(cols = LA.ex_TRY:SRL_GROOT, names_to = "trait_ID", values_to = "trait_value") %>%
  drop_na(trait_value)

# select individual data used to fill the gaps from TRY
combine.indi.used = combine.indi %>% semi_join(sp_used_long, by = c("species", "trait_ID"))

# select all the data used in TRY
TRY.used = combine.indi %>% filter(trait_ID %in% c("leafN_TRY", "rootN_TRY", "RTD_TRY", "seed.mass_TRY", "LDMC_TRY",
                                                   "vegetation.height_TRY", "SSD_TRY", "SRL_TRY", "LA.inc_TRY", "SLA.inc_TRY"))

# combine the two dataframes above; outlier should be checked based on 'all.indi.used.csv'
all.indi.used <- rbind(TRY.used, combine.indi.used)
write.csv(all.indi.used, 'data/traits/all.indi.used.csv', row.names = F)
