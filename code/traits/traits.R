#To do's
# Chosing 

# Gap filling ?? Mirar correo de Mengjiao

rm(list = ls(all.names = TRUE)) 
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(dplyr,FD,tidyverse, lubridate, ggplot2, ggpubr)


source("code/first_script.R")
traits_raw <- read.csv("data/traits/traits_final.csv") #what about poaceae and asteraceae? 
species_code <- read.csv("data/species_code.csv")

#First approach to abundances. 
#Which species are more abundants in the whole experiment? 
flora_abundance <- flora1 %>%
  group_by(code, family, genus_level, species_level) %>%
  summarize(mean_abundance = mean(abundance, na.rm = T),
            sd_abundance= sd(abundance, na.rm = T), 
            n_observations = n())

plot(mean_abundance ~ n_observations, flora_abundance)

sum(flora_abundance$n_observations)

traits_raw$species[traits_raw$species == "Anagallis arvensis"] <- "Lysimachia arvensis"


traits_code <- merge(traits_raw, species_code, by = "species") #Haer un paso intermedio aquí para ver de qué especies no tenemos functional traits_raw y de cuañes tenemos menos


#Comprobaciones de especies que no tenemos

# Species for which we have FT data but they are no longer present in flora database due to aggregation on family or genus level or because we finally didnt measure those species in the field 
setdiff(traits_raw$species, flora$species) 
#Species that we have identified but for which we have no FT data. These species would be great to have to have a more robust mean calculation for poaceae and asteraceae
setdiff(species_code$species, traits_raw$species) 
#Species for which we have FT data but they were rejected from the identified species list, so we do not need it
setdiff(traits_raw$species, traits_code$species) 
#Taxonomic groups present in our flora database for which we do not have FT information
setdiff(flora$species, traits_code$species)
# So far, we have to calculate the FT mean for Poaceae, Torilis sp, Asteraceae and Orchidaceae. For this step it would be valuable to have FT info on
# asteraceae species like Senecio vulgaris, Sonchus sp and Heminthotheca sp (for asteraceae) and Festuca groporruba for poaceae. We have other species
# from these groups but the more we have, the more robust our FT mean will be. 


traits_code$species <- as.factor(traits_code$species)
traits_code$code <- as.factor(traits_code$code)
traits_code$sampling_level <- as.factor(traits_code$sampling_level)
traits_code$family <- as.factor(traits_code$family)
traits_code$genus_level <- as.factor(traits_code$genus_level)
traits_code$species_level <- as.factor(traits_code$species_level)

traits_code <- traits_code %>%
  select(!c(OBS, internal_code, life_cycle, genus_level, species_level, family, sampling_level))

traits_code %>% write.csv("data/traits/traits_code.csv")

#We are going to reject those fuctional traits that are lacking in more than the 60% of species

missing_perc <- colSums(is.na(traits_code)) / nrow(traits_code) * 100
print(missing_perc)

missing_perc_species <- rowSums(is.na(traits_code)) / ncol(traits_code) * 100
print(missing_perc_species)
missing_data_df <- data.frame(
  species = traits_code$species,     # Species names from traits_code
  missing_perc = missing_perc_species # Percentages of missing data
)
missing_data_df_sorted <- missing_data_df[order(missing_data_df$missing_perc), ]
tail(missing_data_df_sorted)


traits_code <- traits_code %>%
  select(!c(names(missing_perc[missing_perc > 50])))
print(names(missing_perc[missing_perc > 50])) #FT rejected


# Filling gaps with missForest package. 
# missForest is a machine learning 
library(missForest)

imputed_data <- missForest(traits_code) #default ntrees = 100
# Access the imputed data
imputed_traits <- imputed_data$ximp
imputed_data$OOBerror # high error. Should we go ahead?


head(imputed_traits)
summary(imputed_traits)



#Hacer larga: 

traits_long <- traits_code %>%
  pivot_longer(
    cols = -c(species, code, family, genus_level, species_level, sampling_level, life_cycle), #unchanged columns
    names_to = "functional_trait",              # Name for the new "variable" column
    values_to = "ft_value"               # Name for the new "value" column
  ) 

traits_torilis <- subset(traits_long, genus_level == "Torilis")
traits_poaceae <- subset(traits_long, family == "Poaceae" & sampling_level == "family")
traits_asteraceae <- subset(traits_long, family == "Asteraceae" & sampling_level == "family")
#traits_brasiccaceae <- subset(traits_code, family == "Brassicaceae")
traits_orchidaceae <- subset(traits_long, family == "Orchidaceae")
traits_species <- subset(traits_long, sampling_level == "species")

#Crear una funcion que haga un summarize de las bases de datos estas para calcular el functional traits medios
# Qué pasa con los NA



#Luego, juntar todas las bases de datos con traits_species



