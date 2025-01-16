library(BIEN)
library(ape) #Package for working with phylogenies in R
library(maps) #Useful for making quick maps of occurrences
library(sf) # A package for spatial data
library(tidyverse)

# Trait data (49 out of 53 species)
species_vector <- c('Anacamptis pyramidalis', 'Aphanes arvensis', 'Avena sterilis', 'Bellis perennis',
                      'Bromus hordeaceus', 'Bromus sterillis', 'Capsella bursa-pastoris', 'Carex divulsa', 
                      'Cerastium glomeratum', 'Convolvulus arvensis', 'Crepis capillaris', 'Cynosurus echinatus',
                      'Dactilys glomerata', 'Elymus repens', 'Erophila verna', 'Festuca arundinacea', 'Festuca gruporruba',
                      'Gaudinia fragilis', 'Geranium dissectum', 'Geranium molle', 'Hordeum murinum', 'Hypochaeris radicata',
                      'Kickxia spuria', 'Lamium purpureum ', 'Leontodon hispidus', 'Linum bienne', 'Lotus corniculatus',
                      'Lolium perenne', 'Anagallis arvensis', 'Mercurialis annua', 'Medicago arabica', 'Medicago lupulina',
                      'Medicago polymorpha', 'Ophrys apifera', 'Plantago lanceolata', 'Poa annua', 'Poa bulbosa', 'Potentilla reptans',
                      'Ranunculus bulbosus', 'Ranunculus parviflorus', 'Rumex crispus', 'Sherardia arvensis', 'Stellaria media',
                      'Torilis arvensis', 'Torilis nodosa', 'Trifolium dubium', 'Trifolium repens', 'Trifolium fragiferum',
                      'Valerianella locusta', 'Veronica arvensis', 'Verbena officinalis', 'Veronica persica', 'Vicia sativa')

trait_bien <- BIEN_trait_species(species_vector,
                                 all.taxonomy = TRUE,
                                 political.boundaries = TRUE)

colnames(trait_bien)
sort(unique(trait_bien$trait_name))
sp <- sort(unique(trait_bien$scrubbed_species_binomial))
species_vector[!species_vector %in% sp] #4 species

# select traits we need (7 traits)
trait_vector <- c('leaf area', 'leaf area per leaf dry mass', 'leaf dry mass per leaf fresh mass',
                  'leaf nitrogen content per leaf dry mass', 'seed mass', 
                  'maximum whole plant height', 'whole plant height')

trait_bien_select = trait_bien %>% dplyr::filter(trait_name %in% trait_vector)
write.csv(trait_bien_select, "trait_bien_select.csv", row.names = F)

#check unit: the unit are unified
table(trait_bien_select$trait_name, trait_bien_select$unit)

######################## check outliers ####################
trait_bien_select$trait_value <- as.numeric(trait_bien_select$trait_value)
t1 = trait_bien_select %>% dplyr::filter(trait_name == 'whole plant height')
t1_sp <- sort(unique(t1$scrubbed_species_binomial))[26:49]
t2 = t1 %>% filter(scrubbed_species_binomial %in% t1_sp )

ggplot(t2, aes(x = trait_value)) +
  geom_histogram() +
  facet_wrap(~ scrubbed_species_binomial)

#find Q1, Q3, and interquartile range for values in points column
Q1 <- quantile(t2$trait_value, .25)
Q3 <- quantile(t2$trait_value, .75)
IQR <- IQR(t2$trait_value)
#subset data where points value is outside 1.5*IQR of Q1 and Q3
outliers <- subset(t2, t2$trait_value < (Q1 - 1.5*IQR) | t2$trait_value > (Q3 + 1.5*IQR))



