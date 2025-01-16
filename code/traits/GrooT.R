library(tidyverse)
GRooTFullVersion <- read.csv("D:/RECODYN_Pro/GRooT database/GRooTFullVersion.csv", header = T, na.strings = c("", "NA"))
colnames(GRooTFullVersion)
unique(GRooTFullVersion$traitName)

# select trait we need: Specific_root_length, Root_tissue_density, Root_N_concentration
GRooT_3 = GRooTFullVersion %>% filter(traitName %in% c('Specific_root_length', 'Root_tissue_density', 'Root_N_concentration'))
colnames(GRooT_3)
unique(GRooT_3$traitName)


# combine genusTNRS and speciesTNRS to get species name
GRooT_3_species = GRooT_3 %>% unite(speciesname, genusTNRS, speciesTNRS, sep = " ")
# select species we need (29 out of 53 species); tried different names of the species
GRooT_3_select = GRooT_3_species %>% filter(speciesname %in% c('Anacamptis pyramidalis', 'Aphanes arvensis', 'Avena sterilis', 'Bellis perennis',
                                                                'Bromus hordeaceus', 'Bromus sterillis', 'Capsella bursa-pastoris', 'Carex divulsa', 
                                                                'Cerastium glomeratum', 'Convolvulus arvensis', 'Crepis capillaris', 'Cynosurus echinatus',
                                                                'Dactilys glomerata', 'Elymus repens', 'Erophila verna', 'Festuca arundinacea', 'Festuca gruporruba',
                                                                'Gaudinia fragilis', 'Geranium dissectum', 'Geranium molle', 'Hordeum murinum', 'Hypochaeris radicata',
                                                                'Kickxia spuria', 'Lamium purpureum ', 'Leontodon hispidus', 'Linum bienne', 'Lotus corniculatus',
                                                                'Lolium perenne', 'Lysimachia arvensis', 'Mercurialis annua', 'Medicago arabica', 'Medicago lupulina',
                                                                'Medicago polymorpha', 'Ophrys apifera', 'Plantago lanceolata', 'Poa annua', 'Poa bulbosa', 'Potentilla reptans',
                                                                'Ranunculus bulbosus', 'Ranunculus parviflorus', 'Rumex crispus', 'Sherardia arvensis', 'Stellaria media',
                                                                'Torilis arvensis', 'Torilis nodosa', 'Trifolium dubium', 'Trifolium repens', 'Trifolium fragiferum',
                                                                'Valerianella locusta', 'Veronica arvensis', 'Verbena officinalis', 'Veronica persica', 'Vicia sativa'))
sort(unique(GRooT_3_select$speciesname))
table(GRooT_3_select$traitName, GRooT_3_select$speciesname)

write.csv(GRooT_3_select, "GrooT_selected_3roottraits.csv", row.names = F)

