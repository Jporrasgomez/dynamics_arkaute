


########### follow the 'Workflow for general data preprocessing using rtry' ###############
library(rtry)
library(tidyverse)


#import TRY data

#datasets released from TRY are in a long-table format, where the traits are defined in the columns TraitID and TraitName. 
#Ancillary data are defined in the columns DataID and DataName, which also provide additional information for the traits.
TRYdata1 <- rtry_import('data/traits/31423.txt')
View(TRYdata1)

#Explore the imported data
#a first understanding of the data, an additional column is added to show the total count within each group. 
#sorted by TraitID
TRYdata1_explore_trait <- rtry_explore(TRYdata1, TraitID, TraitName)
View(TRYdata1_explore_trait)

#sorted by AccSpeciesID
TRYdata1_explore_species <- rtry_explore(TRYdata1, AccSpeciesID, AccSpeciesName, TraitID, TraitName)
View(TRYdata1_explore_species)

#which ancillary data are provided within the dataset. 
# Group the input data based on DataID, DataName, TraitID and TraitName
# and sort the output by TraitID using the sortBy argument, to see if there are similar data in each trait.
TRYdata1_explore_anc <- rtry_explore(TRYdata1, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(TRYdata1_explore_anc)

#Select columns
workdata <- rtry_remove_col(TRYdata1, V29)

#Select rows
View(TRYdata1_explore_anc)

#To select all trait records and the ancillary data of interest:
# 59 Latitude
# 60 Longitude
# 61 Altitude
# 6601 Sampling date
# 327 Exposition
# 413 Plant developmental status / plant age / maturity / plant life stage
# 1961 Health status of plants (vitality)
# 113 Reference / source
workdata <- rtry_select_row(workdata, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113))
workdata_explore_anc <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

#################### Exclude (remove) data ######################
#8.1 Exclude (remove) observations of juvenile plants
# Select the rows where DataID is 413, i.e. the data containing the plant development status
tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 413)

# Then explore the unique values of the OrigValueStr within the selected data
tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = OrigValueStr)
unique(tmp_unfiltered$OrigValueStr)

# Criteria
# 1. DataID equals to 413
# 2. OrigValueStr equals to "juvenile" ect.
workdata <- rtry_exclude(workdata, 
                         (DataID %in% 413) & (OrigValueStr %in% c("juvenile", "seedlings", "Juvenile","immature", "juvenile, 11-14 weeks")), 
                         baseOn = ObservationID)
#check again
# Select the rows where DataID is 413, i.e. the data containing the plant development status
# Then explore the unique values of the OrigValueStr within the selected data
tmp_filtered <- rtry_select_row(workdata, DataID %in% 413)
tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = OrigValueStr)
unique(tmp_filtered$OrigValueStr)

# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
workdata_explore_anc_excluded <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# #8.2 Exclude observations without geo-referenced information and from irrelevant regions
# # Select only the geo-referenced observations, i.e. with DataID 59 Latitude
# # Set getAncillary to TRUE to obtain (keep) all traits and ancillary data
# workdata <- rtry_select_row(workdata, DataID %in% 59, getAncillary = TRUE)
# 
# # Select the rows that contain DataID 59, i.e. latitude information
# # Then explore the unique values of the StdValue within the selected data
# tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 59)
# tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
# 
# # Exclude observations using latitude information
# # Criteria
# # 1. DataID equals to 59
# # 2. StdValue smaller than 40 or NA
# workdata <- rtry_exclude(workdata, (DataID %in% 59) & (StdValue < 40 | is.na(StdValue)), baseOn = ObservationID)
# 
# #check again
# # Select the rows where DataID is 59 (Latitude)
# # Then explore the unique values of the StdValue within the selected data
# # Sort the exploration by StdValue
# tmp_filtered <- rtry_select_row(workdata, DataID %in% 59)
# tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
# 
# #Filter according to longitude
# # Select only the geo-referenced observations with DataID 60 Longitude
# # Set getAncillary to TRUE to obtain (keep) all traits and ancillary data
# workdata <- rtry_select_row(workdata, DataID %in% 60, getAncillary = TRUE)
# 
# # Select the rows that contain DataID 60, i.e. longitude information
# # Then explore the unique values of the StdValue within the selected data
# tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 60)
# tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
# 
# # Exclude observations using longitude information
# # Criteria
# # 1. DataID equals to 60
# # 2. StdValue smaller than 10 or larger than 60 or NA
# workdata <- rtry_exclude(workdata, (DataID %in% 60) & (StdValue < 10 | StdValue > 60 | is.na(StdValue)), baseOn = ObservationID)
# 
# #check again
# # Select the rows where DataID is 60 (Longitude)
# # Then explore the unique values of the StdValue within the selected data
# # Sort the exploration by StdValue
# tmp_filtered <- rtry_select_row(workdata, DataID %in% 60)
# tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)

#8.3 Exclude non-representative sub-traits
# Group the input data based on DataID, DataName, TraitID and TraitName
# Then sort the output by TraitID using the sortBy argument
#tmp_unfiltered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)


#8.4 Exclude data according to standard values (StdValue)
# Select the rows where DataID is 6582, 6583 and 6584, i.e. the data containing the SLA information
# Then explore the unique values of the StdValue within the selected data
# tmp_unfiltered <- rtry_select_row(workdata, DataID %in% c(6582, 6583, 6584))
# tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, Comment, sortBy = StdValue)

# Criteria
# 1. DataID equals to 6582, 6583 or 6584
# 2. StdValue smaller than 1; Do we need this?
#workdata <- rtry_exclude(workdata, (DataID %in% c(6582, 6583, 6584)) & (StdValue < 1), baseOn = ObsDataID)

#check again
# Select the rows where DataID is 6582, 6583 and 6584, i.e. the data containing the SLA information
# Then explore the unique values of the StdValue within the selected data
# tmp_filtered <- rtry_select_row(workdata, DataID %in% c(6582, 6583, 6584))
# tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, Comment, sortBy = StdValue)

#8.5 Exclude outliers according to error risk (ErrorRisk)
# Group the input data based on DataID, DataName, TraitID, TraitName and ErrorRisk
# Then sort the output by ErrorRisk using the sortBy argument
tmp_unfiltered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, ErrorRisk, sortBy = ErrorRisk)

# Criteria
# 1. ErrorRisk larger than or equal to 4
workdata <- rtry_exclude(workdata, ErrorRisk >= 4, baseOn = ObsDataID)

#check again
# Group the input data based on DataID, DataName, TraitID, TraitName and ErrorRisk
# Then sort the output by ErrorRisk using the sortBy argument
tmp_filtered <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, ErrorRisk, sortBy = ErrorRisk)

#9 Remove duplicates based on duplicate identifier (OrigObsDataID)
# Remove duplicates
workdata <- rtry_remove_dup(workdata)

#10 Transform to wide table
#-------------------------------------------------
# Exclude
# 1. All entries with "" in TraitID
# 2. Potential categorical traits that don't have a StdValue
# 3. Traits that have not yet been standardized in TRY
# Then select the relevant columns for transformation
# Note: The complete.cases() is used to ensure the cases are complete,
#       i.e. have no missing values
#-------------------------------------------------
num_traits <- rtry_select_row(workdata, complete.cases(TraitID) & complete.cases(StdValue))
num_traits <- rtry_select_col(num_traits, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName)

# individual trait values; not this one: it has repeat data
# write.csv(num_traits, "indi.trait_longform.csv", row.names = F)

#check unit
table(num_traits$TraitName, num_traits$UnitName)

#checkoutliers
# t1 = num_traits %>% dplyr::filter(TraitName == 'Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)')
# t1_sp <- sort(unique(t1$AccSpeciesName))[26:49]
# t2 = t1 %>% filter(AccSpeciesName %in% t1_sp )
# 
# ggplot(t2, aes(x = trait_value)) +
#   geom_histogram() +
#   facet_wrap(~ scrubbed_species_binomial)


# Extract the unique value of latitude (DataID 59) and longitude (DataID 60) together with the corresponding ObservationID
# workdata_georef <- rtry_select_anc(workdata, 59, 60)

# To merge the extracted ancillary data with the numerical traits
# Merge the relevant data frames based on the ObservationID using rtry_join_left (left join)
#num_traits_georef <- rtry_join_left(num_traits, workdata_georef, baseOn = ObservationID)

# Perform wide table transformation on TraitID, TraitName and UnitName
# With cell values to be the mean values calculated for StdValue
num_traits_georef_wider <- rtry_trans_wider(num_traits, names_from = c(TraitID, TraitName, UnitName),
                                            values_from = c(StdValue), values_fn = list(StdValue = mean))


# wide to long
num_traits_georef_long <- num_traits_georef_wider %>%
  pivot_longer(cols = "14_Leaf nitrogen (N) content per leaf dry mass_mg/g":"3086_Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded_mm2 mg-1", 
               names_to = "TraitName", values_to = "StdValue") %>% drop_na(StdValue)

write.csv(num_traits_georef_long, "data/traits/indi.trait_longform.csv", row.names = F)

#11 Export preprocessed TRY data
# Export the data into a CSV file
#rtry_export(num_traits_georef_wider, "workdata_wider_traits.csv")

#group by species
# mean_traits_species = num_traits_georef_wider %>% group_by(AccSpeciesID, AccSpeciesName) %>% summarise_at(2:21, mean , na.rm = T)

# write.csv(mean_traits_species, "mean_traits_species.csv", row.names = F)




