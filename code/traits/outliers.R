


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr, tidyr, tidyverse, ggplot2, BIEN, ape, maps, sf, rtry, ggrepel)




#Outliers of TRAITS"

source("code/first_script.R")  
traits <- read.csv('data/traits/all.indi.used.csv')

traits <- traits %>%
  mutate(across(where(is.character), as.factor))

species_code <- read.csv("data/species_code.csv") 


## Checking which species are absent
checking <- anti_join(traits, species_code, by = "species") %>% 
  distinct(species, .keep_all = T) #Anagallis arvensis = Lysimachia arvensis

traits <- traits %>% 
  mutate(species = recode(species, "Anagallis arvensis" = "Lysimachia arvensis"))

traits <- traits %>%
  filter(species != "Medicago polymorpha") %>%
  mutate(species = droplevels(species))


traits <- left_join(traits, species_code, by = "species") 


 traits <- traits %>% 
   select(c("code", "species", "trait_name", "trait_ID", "database", "trait_value"))


traits <- traits %>% 
  group_by(species, trait_name) %>% 
  mutate(n_observations = n()) %>% 
  ungroup()



hist(traits$n_observations, breaks = 50)


#Let's try

traits$code_n_obs <- paste(traits$code, traits$n_observations, sep = ", ")


# Remove outliers based on Z-scores
traits_cleaned <- traits %>%
  group_by(trait_name, species) %>% 
  mutate(
    mean_trait = mean(trait_value, na.rm = TRUE),
    sd_trait = sd(trait_value, na.rm = TRUE),
    z_score = (trait_value - mean_trait) / sd_trait
  ) %>%
  filter(
    n_observations <= 5 | abs(z_score) <= 4  # Do not filter outliers for n_observations <= 5
  ) %>%
  ungroup() %>%
  select(-mean_trait, -sd_trait, -z_score)  # Remove temporary columns if not needed

# View the cleaned data
outliers <- anti_join(traits, traits_cleaned)


#Calculating the average traits per taxonomic groups: torilis sp, poaceae, asteraceae and orchidaceae

traits_poaceae <- traits_cleaned %>% 
  filter(species %in% c("Avena sterilis", "Bromus hordeaceus", "Bromus sterilis", "Cynosurus echinatus", "Elymus repens", 
                        "Hordeum murinum", "Poa annua", "Poa bulbosa", "Lolium perenne", "Gaudinia fragilis"))
traits_poaceae <- traits_poaceae %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n())
traits_poaceae$code <- "poaceae"


traits_asteraceae <- traits_cleaned %>% 
  filter(species %in% c("Crepis capillaris", "Hypochaeris radicata", "Leontodon hispidus"))
traits_asteraceae <- traits_asteraceae %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n())
traits_asteraceae$code <- "asteraceae"


traits_torilis <- traits_cleaned %>% 
  filter(species %in% c("Torilis nodosa", "Torilis arvensis"))
traits_torilis <- traits_torilis %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n())
traits_torilis$code <- "tosp"

traits_orchidaceae <- traits_cleaned %>% 
  filter(species %in% c("Anacamptis pyramidalis", "Ophrys apifera"))
traits_orchidaceae <- traits_orchidaceae %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n())
traits_orchidaceae$code <- "orchidaceae"


#I delete all species that are part of all taxonomic groups and also
#Lotus corniculatus, Erophila verna and Sterllaria media because we did not measure it in the field finally.
traits_cleaned <- traits_cleaned %>% 
  filter(!species %in% c("Avena sterilis", "Bromus hordeaceus", "Bromus sterilis", "Cynosurus echinatus", "Elymus repens", 
                         "Hordeum murinum", "Poa annua", "Poa bulbosa", "Crepis capillaris", "Hypochaeris radicata", "Leontodon hispidus", 
                         "Torilis nodosa", "Torilis arvensis", "Lolium perenne", "Lotus corniculatus", "Gaudinia fragilis", "Anacamptis pyramidalis",
                         "Ophrys apifera", "Erophila verna", "Stellaria media"))




traits_mean <- traits_cleaned %>% 
  group_by(code, species, trait_name, trait_ID, database, n_observations) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T)) %>%
  rename(n_obs_traits = n_observations)

traits_mean <- bind_rows(traits_mean, traits_poaceae, traits_asteraceae, traits_torilis, traits_orchidaceae)

trait_na <- traits_mean %>% 
  filter(if_any(everything(), is.na))



setdiff(unique(traits_mean$code), unique(flora$code))
setdiff(unique(flora$code), unique(traits_mean$code))

#The function checks which species are present in traits_mean$species but not present in flora$species.
setdiff(unique(traits_mean$species), unique(flora$species))
setdiff(unique(flora$species), unique(traits_mean$species))

# I have to check on Myosotis, Sonchus and Cirsium. 

#Changes that have been made: 
# - Sonchus sp has been substituted by "asteraceae" in the flora_abrich database
# - For the species Cirsium sp, Myosotis discolor and Cardamine sp we do not have functional traits. 
# What can I do about Cirsium? it is one of the most abudance species at the end of the samplings. 


#### CWM analysis RAW ####

library(FD)
library(psych)

#Hacer unas data frames como dummy$trait y dummy$abun del paquete FD. necesito una matriz de abundancias de las comunidades. 

# !!! abundance matrix tiene que tener la misma cantidad de especies que la matrix de traits y que los codes esténen e mismo orden...


# Preparing trait matrix: 

traits_mean_wide <- traits_mean %>%
  ungroup() %>%                           # Remove grouping structure
  select(-trait_sd, -database, -n_obs_traits, -trait_ID, -species) %>%  # Remove unnecessary columns
  pivot_wider(
    names_from = trait_name,              # Columns will be based on trait_name levels
    values_from = trait_mean              # Values will come from trait_mean
  )


#Transform all SLA traits into one 
traits_mean_wide$SLA.inc <- ifelse(is.na(traits_mean_wide$SLA.inc), traits_mean_wide$SLA.ex, traits_mean_wide$SLA.inc) 

#Transform all LA traits into one 
traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA.ex, traits_mean_wide$LA.inc)
traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA.un, traits_mean_wide$LA.inc)
traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA, traits_mean_wide$LA.inc)


traits_mean_wide <- traits_mean_wide %>% 
  select(!c("SLA.ex","LA.ex", "LA.un", "LA"))

traits_mean_wide <- traits_mean_wide %>% 
  rename(SLA = SLA.inc ) %>% 
  rename(LA = LA.inc)


missing_data_df <- data.frame(
  code = traits_mean_wide$code,     # Species names from traits_mean_wide
  missing_perc = rowSums(is.na(traits_mean_wide)) / ncol(traits_mean_wide) * 100 # Percentages of missing data
)
print(missing_data_df_sorted <- missing_data_df[order(missing_data_df$missing_perc), ])

## According to Carmona et al. 2021, we have to remove the species with less than the 50% of the traits. 
# These are: libi, amsp and rapa. 

# Before deleting them, we can check the relevance of these species.  

theme_set(theme_bw() +
            theme(legend.position = "right",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

flora_abrich_nobs <- flora %>% 
  group_by(code) %>% 
  summarize(n_observations= n(),
            mean_abundance = mean(abundance))

flora_abrich_nobs$abnobs <- flora_abrich_nobs$mean_abundance * flora_abrich_nobs$n_observations

ggplot(flora_abrich_nobs, aes(x = mean_abundance, y = n_observations, label = code, color = abnobs))+
  geom_point() + 
  scale_color_gradient(low = "blue4", high = "red2") +  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )

#rapa is a species with a lot of abundance and n_observations, but not one of the highest

traits_mean_wide <- traits_mean_wide %>% 
  filter(!code %in% c("libi", "amsp", "rapa")) %>% 
  droplevels()

#Now we have 2 traits with NA's above 50%, 1 that is = 50% and 1 that is 44%. I would detele all
# given that the following one is only 14%
missing_perc <- colSums(is.na(traits_mean_wide)) / nrow(traits_mean_wide) * 100
print(missing_perc)

traits_mean_wide <- traits_mean_wide %>% 
  select(!c("rootN", "RTD", "SRL", "SSD"))

# We now have only 5 NA in trait leafN 


# Prepare the database for the FD function: 

traits_matrix <- as.data.frame(traits_mean_wide)

rownames(traits_matrix) <- traits_matrix$code

traits_matrix <- traits_matrix %>% 
  arrange(code)

traits_matrix <- traits_matrix %>% 
  select(!code)


# We can try to impute the NAs with missforest
library(missForest)

imputed_data <- missForest(traits_matrix) #default ntrees = 100
# Access the imputed data
imputed_traits <- imputed_data$ximp
imputed_data$OOBerror # Very low error. Acceptable imputation!

boxplot(traits_mean_wide$leafN)
boxplot(imputed_traits$leafN)

head(imputed_traits)
summary(imputed_traits)




### Preparing abundance matrix: 

flora_abrich <- flora %>% 
  filter(!code %in% c("cisp", "casp", "mydi",  #Deleting the species for which we do not have traits
                      "libi", "amsp", "rapa"))  #Deleting the species for which we had more than 50% of NAs

flora_abrich_wide_reference <- flora_abrich %>% 
  ungroup() %>% 
  select(sampling, date, treatment, plot, abundance, code)

flora_abrich_wide_reference$com <- paste(flora_abrich_wide_reference$sampling, flora_abrich_wide_reference$treatment, flora_abrich_wide_reference$plot, sep = "/")


flora_abrich_wide <- flora_abrich_wide_reference %>% 
  select(com, abundance, code)
flora_abrich_wide$abundance <- flora_abrich_wide$abundance/100

flora_abrich_wide <- flora_abrich_wide %>% 
  pivot_wider(
    names_from = code, 
    values_from = abundance) %>%
  select(com, sort(setdiff(names(.), "com")))


# transformation NA into 0 
codes <- unique(flora_abrich$code)
for(i in 1:length(codes)){
  flora_abrich_wide[[paste0(codes[i])]] <- ifelse(is.na(flora_abrich_wide[[paste0(codes[i])]]), 0, flora_abrich_wide[[paste0(codes[i])]])
}
  

abundance_matrix <- as.data.frame(flora_abrich_wide)
rownames(abundance_matrix) <- abundance_matrix$com
abundance_matrix <- abundance_matrix %>% 
  select(!com)




#Calculation of CMW with function functcomp

cmw <- functcomp(as.matrix(traits_matrix), as.matrix(abundance_matrix))

cmw$com <- rownames(cmw); rownames(cmw) <- NULL

cmw$sampling <- sapply(strsplit(cmw$com, "/"), function(x) x[1])
cmw$treatment <- sapply(strsplit(cmw$com, "/"), function(x) x[2])
cmw$plot <- sapply(strsplit(cmw$com, "/"), function(x) x[3])

cmw <- cmw %>% 
  select(!com)


# Plotting 

cmw_long <- cmw %>% 
  pivot_longer(cols = c("LDMC", "leafN", "SLA", "LA", "vegetation.height", "seed.mass"),
               names_to = "trait_name",
               values_to = "trait_value")


sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$date <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$date, label = TRUE)
sampling_dates$day <- day(sampling_dates$date)
sampling_dates$year <- year(sampling_dates$date)

sampling_dates <- sampling_dates %>% 
  select(sampling, date, day, month, year, one_month_window, omw_date)

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

cmw_long <- merge(cmw_long, sampling_dates)

cmw_db<- cmw_long %>% 
  group_by(sampling, treatment, trait_name) %>% 
  mutate(
    mean_trait_value = mean(trait_value, na.rm = TRUE), 
    sd_trait_value = sd(trait_value, na.rm = TRUE)
  ) %>% 
  ungroup()

#Añadir mean y sd a la base de datos para lugo añadir las barras de eror y añadir lo de dodge point para cambiarlos de lugar. 


library(ggplot2)
library(grid)

trait_levels <- unique(cmw_db$trait_name)


a_LDMC <-
  ggplot(subset(cmw_db, trait_name == trait_levels[1]),
                 aes(x = date, y = trait_value)) + 
    
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
    
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
    
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
    
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
    
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
    
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
    
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
    
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
        ) +
    
  labs(y = trait_levels[1], x = NULL) #

# Box plot
b_LDMC <- ggplot(subset(cmw_db, trait_name == trait_levels[1]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_LDMC <- a_LDMC +
  annotation_custom(
    grob = ggplotGrob(b_LDMC),
    xmin = as.Date("2023-04-20"), # Adjust position: left boundary
    xmax = as.Date("2023-09-01"), # Adjust position: right boundary
    ymin = 170, # Adjust position: bottom boundary
    ymax = 200 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_LDMC



a_leafN <-
  ggplot(subset(cmw_db, trait_name == trait_levels[2]),
         aes(x = date, y = trait_value)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = trait_levels[2], x = NULL) #

# Box plot
b_leafN <- ggplot(subset(cmw_db, trait_name == trait_levels[2]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_leafN <- a_leafN +
  annotation_custom(
    grob = ggplotGrob(b_leafN),
    xmin = as.Date("2024-06-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-05"), # Adjust position: right boundary
    ymin = 33, # Adjust position: bottom boundary
    ymax = 37 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_leafN



a_SLA <-
  ggplot(subset(cmw_db, trait_name == trait_levels[3]),
         aes(x = date, y = trait_value)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = trait_levels[3], x = NULL) #

# Box plot
b_SLA <- ggplot(subset(cmw_db, trait_name == trait_levels[3]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_SLA <- a_SLA +
  annotation_custom(
    grob = ggplotGrob(b_SLA),
    xmin = as.Date("2024-06-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-05"), # Adjust position: right boundary
    ymin = 33, # Adjust position: bottom boundary
    ymax = 39 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_SLA



a_LA <-
  ggplot(subset(cmw_db, trait_name == trait_levels[4]),
         aes(x = date, y = trait_value)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = trait_levels[4], x = NULL) #

# Box plot
b_LA <- ggplot(subset(cmw_db, trait_name == trait_levels[4]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_LA <- a_LA +
  annotation_custom(
    grob = ggplotGrob(b_LA),
    xmin = as.Date("2024-06-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-05"), # Adjust position: right boundary
    ymin = 1500, # Adjust position: bottom boundary
    ymax = 1900 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_LA




a_height <-
  ggplot(subset(cmw_db, trait_name == trait_levels[5]),
         aes(x = date, y = trait_value)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = trait_levels[5], x = NULL) #

# Box plot
b_height <- ggplot(subset(cmw_db, trait_name == trait_levels[5]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_height <- a_height +
  annotation_custom(
    grob = ggplotGrob(b_height),
    xmin = as.Date("2024-06-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-05"), # Adjust position: right boundary
    ymin = 0.52, # Adjust position: bottom boundary
    ymax = 0.63 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_height







a_seed_mass <-
  ggplot(subset(cmw_db, trait_name == trait_levels[6]),
         aes(x = date, y = trait_value)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_trait_value + sd_trait_value, ymin = mean_trait_value - sd_trait_value, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_trait_value, , color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  labs(y = "Seed mass", x = NULL) #

# Box plot
b_seed_mass <- ggplot(subset(cmw_db, trait_name == trait_levels[6]), aes(y = trait_value)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA)) + # Transparent plot background+ 
  labs(y = NULL)



# Combine plots
ggcomb_seed_mass <- a_seed_mass +
  annotation_custom(
    grob = ggplotGrob(b_seed_mass),
    xmin = as.Date("2024-06-01"), # Adjust position: left boundary
    xmax = as.Date("2024-11-05"), # Adjust position: right boundary
    ymin = 7.5, # Adjust position: bottom boundary
    ymax = 10 # Adjust position: top boundary
  )

# Render combined plot
#ggcomb_seed_mass




ggcomb_SLA
ggcomb_LA
ggcomb_LDMC
ggcomb_leafN
ggcomb_height
ggcomb_seed_mass
