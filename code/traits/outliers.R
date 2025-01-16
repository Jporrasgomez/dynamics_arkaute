


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr, tidyr, tidyverse, ggplot2, BIEN, ape, maps, sf, rtry)

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

str(traits)


hist(traits$n_observations, breaks = 50)



species_levels <- unique(traits$species)
traits_levels <- unique(traits$trait_name)

#Let's try

traits$code_n_obs <- paste(traits$code, traits$n_observations, sep = ", ")

ggplot (subset(traits, trait_name == traits_levels[1]), #change 1 by any number between 1 and 14 to check others traits
        aes(x = trait_name, y = trait_value)) + 
  facet_wrap(~ code_n_obs, ncol = 10, nrow = 5) +
  geom_boxplot()


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

setdiff(unique(traits_mean$species), unique(flora$species))
setdiff(unique(flora$species), unique(traits_mean$species))

# I have to check on Myosotis, Sonchus and Cirsium. 

#Changes that have been made: 
# - Sonchus sp has been substituted by "asteraceae" in the flora database
# - For the species Cirsium sp, Myosotis discolor and Cardamine sp we do not have functional traits. 
# What can I do about Cirsium? it is one of the most abudance species at the end of the samplings. 


#Mirar Carmona et al. para ver con cuanto %NA's deberiamos desconsiderar usar un trait. 





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

traits_mean_wide$SLA.inc <- ifelse(is.na(traits_mean_wide$SLA.inc), traits_mean_wide$SLA.ex, traits_mean_wide$SLA.inc) 

traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA.ex, traits_mean_wide$LA.inc)
traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA.un, traits_mean_wide$LA.inc)
traits_mean_wide$LA.inc <- ifelse(is.na(traits_mean_wide$LA.inc),traits_mean_wide$LA, traits_mean_wide$LA.inc)
## what about LA? hay varios

traits_mean_wide <- traits_mean_wide %>% 
  select(!c("SLA.ex","LA.ex", "LA.un", "LA"))

traits_mean_wide <- traits_mean_wide %>% 
  rename(SLA = SLA.inc ) %>% 
  rename(LA = LA.inc)

traits_mean_wide <- traits_mean_wide %>% 
  select(c("code", "LDMC", "leafN", "SLA", "LA", "vegetation.height", "seed.mass", "SRL"))


traits_mean_wide <- as.data.frame(traits_mean_wide)

rownames(traits_mean_wide) <- traits_mean_wide$code

traits_mean_wide <- traits_mean_wide %>% 
  select(!code)


# Reorder the levels of 'code' alphabetically

traits_mean_wide <- traits_mean_wide %>% 
  arrange(code)

#Preparing abundance matrix: 

flora <- flora %>% 
  filter(!code %in% c("cisp", "casp", "mydi")) #Deleting the species for which we do not have traits


flora_wide_reference <- flora %>% 
  ungroup() %>% 
  select(sampling, treatment, plot, abundance, code)
flora_wide_reference$com <- paste(flora_wide_reference$sampling, flora_wide_reference$treatment, flora_wide_reference$plot, sep = "/")


flora_wide <- flora_wide_reference %>% 
  select(com, abundance, code)
flora_wide$abundance <- flora_wide$abundance/100

flora_wide <- flora_wide %>% 
  pivot_wider(
    names_from = code, 
    values_from = abundance) %>%
  select(com, sort(setdiff(names(.), "com")))


#############TRANSFORMAR TODOS NAs en 0s
codes <- unique(traits_mean$code)
for(i in 1:codes){
  flora_wide
}
  


flora_wide <- as.data.frame(flora_wide)
rownames(flora_wide) <- flora_wide$com
flora_wide <- flora_wide %>% 
  select(!com)




rownames(data_frame) <- data_frame$col1
cmw <- functcomp(as.matrix(traits_mean_wide), as.matrix(flora_wide))


dummy$trait
dummy$abun




traits_flora0 <- merge(flora, traits_mean, by = "code")
traits_flora0$abundance <- traits_flora0$abundance/100

traits_flora <- traits_flora0 %>% 
  group_by(sampling, sampling_date, date, treatment, plot, trait_name) %>% 
  summarize(CWM = sum(abundance*trait_mean), na.rm = T)

mean_sd_traits <- traits_flora %>%
  group_by(treatment, sampling, sampling_date, date, trait_name) %>%
  summarize(mean_CWM = mean(CWM),
            sd_CWM = sd(CWM))


# set theme for the plot
theme_set(theme_bw() +
            theme(axis.title.x = element_blank(),
                  legend.position = "NULL",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

treatment_labs_dynamics <- c("Control", "Warming", "Perturbation", "Warming and perturbation")
names(treatment_labs_dynamics) <- c("c","w", "p", "wp")

source("code/plots_functions_flora/plot_traits_dynamics.R")

plot_traits_dynamics(subset(traits_flora, trait_name == traits_levels[1]),
                     subset(mean_sd_traits, trait_name == traits_levels[1]),
                              traits_levels[1])


for (i in 1:14) {
  plot <- plot_traits_dynamics(
    subset(traits_flora, trait_name == traits_levels[i]),
    subset(mean_sd_traits, trait_name == traits_levels[i]),
    traits_levels[i]
  )
  
  print(plot)
}

i = 3 ; ggplot(
  subset(traits_flora, trait_name == traits_levels[i]),
       aes(x = date, y = CWM, color = treatment)) +
  geom_point() +
  geom_smooth(method = "loess", se = T, aes(color = treatment, fill = treatment)) +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = paste(traits_levels[i]))










