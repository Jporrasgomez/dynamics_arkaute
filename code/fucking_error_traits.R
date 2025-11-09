




########################################
## CALCULATING MEAN VALUES FOR TRAITS
#######################################

#Calculating the average traits per taxonomic groups: torilis sp, poaceae, asteraceae and orchidaceae
sp_poaceae <- c("Avena sterilis", "Bromus hordeaceus", "Bromus sterilis", "Cynosurus echinatus",
                "Elymus repens", "Hordeum murinum", "Poa annua", "Poa bulbosa",
                "Lolium perenne", "Gaudinia fragilis")

sp_asteraceae <- c("Crepis capillaris", "Hypochaeris radicata", "Leontodon hispidus")

sp_orchidaceae <- c("Anacamptis pyramidalis", "Ophrys apifera")

sp_torilis <- c("Torilis nodosa", "Torilis arvensis")


labels_sp <- c("poaceae", "asteraceae", "orchidaceae", "tosp")

list_sp <- list(sp_poaceae, sp_asteraceae, sp_orchidaceae, sp_torilis)
                   # 1          # 2            # 3             # 4
trait_values_species <- list()
trait_values_aggtax <- list()

for (i in c(1:4)){
  
  
  data1 <- traits_cleaned %>% 
    filter(species %in% list_sp[[i]]) %>% 
    group_by(trait_name, species, code) %>% 
    summarize(species_mean_trait = mean(trait_value, na.rm = T),
              species_sd_trait   = sd(trait_value, na.rm = T), 
              n_obs_sp           = n())
  
  
  
  trait_values_species[[i]] <- data1 
  
  data2 <- data1 %>% 
    group_by(trait_name) %>% 
    summarize(trait_mean       = mean(species_mean_trait, na.rm = T),
              trait_sd = sd(species_mean_trait, na.rm = T),
              ) %>% 
    mutate(code = labels_sp[i])
  
  trait_values_aggtax[[i]] <- data2
                
  
}


trait_means_poaceae_splevel <- trait_values_species[[1]] %>%  print()
trait_means_asteraceae_splevel <- trait_values_species[[2]] %>%  print()
trait_means_orchidaceae_splevel <- trait_values_species[[3]] %>%  print()
trait_means_tosp_splevel <- trait_values_species[[4]] %>%  print()


trait_means_poaceae <- trait_values_aggtax[[1]] %>%  print()
trait_means_asteraceae <- trait_values_aggtax[[2]] %>%  print()
trait_means_orchidaceae <- trait_values_aggtax[[3]] %>%  print()
trait_means_tosp <- trait_values_aggtax[[4]] %>%  print()




sp_all <- c(sp_poaceae, sp_asteraceae, sp_orchidaceae, sp_torilis)

sp_others <- setdiff(unique(traits_cleaned$species), sp_all)


trait_means_others0 <- traits_cleaned %>% 
  filter(species %in% sp_others) %>% 
  group_by(trait_name, species, code) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T),
            trait_sd   = sd(trait_value, na.rm = T), 
            n_obs_sp           = n(), 
            )


trait_means_others <- trait_means_others0 %>% 
  ungroup() %>%
  select(trait_name, trait_mean, trait_sd, code)
  



trait_means <- bind_rows(trait_means_others,
                         trait_means_poaceae,
                         trait_means_asteraceae,
                         trait_means_orchidaceae,
                         trait_means_tosp)


