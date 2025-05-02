


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr, tidyr, tidyverse, ggplot2, BIEN, ape, maps, sf, rtry, ggrepel)



#Outliers of TRAITS"

source("code/1.first_script.R")  
rm(list = setdiff(ls(), "flora_abrich"))
traits <- read.csv('data/traits/all.indi.used.csv')
source("code/palettes_labels.R")

theme_set(theme_bw() +
            theme(legend.position = "right",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))

# I remove 8 dummy rows of flora_abrich that I use for other 
flora_abrich <- flora_abrich %>% 
  filter(!is.na(code))

traits <- traits %>%
  mutate(across(where(is.character), as.factor))

species_code <- read.csv("data/species_code.csv") %>% 
  mutate(species = recode(species, "CAPSELLA BURSA-PASTORIS" = "Capsella bursa-pastoris"))
  

## Checking which species are absent
checking <- anti_join(traits, species_code, by = "species") %>% 
  distinct(species, .keep_all = T) %>% 
  print()#Anagallis arvensis = Lysimachia arvensis

traits <- traits %>% 
  mutate(species = recode(species, "Anagallis arvensis" = "Lysimachia arvensis"))%>%
  filter(species != "Medicago polymorpha") %>%
  mutate(species = droplevels(species)) %>% 
  left_join(species_code, by = "species") %>% 
   select(c("code", "species", "trait_name", "trait_ID", "database", "trait_value")) %>% 
  group_by(species, trait_name) %>% 
  mutate(n_observations = n()) %>% 
  ungroup() %>% 
  mutate(
    code_n_obs = paste(code, n_observations, sep = ", ")
  )




# Remove species that are "sp". This is: working just with identified species up to species level. 
flora_abrich <- flora_abrich %>% 
  filter(!code %in% c("chsp", "amsp", "cisp", "casp"))

traits <- traits %>% 
  filter(!code %in% c("chsp", "amsp")) #Actually we have no tait info about "cisp" and "casp"



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

########################################
## CALCULATING MEAN VALUES FOR TRAITS
#######################################

#Calculating the average traits per taxonomic groups: torilis sp, poaceae, asteraceae and orchidaceae
sp_poaceae <- c("Avena sterilis", "Bromus hordeaceus", "Bromus sterilis", "Cynosurus echinatus",
                "Elymus repens", "Hordeum murinum", "Poa annua", "Poa bulbosa",
                "Lolium perenne", "Gaudinia fragilis")
traits_poaceae <- traits_cleaned %>% 
  filter(species %in% sp_poaceae) %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n()) %>% 
  mutate(code = "poaceae")


sp_asteraceae <- c("Crepis capillaris", "Hypochaeris radicata", "Leontodon hispidus")
traits_asteraceae <- traits_cleaned %>% 
  filter(species %in% sp_asteraceae) %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n()) %>% 
  mutate(code = "asteraceae")


sp_torilis <- c("Torilis nodosa", "Torilis arvensis")
traits_torilis <- traits_cleaned %>% 
  filter(species %in% sp_torilis) %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n()) %>% 
  mutate(code = "tosp")

sp_orchidaceae <- c("Anacamptis pyramidalis", "Ophrys apifera")
traits_orchidaceae <- traits_cleaned %>% 
  filter(species %in% sp_orchidaceae) %>% 
  group_by(trait_name) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T),
            n_obs_traits = n()) %>% 
  mutate(code = "orchidaceae")


sp_all <- c(sp_poaceae, sp_asteraceae, sp_orchidaceae, sp_torilis, "Erophila verna", "Stellaria media",
            "Lotus corniculatus")

#I delete all species that are aggregate within taxonomic groups and also
#Lotus corniculatus, Erophila verna and Sterllaria media because we did not measure them in the field finally.


traits_mean <- traits_cleaned %>% 
  filter(!species %in% sp_all) %>% 
  group_by(code, species, trait_name, trait_ID, database, n_observations) %>% 
  summarize(trait_mean = mean(trait_value, na.rm = T), 
            trait_sd = sd(trait_value, na.rm = T)) %>%
  rename(n_obs_traits = n_observations)

traits_mean <- bind_rows(traits_mean, traits_poaceae, traits_asteraceae, traits_torilis, traits_orchidaceae)

#trait_na <- traits_mean %>% 
#  filter(if_any(everything(), is.na)) %>% 
#  print()




#The function checks which species are present in traits_mean$species but not present in flora$species.
setdiff(unique(traits_mean$code), unique(flora_abrich$code))
setdiff(unique(flora_abrich$code), unique(traits_mean$code))

# I have to check on Myosotis, Sonchus and Cirsium. 

#Changes that have been made: 
# - Sonchus sp has been substituted by "asteraceae" in the flora_abrich database
# - For the species Cirsium sp, Myosotis discolor and Cardamine sp we do not have functional traits. 
# What can I do about Cirsium? it is one of the most abudance species at the end of the samplings. 



#######################################
########## CWM analysis RAW ###########
#######################################

library(FD)
library(psych)

#Hacer unas data frames como dummy$trait y dummy$abun del paquete FD. necesito una matriz de abundancias de las comunidades. 

# !!! abundance matrix tiene que tener la misma cantidad de especies que la matrix de traits y que los codes esténen e mismo orden...


####### TRAIT MATRIX: 

traits_mean_wide <- traits_mean %>%
  ungroup() %>%                           # Remove grouping structure
  select(-trait_sd, -database, -n_obs_traits, -trait_ID, -species) %>%  # Remove unnecessary columns
  pivot_wider(
    names_from = trait_name,              # Columns will be based on trait_name levels
    values_from = trait_mean              # Values will come from trait_mean
  )


##|  Treating SLA and LA traits. 
##| We have 2 traits for SLA: SLA.ex and SLA.inc
##| We have 3 traits for LA: LA.ex, LA.un and LA.inc. 
##| They all differ if they include or no the pedunculum of the leaf. 
##| When there is one trait available for a species, the others are not. So we will keep just one value per SLA and LA and species
##| 

traits_mean_wide <- traits_mean_wide %>% 
  mutate(SLA.inc = ifelse(is.na(SLA.inc), SLA.ex, SLA.inc)) %>% 
  mutate(LA.inc = ifelse(is.na(LA.inc), LA.ex, LA.inc)) %>% 
  mutate(LA.inc = ifelse(is.na(LA.inc), LA.un, LA.inc)) %>% 
  mutate(LA.inc = ifelse(is.na(LA.inc), LA, LA.inc)) %>% 
  select(!c("SLA.ex","LA.ex", "LA.un", "LA")) %>% 
  rename(SLA = SLA.inc ) %>% 
  rename(LA = LA.inc)

##  ---REMOVING SPECIES WITH LOW NUMBER OF TRAITS AVAILABLE---

traits_mean_wide %>%
  mutate(
    missing_data = rowSums(is.na(.))
  ) %>%
  select(code, missing_data) %>%
  mutate(
    code = factor(code, levels = code[order(missing_data, decreasing = TRUE)])
  ) %>%
  ggplot(aes(x = code, y = missing_data)) +
  geom_col(fill = "steelblue") +
  labs(x = "Species code", y = "% Missing traits data", 
       title = "Number of traits without data per species (n traits = 10)") +
  geom_hline(yintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
## According to Carmona et al. 2021, we have to remove the species with less than the 50% of the traits. 
# These are: libi, amsp and rapa. 
# Before deleting them, we can check the relevance of these species (in checking.results we can do it)
# rapa is a species with a lot of abundance and n_observations, but not one of the highest

traits_mean_wide <- traits_mean_wide %>% 
  filter(!code %in% c("libi", "amsp", "rapa")) %>% 
  droplevels()

## REMOVING TRAITS WITH HIGH NUMBER OF NA

missing_data <- colSums(is.na(traits_mean_wide)) / nrow(traits_mean_wide) * 100
print(missing_data)

data.frame(
  variable = names(missing_data),
  missing_perc = as.numeric(missing_data)
) %>%
  mutate(
    variable = factor(variable, levels = variable[order(missing_perc, decreasing = TRUE)])
  ) %>% 
ggplot(aes(x = variable, y = missing_perc)) +
  geom_col(fill = "seagreen") +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "Trait", y = "% Missing data", 
       title = "Percentage of Missing Trait Data per Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

traits_mean_wide <- traits_mean_wide %>% 
  select(!c("rootN", "RTD", "SRL", "SSD"))

#Now we have 2 traits above 50 % NA, 1 = 50% and 1 around 45%. I would delete them all
# given that the following one is araound 15%

# We now have only 5 NA in trait leafN 

# Prepare the database for the FD function: 

traits_matrix <- as.data.frame(traits_mean_wide)

rownames(traits_matrix) <- traits_matrix$code

traits_matrix <- traits_matrix %>% 
  arrange(code)

traits_matrix <- traits_matrix %>% 
  select(!code)



# We can try to impute the NAs with missforest,
# however, the amount of NA is so low that we consider that is not necessary

#library(missForest)

#imputed_data <- missForest(traits_matrix) #default ntrees = 100
## Access the imputed data
#imputed_traits <- imputed_data$ximp
#imputed_data$OOBerror # Very low error. Acceptable imputation!
#
#boxplot(traits_mean_wide$leafN)
#boxplot(imputed_traits$leafN)
#
#head(imputed_traits)
#summary(imputed_traits)




### ABUNDANCE MATRIX 

# We will prepare 2 abundance matrix: 1) for abundance at plot level
#                                     2) for abundance at sampling level. This is: mean abundance of each species per treatment and sampling

# Common step for both abudance matrix: 

flora_abrich <- flora_abrich %>% 
  filter(!code %in% c("cisp", "casp", "mydi",  #Deleting the species for which we do not have traits
                      "libi", "amsp", "rapa"))  #Deleting the species for which we had more than 50% of NAs





#Abundance matrix 1: sampling level


abundance_matrix_sampling <- flora_abrich %>% 
  ungroup() %>% 
  select(sampling, date, treatment, plot, abundance_s, code) %>% 
  mutate(
    abundance_s = abundance_s / 100
  ) %>% 
  group_by(treatment, sampling, code) %>% 
  summarize(mean_abundance = mean(abundance_s, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    com = paste(sampling, treatment, sep = "/")
  ) %>% 
  select(com, mean_abundance, code) %>% 
  pivot_wider(
    names_from = code, 
    values_from = mean_abundance) %>%
  select(com, sort(setdiff(names(.), "com"))) %>%
  column_to_rownames("com") %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>%  # NA = 0 
  as.data.frame() 


cwm_sampling <- functcomp(as.matrix(traits_matrix), as.matrix(abundance_matrix_sampling))
cwm_sampling$com <- rownames(cwm_sampling); rownames(cwm_sampling) <- NULL

cwm_sampling <- cwm_sampling %>% 
  mutate(
    sampling = sapply(strsplit(com, "/"), function(x) x[1]),
    treatment = sapply(strsplit(com, "/"), function(x) x[2]),
  ) %>% 
  select(-com)


library(factoextra)

cwm_sampling %>% 
  select(-treatment, - sampling) %>% 
  prcomp(center = T, scale. = T) %>% 
  fviz_pca_biplot(geom = "point", repel = T, title = " ",
                                     ggthem = theme_test())

### PCA
##cwm_sampling %>% 
##  select(-sampling, -treatment) %>% 
##  prcomp(center = TRUE, scale. = TRUE) %>% 
##  fviz_pca_biplot(geom.ind = "point",
##                  habillage = cwm_sampling$treatment,
##                  addEllipses = TRUE,
##                  ellipse.level = 0.68,
##                  palette = palette5,
##                  repel = TRUE,  # Evita la superposición de etiquetas
##                  ggtheme = theme_test()) +
##  scale_shape_manual(values = point_shapes, labels = labels3) +
##  scale_color_manual(values = palette5, labels = labels3) +
##  guides(color = guide_legend(title = NULL),    # Leyenda de color
##         shape = guide_legend(title = NULL),    # Leyenda de forma
##         fill = "none") +                      # Elimina la leyenda de las elipses (relleno)
##  theme(legend.position = "bottom",
##        legend.title = element_blank(),        # Elimina el título de la leyenda
##        legend.key = element_blank()) +        # Elimina las claves de la leyenda de color/forma
##  labs(title = "PCA mean abundance values at sampling level")
 

# PCA on CWM data (excluding metadata columns)
pca_sampling0 <- cwm_sampling %>%
  select(-sampling, -treatment) %>%
  prcomp(center = TRUE, scale. = TRUE)

# Individual coordinates (scores) + treatment info
pca_sampling <- as.data.frame(pca_sampling0$x)
pca_sampling$treatment <- cwm_sampling$treatment

# Loadings (trait vectors)
loadings_df <- as.data.frame(pca_sampling0$rotation)
loadings_df$trait <- rownames(loadings_df)
loadings_df <- loadings_df %>%
  mutate(PC1 = PC1 * 4, PC2 = PC2 * 4)  # scale factor can be adjusted (lenth of arrows)

# Explained variance
eig_values <- pca_sampling0$sdev^2
var_explained <- round(100 * eig_values / sum(eig_values), 1)

# Final PCA plot
# Añade la columna de sampling
pca_sampling$sampling <- cwm_sampling$sampling

# Gráfico PCA final con numeración y líneas
ggplot(pca_sampling, aes(x = PC1, y = PC2, color = treatment, shape = treatment)) +
  #geom_path(aes(group = treatment), linewidth = 0.5, alpha = 0.2) +  # Conecta puntos del mismo tratamiento
  geom_point(size = 1.5) +
  #geom_text_repel(aes(label = sampling, color = treatment), size = 3, max.overlaps = Inf)  +  # Números de sampling
  stat_ellipse(aes(fill = treatment, color = treatment),
               alpha = 0.2,
               geom = "polygon",
               level = 0.68,
               type = "norm",
               linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.3, "cm")),
               color = "gray30") +
  geom_text_repel(data = loadings_df,
                  aes(x = PC1, y = PC2, label = trait),
                  inherit.aes = FALSE,
                  color = "gray30",
                  max.overlaps = Inf) +
  scale_color_manual(values = palette5, labels = labels3) +
  scale_fill_manual(values = palette5) +
  scale_shape_manual(values = point_shapes, labels = labels3) +
  labs(x = paste0("PC1 (", var_explained[1], "%)"),
       y = paste0("PC2 (", var_explained[2], "%)"),
       title = "CWM differences (mean abundance) at sampling level") +
  guides(color = guide_legend(title = NULL),
         shape = guide_legend(title = NULL),
         fill = "none") +
  theme_test() +
  theme(legend.position = "bottom")












# Abundance matrix 2: plot level (dynamics)

abundance_matrix_dynamics <- flora_abrich %>% 
  ungroup() %>% 
  select(sampling, date, treatment, plot, abundance_s, code) %>% 
  mutate(
    com = paste(sampling, treatment, plot, sep = "/")
  ) %>% 
  select(com, abundance_s, code) %>% 
  mutate(abundance_s = abundance_s/100) %>% # abundance = (0,1)
  pivot_wider(
    names_from = code, 
    values_from = abundance_s) %>%
  select(com, sort(setdiff(names(.), "com"))) %>%
  column_to_rownames("com") %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>%  # NA = 0 
  as.data.frame() 


# Calculation of cwm with function functcomp

#cwm at plot level (dynamics)

cwm_plot <- FD::functcomp(as.matrix(traits_matrix), as.matrix(abundance_matrix_dynamics))
cwm_plot$com <- rownames(abundance_matrix_dynamics); rownames(cwm_plot) <- NULL


cwm_plot <- cwm_plot %>% 
  mutate(
    sampling = sapply(strsplit(com, "/"), function(x) x[1]),
    treatment = sapply(strsplit(com, "/"), function(x) x[2]),
    plot = sapply(strsplit(com, "/"), function(x) x[3])
  ) %>% 
  select(-com)


library(factoextra)


cwm_plot %>% 
  select(-treatment, - sampling, -plot) %>% 
  prcomp(center = T, scale. = T) %>% 
  fviz_pca_biplot(geom = "point", repel = T, title = " ",
                  ggthem = theme_test())

pca_plot0 <- cwm_plot %>%
  select(-sampling, -treatment, -plot) %>%
  prcomp(center = TRUE, scale. = TRUE)

# Individual coordinates (scores) + treatment info
pca_plot <- as.data.frame(pca_plot0$x)
pca_plot <- pca_plot %>% 
  mutate(sampling = cwm_plot$sampling, 
         plot = cwm_plot$plot, 
         treatment = cwm_plot$treatment)

# Loadings (trait vectors)
loadings_df_plot <- as.data.frame(pca_plot0$rotation)
loadings_df_plot$trait <- rownames(loadings_df_plot)
loadings_df_plot <- loadings_df_plot %>%
  mutate(PC1 = PC1 * 4, PC2 = PC2 * 4)  # scale factor can be adjusted (lenth of arrows)

# Explained variance
eig_values <- pca_plot0$sdev^2
var_explained <- round(100 * eig_values / sum(eig_values), 1)





# Gráfico PCA final con numeración y líneas
ggplot(pca_plot, aes(x = PC1, y = PC2, color = treatment, shape = treatment)) +
  geom_point(size = 1.5) +
  stat_ellipse(aes(fill = treatment, color = treatment),
               alpha = 0.2,
               geom = "polygon",
               level = 0.68,
               type = "norm",
               linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_segment(data = loadings_df_plot,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.3, "cm")),
               color = "gray30") +
  geom_text_repel(data = loadings_df_plot,
                  aes(x = PC1, y = PC2, label = trait),
                  inherit.aes = FALSE,
                  color = "gray30",
                  max.overlaps = Inf) +
  scale_color_manual(values = palette5, labels = labels3) +
  scale_fill_manual(values = palette5) +
  scale_shape_manual(values = point_shapes, labels = labels3) +
  labs(x = paste0("PC1 (", var_explained[1], "%)"),
       y = paste0("PC2 (", var_explained[2], "%)"),
       title = "CWM differences: abundance of species at plot level") +
  guides(color = guide_legend(title = NULL),
         shape = guide_legend(title = NULL),
         fill = "none") +
  theme_test() +
  theme(legend.position = "bottom")





# Plotting cwm dynamics


sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
  mutate(sampling = as.factor(sampling),
         date = ymd(date), 
         month = month(date, label = TRUE),
         day = day(date), 
         year = year(date)) %>% 
  select(sampling, date, year 
         #, day, month, one_month_window, omw_date
  ) %>% 
  mutate(across(where(is.character), as.factor))

cwm_plot_db <- cwm_plot %>% 
  merge(sampling_dates)


trait_levels <- c("LDMC", "leafN", "SLA", "LA", "vegetation.height", "seed.mass")

source("code/meta_function/meta_function.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")

palette <- palette5
labels <- labels3

i= 2
meta_function(cwm_plot_db, trait_levels[i], "treatment")


gg_stats_variable

# Differences at treatment level
gg_dunn_variable 
gg_ttest_variable
gg_RR_dynamics <- gg_RR
gg_RR_wp_dynamics <- gg_RR_wp

# Dynamics differences
gg_all1n
gg_facet
gg_delta_RR
gg_delta_RR_wp 

# Coefficient of variation
gg_stats_cv
gg_dunn_cv
gg_ttest_cv  
gg_dynamics_cv



list_c <- list()
list_wp <- list()

for(i in seq_along(trait_levels)){
  
RR_treatment_c(cwm_plot_db, trait_levels[i])
list_c[[i]] <- RR_treatment

RR_treatment_wp(cwm_plot_db, trait_levels[i])
list_wp[[i]] <- RR_wp_vs_treatment 

}

RR_c <- do.call(rbind, list_c)
RR_wp <- do.call(rbind, list_wp)





z = 1.96

RR_c %>% 
  ## Multiplying by -1 gamma zipf in order to have positive values and being able to read the plots as
  ## evenness
  ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_errorbar(
    aes(ymin = RR - z * se_RR,
        ymax = RR + z * se_RR),
    linewidth = 0.5,
    position = position_dodge(width = 0.2),
    width = 0.1
  ) +  
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR, labels = labels_RR) +
  scale_x_discrete(
    limits = trait_levels,
    labels = c(
      "LDMC" = "LDMC",
      "leafN" = "Leaf N",
      "SLA" = "SLA",
      "LA" = "LA",
      "vegetation.height" = "Height",
      "seed.mass" = "Seed mass"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR of CWM mean values at plot level", color = NULL) +
  theme(legend.position = "bottom")

RR_wp %>% 
  ## Multiplying by -1 gamma zipf in order to have positive values and being able to read the plots as
  ## evenness
  ggplot(aes(x = variable, y = RR, color = RR_descriptor)) + 
  geom_errorbar(
    aes(ymin = RR - z * se_RR,
        ymax = RR + z * se_RR),
    linewidth = 0.5,
    position = position_dodge(width = 0.2),
    width = 0.1
  ) +  
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_RR_wp, labels = labels_RR_wp) +
  scale_x_discrete(
    limits = trait_levels,
    labels = c(
      "LDMC" = "LDMC",
      "leafN" = "Leaf N",
      "SLA" = "SLA",
      "LA" = "LA",
      "vegetation.height" = "Height",
      "seed.mass" = "Seed mass"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR of CWM mean values at plot level", color = NULL) +
  theme(legend.position = "bottom")
