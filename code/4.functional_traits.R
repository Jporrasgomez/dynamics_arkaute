


rm(list = ls(all.names = TRUE))  #Se limpia el environment
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) #se quitan todos los paquetes (limpiamos R)
pacman::p_load(dplyr, tidyr, tidyverse, ggplot2, BIEN, ape, maps, sf, rtry, ggrepel)



#Outliers of TRAITS"

flora_abrich <- read.csv("data/flora_abrich.csv")
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
  filter(!code %in% c("chsp", "amsp")) #Actually we have no trait info about "cisp" and "casp"



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


setdiff(unique(species_code$code), unique(traits_mean$code))
setdiff(unique(species_code$code), unique(traits$code))

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
# These are: libi and rapa. 
# Before deleting them, we can check the relevance of these species (in checking.results we can do it)
# rapa is a species with a lot of abundance and n_observations, but not one of the highest

traits_mean_wide <- traits_mean_wide %>% 
  filter(!code %in% c("libi", "rapa")) %>% 
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


################ CHOOSING TRAITS ##############################################################################

##
traits_final <- traits_mean_wide %>% 
  select(c("code",
           "SLA", 
           "LDMC",
           "leafN",
           #"LA", 
           #"seed.mass", 
           #"vegetation.height"
           ))  ### Choose here the FUNCTIONAL TRAITS WE WANT

#################################################################################################################


#Now we have 2 traits above 50 % NA, 1 = 50% and 1 around 45%. I would delete them all
# given that the following one is araound 15%

# We now have only 5 NA in trait leafN 

# Prepare the database for the FD function: 

traits_matrix <- as.data.frame(traits_final)

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
 
{
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

loadings_df <- loadings_df %>%
  mutate(trait = recode(trait,
                        "leafN"     = "Leaf-Nitrogen",
                        "seed.mass" = "Seed mass",
                        "vegetation.height" = "Height"
  ))
# Explained variance
eig_values <- pca_sampling0$sdev^2
var_explained <- round(100 * eig_values / sum(eig_values), 1)

# Final PCA plot
# Añade la columna de sampling
pca_sampling$sampling <- cwm_sampling$sampling

# Gráfico PCA final con numeración y líneas
gg_cwm_sampling <- 
ggplot(pca_sampling, aes(x = PC1, y = PC2, color = treatment, shape = treatment)) +
  geom_path(aes(group = treatment), linewidth = 0.5, alpha = 0.2) +  # Conecta puntos del mismo tratamiento
  geom_point(size = 1.5) +
  geom_text_repel(aes(label = sampling, color = treatment),
                  size = 3,
                  max.overlaps = Inf,
                  show.legend = F)  +  # Números de sampling
  stat_ellipse(aes(fill = treatment, color = treatment),
               alpha = 0.12,
               geom = "polygon",
               level = 0.68,
               type = "norm",
               linewidth = 0.6, 
               show.legend = F) +
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
  
  scale_color_manual(values = palette_CB, labels = labels3) +
  
  scale_fill_manual(values = palette_CB) +
  
  scale_shape_manual(values = point_shapes, labels = labels3) +
  
  labs(x = paste0("PC1 (", var_explained[1], "%)"),
       y = paste0("PC2 (", var_explained[2], "%)"),
       #title = "CWM differences at sampling level"
       ) +
  guides(color = guide_legend(title = NULL),
         shape = guide_legend(title = NULL),
         fill = "none") +
  #theme_test() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(size = 15),
    legend.position = "bottom"
  )
print(gg_cwm_sampling)
ggsave("results/Plots/protofinal/FT_cwm_sampling.png", plot = gg_cwm_sampling, dpi = 300)

  }

##############################################################################
# Functional-trait PERMANOVA workflow on retained principal components (PCs) #
##############################################################################

# 1. Select the minimum number of PCs that together explain ≥ 80 % of variance
# ---------------------------------------------------------------------------
var_exp   <- (pca_sampling0$sdev^2) / sum(pca_sampling0$sdev^2)  # variance explained by each PC
cum_var   <- cumsum(var_exp)                                     # cumulative variance curve
k_retener <- which(cum_var >= 0.80)[1]      # first index at or above 80 %
print(k_retener)

# 2. Build a scores data frame and add the treatment factor
# ---------------------------------------------------------
pc_scores <- as.data.frame(pca_sampling0$x[, 1:k_retener])       # PC coordinates for each sample
pc_scores$treatment <- cwm_sampling$treatment                    # metadata: treatment as factor

# 3. PERMANOVA on a Euclidean distance matrix of the retained PCs
# ---------------------------------------------------------------
dist_pc <- vegan::vegdist(pc_scores[, 1:k_retener], method = "euclidean")  # distance matrix
adonis_pc <- adonis2(                                                      # permutation MANOVA
  dist_pc ~ treatment,
  data         = pc_scores,
  permutations = 999,
  method       = "euclidean"
)
print(adonis_pc)  # F-ratio, R² and p-value for the treatment effect

# 4. Test homogeneity of dispersions (beta diversity) among treatments
# --------------------------------------------------------------------
bd_pc <- betadisper(dist_pc, pc_scores$treatment)  # distances to group centroids
anova(bd_pc)                                       # permutational ANOVA for dispersion
TukeyHSD(bd_pc)                                    # pairwise dispersion differences

# 5. Pairwise PERMANOVA contrasts with Benjamini–Hochberg p-adjustment
# --------------------------------------------------------------------
library(pairwiseAdonis)
pw_pc <- pairwise.adonis(
  dist_pc,
  factors    = pc_scores$treatment,
  perm       = 999,
  p.adjust.m = "BH"       # controls false-discovery rate
)
print(pw_pc)  # F, R², and adjusted p for every treatment pair

# 6. (Optional) Repeat dispersion test using the original cwm_sampling object
#    – included here only if you want to compare results
# ---------------------------------------------------------------------------
beta <- betadisper(dist_pc, cwm_sampling$treatment)
anova(beta)  # duplicate dispersion test (should match bd_pc)
plot(beta)   # visual assessment of within-group spread






















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

{
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

loadings_df_plot <- loadings_df_plot %>%
  mutate(trait = recode(trait,
                        "leafN"     = "Leaf-Nitrogen",
                        "seed.mass" = "Seed mass",
                        "vegetation.height" = "Height"))
# Explained variance
eig_values <- pca_plot0$sdev^2
var_explained <- round(100 * eig_values / sum(eig_values), 1)





# Gráfico PCA final con numeración y líneas
gg_cwm_plot <- 
ggplot(pca_plot, aes(x = PC1, y = PC2, color = treatment, shape = treatment)) +
  geom_point(size = 1.5) +
  stat_ellipse(aes(fill = treatment, color = treatment),
               alpha = 0.12,
               geom = "polygon",
               level = 0.68,
               type = "norm",
               linewidth = 0.6, 
               show.legend = F) +
  
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
  
  scale_color_manual(values = palette_CB, labels = labels1) +
  
  scale_fill_manual(values = palette_CB) +
  
  scale_shape_manual(values = point_shapes, labels = labels1) +
  
  labs(x = paste0("PC1 (", var_explained[1], "%)"),
       y = paste0("PC2 (", var_explained[2], "%)"),
       #title = "CWM differences: abundance of species at plot level"
       ) +
  
  guides(color = guide_legend(title = NULL),
         shape = guide_legend(title = NULL),
         fill = "none") +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(size = 15),
    legend.position = "bottom"
  )
print(gg_cwm_plot)
ggsave("results/Plots/protofinal/FT_cwm_plot.png", plot = gg_cwm_plot, dpi = 300)

}




##############################################################################
# Functional-trait PERMANOVA workflow on retained principal components (PCs) #
##############################################################################

# 1. Select the minimum number of PCs that together explain ≥ 80 % of variance
# ---------------------------------------------------------------------------
var_exp_plot   <- (pca_plot0$sdev^2) / sum(pca_plot0$sdev^2)  # variance explained by each PC
cum_var_plot   <- cumsum(var_exp_plot)                                     # cumulative variance curve
k_retener_plot <- which(cum_var_plot >= 0.80)[1]      # first index at or above 80 %
print(k_retener_plot)

# 2. Build a scores data frame and add the treatment factor
# ---------------------------------------------------------
pc_scores_plot <- as.data.frame(pca_plot0$x[, 1:k_retener_plot])       # PC coordinates for each sample
pc_scores_plot$treatment <- cwm_plot$treatment                    # metadata: treatment as factor

# 3. PERMANOVA on a Euclidean distance matrix of the retained PCs
# ---------------------------------------------------------------
dist_pc_plot <- vegan::vegdist(pc_scores_plot[, 1:k_retener_plot], method = "euclidean")  # distance matrix
adonis_pc_plot <- adonis2(                                                      # permutation MANOVA
  dist_pc_plot ~ treatment,
  data         = pc_scores_plot,
  permutations = 999,
  method       = "euclidean"
)
print(adonis_pc_plot)  # F-ratio, R² and p-value for the treatment effect

# 4. Test homogeneity of dispersions (beta diversity) among treatments
# --------------------------------------------------------------------
bd_pc_plot <- betadisper(dist_pc_plot, pc_scores_plot$treatment)  # distances to group centroids
anova(bd_pc_plot)                                       # permutational ANOVA for dispersion
TukeyHSD(bd_pc_plot)                                    # pairwise dispersion differences

# 5. Pairwise PERMANOVA contrasts with Benjamini–Hochberg p-adjustment
# --------------------------------------------------------------------
library(pairwiseAdonis)
pw_pc_plot <- pairwise.adonis(
  dist_pc_plot,
  factors    = pc_scores_plot$treatment,
  perm       = 999,
  p.adjust.m = "BH"       # controls false-discovery rate
)
print(pw_pc_plot)  # F, R², and adjusted p for every treatment pair

# 6. (Optional) Repeat dispersion test using the original cwm_sampling object
#    – included here only if you want to compare results
# ---------------------------------------------------------------------------
beta_plot <- betadisper(dist_pc_plot, cwm_plot$treatment)
anova(beta_plot)  # duplicate dispersion test (should match bd_pc)
plot(beta_plot)   # visual assessment of within-group spread





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


pca_cwm_plot <- pca_plot %>%
  merge(sampling_dates)

pca_cwm_plot$PC1 <- pca_cwm_plot$PC1 + abs(min(pca_cwm_plot$PC1)) + 1
pca_cwm_plot$PC2 <- pca_cwm_plot$PC2 + abs(min(pca_cwm_plot$PC2)) + 1
        

pca_cwm_plot %>%  write.csv("data/pca_cwm_plot.csv", row.names = F)

cwm_plot_db <- cwm_plot %>% 
  merge(sampling_dates)

cwm_plot_db %>%  write.csv("data/cwm_plot_db.csv")


trait_levels <- c("LDMC", "leafN", "SLA")

source("code/meta_function/meta_function.R")
source("code/meta_function/RR_TREATMENT_c.R")
source("code/meta_function/RR_TREATMENT_wp.R")

palette <- palette_CB
labels <- labels2


list_c <- list()
list_wp <- list()

for(i in seq_along(trait_levels)){
  
RR_treatment_c(cwm_plot_db, trait_levels[i])
list_c[[i]] <- RR_treatment

RR_treatment_wp(cwm_plot_db, trait_levels[i])
list_wp[[i]] <- RR_wp_vs_treatment 

}

RR_c <- do.call(rbind, list_c) %>% 
  select(RR_descriptor, RR, se_RR, variable)
RR_wp <- do.call(rbind, list_wp)

RR_whole_aggregated <- rbind(RR_c, RR_wp) %>% 
  mutate(
  variable = as.factor(variable)
) %>% 
  mutate(
    upper_limit = RR + se_RR * 1.96,   # calculating interval of confidence
    lower_limit = RR - se_RR * 1.96
  ) %>% 
  mutate(
    null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"))

scales_data <- list()
for(i in seq_along(trait_levels)){
  
  scale_i <- RR_whole_aggregated %>% 
    filter(variable == trait_levels[i]) %>% 
    mutate(
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))
    )
  
  scales_data[[i]] <- scale_i
  
}

RR_whole_aggregated <- do.call(rbind, scales_data)





limits_variables <- c("LDMC", "leafN", "SLA")
labels_variables <- c("LDMC", "Leaf N", "SLA")



{gg_RR_cwm <- 
    
RR_whole_aggregated %>% 
    filter(RR_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>%
    mutate(variable = factor(variable, 
                             levels = limits_variables, 
                             labels = labels_variables)) %>% 
    mutate( variable = fct_rev(variable)) %>% 
  ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
  geom_errorbar(
    aes(xmin = lower_limit,
        xmax = upper_limit),
    linewidth = 0.5,
    position = position_dodge(width = 0.2),
    width = 0.1
  ) +  
  geom_point(position = position_dodge(width = 0.2)) + 
    
  scale_color_manual(values = palette_RR_CB, labels = labels_RR2) +
    
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    
    geom_text(
      aes(
        y = variable,
        x = ifelse(RR < 0, lower_limit - scale/600,  upper_limit + scale/600),
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor,
        size = 13,
      ),
      position = position_dodge2(width = 0.2, preserve = "single"),
      show.legend = F
    )+
    
  labs(y = NULL,
       x = NULL,
       color = NULL) +
    
  gg_RR_theme
  
print(gg_RR_cwm)
}



#ggsave("results/Plots/protofinal/FT_cwm_treatment_effects.png", plot = gg_RR_cwm, dpi = 300)

{gg_RR_cwm_wp <- 
    
    RR_whole_aggregated %>% 
    filter(RR_descriptor %in% c("wp_vs_p")) %>% 
    mutate(variable = factor(variable, 
                             levels = limits_variables, 
                             labels = labels_variables)) %>% 
    
    mutate( variable = fct_rev(variable)) %>% 
    
    ggplot(aes(y = variable, x = RR, color = RR_descriptor)) + 
    geom_errorbar(
      aes(xmin = lower_limit,
          xmax = upper_limit),
      linewidth = 0.5,
      position = position_dodge(width = 0.2),
      width = 0.1
    ) +  
    geom_point(position = position_dodge(width = 0.2)) + 
    
    scale_color_manual(values = palette_RR_wp, labels = labels_RR2) +
    
    scale_y_discrete(
      limits = trait_levels,
      labels = trait_labels
    ) +
    
    geom_vline(xintercept = 0, linetype = "dashed", color = p_CB) +
    
    geom_text(
      aes(
        y = variable,
        x = ifelse(RR < 0, lower_limit - scale/600,  upper_limit + scale/600),
        label = ifelse(null_effect == "NO", "*", NA_character_),
        color = RR_descriptor,
        size = 13,
      ),
      position = position_dodge2(width = 0.2, preserve = "single"),
      show.legend = F
    )+
    
    labs(y = NULL,
         x = NULL,
         color = NULL) +
    
    gg_RR_theme
  
  print(gg_RR_cwm_wp)
}




## DYNAMIS
{
list_dyn_c <- list()
list_dyn_wp <- list()

for(i in seq_along(trait_levels)){

meta_function(cwm_plot_db, trait_levels[i], "treatment")
list_dyn_c[[i]] <- RR_treatment
list_dyn_wp[[i]] <- RR_wp_vs_treatment

}


RR_dyn_c <- do.call(rbind, list_dyn_c) %>% 
  select(sampling, date, RR_descriptor, variable, delta_RR, se_delta_RR)
RR_dyn_wp <- do.call(rbind, list_dyn_wp) %>% 
  select(sampling, date, RR_descriptor, variable, delta_RR, se_delta_RR)



RR_whole_dynamics <- rbind(RR_dyn_c, RR_dyn_wp) %>% 
  mutate(
    variable = as.factor(variable)
  ) %>% 
  mutate(
    upper_limit = delta_RR + se_delta_RR * 1.96,   # calculating interval of confidence
    lower_limit = delta_RR - se_delta_RR * 1.96
  ) %>% 
  mutate(
    null_effect = ifelse(lower_limit <= 0 & upper_limit >= 0, "YES","NO"))

scales_data <- list()
for(i in seq_along(trait_levels)){
  
  scale_i <- RR_whole_dynamics %>% 
    filter(variable == trait_levels[i]) %>% 
    mutate(
      scale = (max(abs(upper_limit)) + max(abs(lower_limit)))
    )
  
  scales_data[[i]] <- scale_i
  
}

RR_whole_dynamics <- do.call(rbind, scales_data)


}




{gg_cwm_dynamics <- 
    RR_whole_dynamics %>% 
    filter(RR_descriptor %in% c("p_vs_c", "w_vs_c", "wp_vs_c")) %>%
    mutate(variable = factor(variable, 
                             levels = limits_variables, 
                             labels = labels_variables)) %>% 
    mutate(RR_descriptor = factor(RR_descriptor,
                                  levels = c("wp_vs_c", "w_vs_c", "p_vs_c"))) %>%
    
      ggplot(aes(x = date, y = delta_RR)) + 
      
      facet_grid(variable ~ RR_descriptor, scales = "free_y",
             labeller = labeller(
               RR_descriptor = as_labeller(labels_RR2))) + 
    
      geom_errorbar(aes(
        ymin = lower_limit,
        ymax = upper_limit,
        color = RR_descriptor),
        alpha = 0.5,
        linewidth = 0.5) +
    
      geom_point(aes(color = RR_descriptor), size = 1.1) + 
    
      geom_line(aes(color = RR_descriptor), linewidth = 0.5) +
    
      scale_color_manual(values = palette_RR_CB) + 
    
    geom_text(aes(
      x = date,
      y = ifelse(delta_RR < 0, lower_limit -  scale/9, upper_limit +  scale/9),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = RR_descriptor
    ),
    position = position_dodge2(width = 0.7, preserve = "single"),
    size = 5,
    show.legend = FALSE
    ) +
    
      geom_hline(yintercept= 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    
      geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    
    gg_RR_theme +
  
    labs(x = NULL, y = NULL) +
    
    theme(
      strip.text.y            = element_blank(),
      strip.background        = element_blank(),
      strip.text.x = element_blank())
  
print(gg_cwm_dynamics)
#ggsave("results/Plots/protofinal/FT_cwm_dynamics.png", plot = gg_cwm_dynamics, dpi = 300)
}

{gg_cwm_wp_dynamics <- 
    RR_whole_dynamics %>% 
    filter(RR_descriptor %in% c("wp_vs_p")) %>%
    mutate(variable = factor(variable, 
                             levels = trait_limits, 
                             labels = trait_labels)) %>% 
    
    ggplot(aes(x = date, y = delta_RR)) + 
    
    facet_grid(variable ~ RR_descriptor, scales = "free_y",
               labeller = labeller(
                 RR_descriptor = as_labeller(labels_RR2))) + 
    
    geom_errorbar(aes(
      ymin = lower_limit,
      ymax = upper_limit,
      color = RR_descriptor),
      alpha = 0.5, 
      linewidht = 0.5) +
    
    geom_point(aes(color = RR_descriptor), size = 1.1) + 
    
    geom_line(aes(color = RR_descriptor), linewidht = 0.5) +
    
    scale_color_manual(values = palette_RR_wp) + 
    
    geom_text(aes(
      x = date,
      y = ifelse(delta_RR < 0, lower_limit -  scale/9, upper_limit +  scale/9),
      label = ifelse(null_effect == "NO", "*", NA_character_),
      color = RR_descriptor
    ),
    position = position_dodge2(width = 0.7, preserve = "single"),
    size = 5,
    show.legend = FALSE
    ) +
    
    geom_hline(yintercept= 0, linetype = "dashed", color = p_CB, linewidht = 0.5) +
    
    geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
    
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    
    gg_RR_theme +
    
    labs(x = NULL, y = NULL) +
    
    theme(
      strip.text.y            = element_blank(),
      strip.background        = element_blank(),
      strip.text.x = element_blank())
  
  print(gg_cwm_wp_dynamics)
  #ggsave("results/Plots/protofinal/FT_cwm_dynamics.png", plot = gg_cwm_dynamics, dpi = 300)
}

    


## JOINING
library(ggpubr)

gg_Warming_Effect <- 
  ggarrange(
    gg_RR_cwm_wp   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_cwm_wp_dynamics + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol     = 2, 
    widths   = c(1, 4)    # A ocupará 1/(1+2)=1/3 del ancho, B 2/3
  )
print(gg_Warming_Effect)
ggsave("results/Plots/protofinal/1.Warming_Effect.png", plot = gg_Warming_Effect, dpi = 300)




gg_Results_traits <- 
  ggarrange(
    gg_RR_cwm   + theme(plot.margin = margin(5,5,5,5)),   # margen uniforme
    gg_cwm_dynamics + theme(plot.margin = margin(5,5,5,5)),
    #labels   = c("A","B"),
    ncol     = 2, 
    widths   = c(1, 5)    # A ocupará 1/(1+2)=1/3 del ancho, B 2/3
  )
print(gg_Results_traits)
ggsave("results/Plots/protofinal/1.Results.png", plot = gg_Results, dpi = 300)

