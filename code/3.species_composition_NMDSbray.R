

# probar con log(abundance) u otras transformaciones 



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, DT, viridis, ggrepel, codyn, vegan, eulerr, ggplot2, ggthemes, ggpubr, ggforce )#
#source("code/1.first_script.R"); rm(list = setdiff(ls(), c("flora_abrich", "biomass_imp", "biomass_noimp")))


flora_abrich <- read.csv("data/flora_abrich.csv")



source("code/palettes_labels.R")
palette <- palette_CB
labels <- labels1



species_ab_sampling <- flora_abrich %>% 
  group_by(code, date, sampling, treatment) %>% 
  summarise(abundance = mean(abundance_s, na.rm = T))

species_ab_sampling <- species_ab_sampling %>% 
  mutate(id = paste0(as.character(treatment), "/" , as.character(sampling)))

species_ab_sampling <- species_ab_sampling %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))




####### DIFFERENCES AT SAMPLING x TREATMENT LEVEL #############

treats <- unique(flora_abrich$treatment)

list1 <- list()
gglist1 <- list()
gglist2 <- list()
count = 0

for(i in 1:length(treats)){
  
  count = count + 1
  
  list1[[count]] <- subset(species_ab_sampling, treatment == treats[i])
  
  sp_wide <- list1[[count]] %>%
    pivot_wider(id_cols = sampling,
                names_from = code,
                values_from = abundance,
                values_fill = list(abundance = 0)) %>% 
    column_to_rownames(var = "sampling") %>% 
    arrange(as.numeric(rownames(.)))
  
  
  #Relative abundance
  
  # Perform NMDS using Bray-Curtis distance
  nmds_res <- metaMDS(sp_wide, distance = "bray", k = 2, trymax = 250, maxit = 999) 
  
  # Extract NMDS sample scores
  nmds_samples <- as.data.frame(scores(nmds_res, display = "sites"))
  
  # Extract NMDS species scores (optional)
  nmds_species <- as.data.frame(scores(nmds_res, display = "species"))
  
  gglist1[[count]] <- ggplot() +
    geom_text_repel(data = nmds_species %>% 
                      rownames_to_column(var = "sp"),
                    aes(x = NMDS1, y = NMDS2, label = sp),
                    color = "grey",
                    max.overlaps = 30) +
    geom_point(data = nmds_samples %>% 
                 rownames_to_column(var = "sampling"),
               aes(x = NMDS1, y = NMDS2),
               size = 1.5) +
    geom_text_repel(data = nmds_samples %>% 
                      rownames_to_column(var = "sampling"),
                    aes(x = NMDS1, y = NMDS2, label = sampling),
                    max.overlaps = 9) +
    geom_path(data = nmds_samples %>% 
                rownames_to_column(var = "sampling"),
              aes(x = NMDS1, y = NMDS2)) +
    geom_hline(aes(yintercept = 0), color = "gray52", linetype = "dashed") +
    geom_vline(aes(xintercept = 0), color = "gray52", linetype = "dashed") +
    labs(title = paste("NMDS using Bray-Curtis:", treats[i], sep = " "),
         subtitle = paste0("Stress = ", round(nmds_res$stress, 3)),
         x = "NMDS1",
         y = "NMDS2")
  
  
}


ggarrange(
  gglist1[[2]],
  gglist1[[1]],
  gglist1[[3]],
  gglist1[[4]], 
  ncol = 2, nrow = 2)


#https://rpubs.com/CPEL/NMDS
#We will do so using the metaMDS function. When running an NMDS you will have to identify your distance matrix
#(island.spp_distmat), your distance metric which should match the distance metric from your distance matrix 
#(here we use "bray"), your selected number of dimensions ("k"), your max number of iterations (usually **"maxit = 999"**),
#and the maximum number of random starts (usually "trymax = 250"). You may need to do a couple of runs with different 
#trymax values, especially if you are working with community data with a lot pf 0’s. Finally,
#wascores is a method of calculating species scores, default is TRUE. Check the metaMDS help file for
#other options to further customize your ordination if necessary.

#As a rule of thumb literature has identified the following cut-off values for stress-level:
#  
#  Higher than 0.2 is poor (risks for false interpretation).
#0.1 - 0.2 is fair (some distances can be misleading for interpretation).
#0.05 - 0.1 is good (can be confident in inferences from plot).
#Less than 0.05 is excellent (this can be rare).





list_sorensen <- list()
list_sorensen_df <- list()
samps <- sort(unique(flora_abrich$sampling))

for(i in 1:length(samps)){
  
  sp_wide <- species_ab_sampling %>% 
    filter(sampling == samps[i]) %>% 
    pivot_wider(id_cols = c(sampling, date, treatment, id),
                names_from = code,
                values_from = abundance,
                values_fill = list(abundance = 0)) %>% 
    ungroup() %>% 
    as_tibble()
  
  
  abundance_data <- sp_wide %>%
    select(-treatment, -sampling, -date, -id)
  
    sorensen <- vegdist(abundance_data, method = "bray", binary = TRUE)
    sorensen <- as.matrix(sorensen)  
    rownames(sorensen) <- sp_wide$id
    colnames(sorensen) <- sp_wide$id
    sorensen[upper.tri(sorensen)] <- NA
    
    sorensen_df <- sorensen %>% 
      as.data.frame() %>%
      rownames_to_column(var = "row_name") %>%
      pivot_longer(-row_name, names_to = "col_name", values_to = "value") %>% 
      filter(!is.na(value))
  
  list_sorensen[[i]] <- sorensen
  list_sorensen_df[[i]] <- sorensen_df
  
  
}


print(list_sorensen)


sorensen_df <- bind_rows(list_sorensen_df) %>% 
  filter(!value == "0") %>%
  separate(col_name, into = c("treatment_x", "sampling_x"), sep = "/") %>%
  separate(row_name, into = c("treatment_y", "sampling_y"), sep = "/") %>% 
  select(-sampling_x) %>% 
  rename(sampling = sampling_y) %>% 
  mutate(sampling = factor(as.numeric(sampling), levels = sort(unique(as.numeric(sampling))))) %>% 
  arrange(sampling) %>% 
  mutate(comparison = paste0(treatment_x, "-", treatment_y)) %>% 
  select(-treatment_x, -treatment_y) %>% 
  mutate(comparison = ifelse(comparison == "p-c", "c-p", comparison),
         comparison = ifelse(comparison == "w-c", "c-w", comparison),
         comparison = ifelse(comparison == "wp-c", "c-wp", comparison))

{gg_sorensen <- 
sorensen_df %>% 
  filter(comparison %in% c("c-p", "c-w", "c-wp")) %>% 
ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  facet_wrap(~ comparison, labeller = labeller(
    RR_descriptor = as_labeller(labels_RR))) + 
  geom_point() + 
  geom_line() +
  geom_smooth(
    se = TRUE, aes(color = comparison, fill = comparison),
    method = "lm", span = 0.6, alpha = 0.2 ) + 
  scale_color_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  scale_fill_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  labs( y = "Beta-diversity Sorensen") + 
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) + 
  labs(x = "Sampling") + 
  theme(legend.position = "bottom")
print(gg_sorensen)
ggsave("results/Plots/protofinal/sorensen_c.png", plot = gg_sorensen, dpi = 300)}


{gg_sorensen_wp <- 
sorensen_df %>% 
  filter(comparison %in% c("p-wp", "w-wp")) %>% 
  ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  facet_wrap(~ comparison) + 
  geom_point() + 
  geom_line() +
  geom_smooth(
    se = TRUE, aes(color = comparison, fill = comparison),
    method = "lm", span = 0.6, alpha = 0.2 ) + 
  scale_color_manual(values = c("w-wp" = "#D08A00", "p-wp" = "#3A3A3A")) +
  scale_fill_manual(values = c("w-wp" = "#D08A00", "p-wp" = "#3A3A3A")) +
  labs( y = "Beta-diversity Sorensen") + 
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) + 
  labs(x = "Sampling") + 
  theme(legend.position = "bottom")
print(gg_sorensen_wp)
ggsave("results/Plots/protofinal/sorensen_wp.png", plot = gg_sorensen_wp, dpi = 300)}







##############################################################################
#### 1. SPECIES COMPOSITION DIFFERENCES AT SAMPLING LEVEL #######################
##############################################################################


########### 1. VISUALIZATION: NMDS ##########################

i = 1
# 1: no abundance transformation
# 2: Hellinger transformation of abundance


{
  sp_wide_sampling <- species_ab_sampling %>%
  pivot_wider(id_cols = c(sampling, date, treatment, id),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) %>% 
  ungroup() %>% 
  as_tibble()

# create a distance matrix 
abundance_data_sampling <- sp_wide_sampling %>% 
  select(-treatment, -sampling, -date, -id)

# Optional step in which I transform abundance data sampling in Hellinger abundances
abundance_data_sampling_hellinger <- vegan::decostand(abundance_data_sampling, method = "hellinger")

abundance_list <- list(abundance_data_sampling, abundance_data_sampling_hellinger)
# Compute Bray-Curtis distance matrix

distance_matrix_sampling_bc <- vegan::vegdist(as.data.frame(abundance_list[[i]]), method = "bray")

# Run NMDS (2 dimensions, 100 tries)
nmds_bc_sampling <- metaMDS(distance_matrix_sampling_bc, k = 2, trymax = 250, maxit = 999)

# Extract NMDS coordinates
nmds_df_sampling <- data.frame(
  NMDS1 = nmds_bc_sampling$points[, 1],
  NMDS2 = nmds_bc_sampling$points[, 2],
  treatment = sp_wide_sampling$treatment,
  sampling = sp_wide_sampling$sampling,
  date = sp_wide_sampling$date
)

# Arrange by sampling order
nmds_df_sampling <- nmds_df_sampling %>% arrange(sampling)




# Species arrows visualization

set.seed(123)  # reproducibility for envfit permutations
fit <- vegan::envfit(nmds_bc_sampling, as.data.frame(abundance_list[[i]]), permutations = 999)

# Extract species scores (vectors) 

sp_scores <- as.data.frame(fit$vectors$arrows) %>%
  mutate(p = fit$vectors$pvals,
         R2 = fit$vectors$r,
         species = rownames(.)) %>%
  filter(p < 0.05, R2 > 0.15) %>%    # Filter significant and high correlation scores                             
  mutate(NMDS1 = NMDS1 * R2 * 1.5,     # Scaling arrows
         NMDS2 = NMDS2 * R2 * 1.5)     # Scaling arrows




# Plot NMDS results using ggplot

ggnmds_alltreatments <- 
  ggplot(nmds_df_sampling, aes(x = NMDS1, y = NMDS2, color = treatment)) +
    
    
    stat_ellipse(geom = "polygon", aes(fill = treatment),
                 alpha = 0.12, show.legend = FALSE, level = 0.68) + 
    
    geom_point(size = 1.5, aes(shape = treatment), show.legend = T) +
    
    geom_text_repel(aes(label = sampling),
                    max.overlaps = 3,
                    size = 3,
                    show.legend = F) +
  
    geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
    
    geom_path(aes(group = treatment), linewidth = 0.5, alpha = 0.2) +
    
    geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  
    geom_segment(data = sp_scores,
                 aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.1, "cm")),
                 colour = "black", 
                 alpha = 0.5) +
    
    geom_text(data = sp_scores,
              aes(x = NMDS1, y = NMDS2, label = species),
              hjust = 0.5, vjust = -0.3, size = 3,
              colour = "black",
              alpha = 0.5) +
   
    scale_color_manual(values = palette_CB, labels = labels, guide = "legend") +
    
    scale_fill_manual(values = palette_CB, guide = "none" ) +
    
    scale_shape_manual(values = point_shapes, guide = "none") +
    
    labs(
      #title = "NMDS Bray-Curtis: mean abundance of species at sampling level",
         subtitle = paste0("Stress = ", round(nmds_bc_sampling$stress, 3)),
         x = "NMDS1", y = "NMDS2", color = " ") +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      text = element_text(size = 15),
      legend.position = "bottom"
    )

print(ggnmds_alltreatments)

}

#ggsave("results/Plots/protofinal/species_composition_sampling_LABELS.png", plot = ggnmds_alltreatments, dpi = 300)





########### 1.2. STATISTICAL ANALYSIS: PERMANOVA ##########################

adonis_sampling <- adonis2(
  distance_matrix_sampling_bc ~ treatment,  # puedes agregar más variables si quieres
  data = sp_wide_sampling,                 # debe tener las variables explicativas
  permutations = 999,                      # número de permutaciones
  method = "bray"
)

# Mostrar resultados
print(adonis_sampling)
# Hay un efecto significativo del tratamiento sobre la composición de especies (p = 0.001). 
# El tratamiento explica aproximadamente el 33.7% de la variación en la composición.

bd <- betadisper(distance_matrix_sampling_bc, sp_wide_sampling$treatment)
anova(bd)
# El resultado de ANOVA para las dispersiónes dentro de grupos (tratamientos) es significativo (p = 0.0004).
# Esto significa que la variabilidad o dispersión dentro de al menos un grupo es diferente respecto a otros grupos.
permutest(bd) 
plot(bd)
boxplot(bd)

permutest(bd, pairwise = TRUE)
TukeyHSD(bd)

library(pairwiseAdonis)

# Ejecutamos las comparaciones por pares
pw_adonis <- pairwise.adonis(
  x           = distance_matrix_sampling_bc,                 # tu matriz de distancias Hellinger–Bray
  factors     = sp_wide_sampling$treatment,  # factor con los cuatro tratamientos
  perm        = 999,                         # número de permutaciones
  p.adjust.m  = "BH"                         # corrección de p por Benjamini–Hochberg
)

print(pw_adonis)







# Chat gpt:

##| Unlike Principal Coordinate Analysis (PCoA) or Principal Component Analysis (PCA),
##| where each axis has an explained variance, Non-Metric Multidimensional Scaling (NMDS)
##| does not provide a direct percentage of variance explained per axis. 
##| However, you can estimate how much each NMDS axis contributes to the representation of 
##| the distances by using the correlation between the NMDS axes and the original distance matrix.
##| You can calculate the squared correlation (R²) between the original distance matrix and each NMDS axis.
##|  This gives you an approximation of how well each axis represents the distances.

cor1 <- cor(distance_matrix_sampling_bc, dist(nmds_bc_sampling$points[,1]), method = "pearson") 
cor2 <- cor(distance_matrix_sampling_bc, dist(nmds_bc_sampling$points[,2]), method = "pearson") 

# Compute percentage explained by each axis
explained_NMDS1 <- cor1^2 / (cor1^2 + cor2^2) * 100
explained_NMDS2 <- cor2^2 / (cor1^2 + cor2^2) * 100

# Print results
cat("NMDS1 explains:", round(explained_NMDS1, 2), "%\n")
cat("NMDS2 explains:", round(explained_NMDS2, 2), "%\n")






##############################################################################
#### 2. SPECIES COMPOSITION DIFFERENCES AT PLOT LEVEL ###########################
##############################################################################


####################### 2.1. VISUALIZATION: NMDS ##############################

{i = 1
# 1: no abundance transformation
# 2: Hellinger transformation of abundance


species_ab_plot <- flora_abrich %>% 
  select(date, code, sampling, plot, treatment, family, genus_level, species_level, abundance_s)

species_ab_plot <- species_ab_plot %>% 
  mutate(id = paste0(treatment, "/", sampling, "/", plot))

species_ab_plot <- species_ab_plot %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))

sp_wide_plot <- species_ab_plot %>%
  pivot_wider(id_cols = c(sampling, date, treatment, plot),
              names_from = code,
              values_from = abundance_s,
              values_fill = list(abundance = 0)) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

sp_wide_plot %>% write.csv("data/relative_abudance_species_time.csv")

abundance_data_plot <- sp_wide_plot %>% select(-treatment, -sampling, -date, -plot)

# Optional step in which I transform abundance data sampling in Hellinger abundances
abundance_data_plot_hellinger <- vegan::decostand(abundance_data_plot, method = "hellinger")

abundance_plot_list <- list(abundance_data_plot, abundance_data_plot_hellinger)

# Compute Bray-Curtis distance matrix
distance_matrix_bc_plot <- vegan::vegdist(as.data.frame(abundance_plot_list[[i]]), method = "bray")

# Run NMDS (2 dimensions, 250 tries)
# here I expand the dimensions to 3 because stress = 0.23 if k = 2. We acn take a look to NMDS3
# With k = 3, stress = 0.16
nmds_bc_plot <- metaMDS(distance_matrix_bc_plot, k = 3, trymax = 250, maxit = 999)

nmds_bc_plot$stress

# Extract NMDS coordinates
nmds_df_plot <- data.frame(
  NMDS1 = nmds_bc_plot$points[, 1],
  NMDS2 = nmds_bc_plot$points[, 2],
  NMDS3 = nmds_bc_plot$points[, 3],
  treatment = sp_wide_plot$treatment,
  sampling = sp_wide_plot$sampling,
  plot = sp_wide_plot$plot,
  date = sp_wide_plot$date
)


}

# Species arrows visualization

{set.seed(123)  # reproducibility for envfit permutations
fit_plot <- vegan::envfit(nmds_bc_plot, as.data.frame(abundance_plot_list[[i]]), permutations = 999)


sp_scores_plot <- as.data.frame(fit$vectors$arrows) %>%
  mutate(p = fit_plot$vectors$pvals,
         R2 = fit_plot$vectors$r,
         species = rownames(.))
  #     )%>%
  #filter(p < 0.05, R2 > 0.15) %>%    # Filter significant and high correlation scores                             
  #mutate(NMDS1 = NMDS1 * R2 * 3,     # Scaling arrows
  #       NMDS2 = NMDS2 * R2 * 3)     # Scaling arrows
  #


ggNMDS12_allplots <-
  ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS2, color = treatment)) +
    
 
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.12, show.legend = FALSE, level = 0.68) + 
  
    
  geom_point(size = 1.5, aes(shape= treatment), show.legend = T) +
    
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
    
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
    
    geom_segment(data = sp_scores_plot,
                 aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.1, "cm")),
                 colour = "black", 
                 alpha = 0.5) +
    
    geom_text_repel(data = sp_scores_plot,
              aes(x = NMDS1, y = NMDS2, label = species),
              hjust = 0.5, vjust = -0.3, size = 3,
              colour = "black",
              alpha = 0.5) +
    
    
  scale_color_manual(values = palette_CB, labels = labels, guide = "legend") +
  
  scale_fill_manual(values = palette_CB, guide = "none" ) +
  
  scale_shape_manual(values = point_shapes, guide = "none") +
    
    
  labs(
    #title = "NMDS Bray-Curtis: abundance of species at plot level",
    subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
    x = "NMDS1", y = "NMDS2", color = "Treatment"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(size = 15),
    legend.position = "bottom"
  )
print(ggNMDS12_allplots)

}

#ggsave("results/Plots/protofinal/species_composition_plot_LABELS.png", plot = ggNMDS12_allplots, dpi = 300)





# Arrange by sampling order

min(nmds_df_plot$NMDS1)
min(nmds_df_plot$NMDS2)
min(nmds_df_plot$NMDS3)

#Changes on NMDS values to avoid pressence of 0 and negative values since log(RR) do not work with those


#nmds_df_plot<- nmds_df_plot %>% 
#  mutate(NMDS1 = NMDS1 + abs(min(nmds_df_plot$NMDS1)) + 1,
#         NMDS2 = NMDS2 + abs(min(nmds_df_plot$NMDS2)) + 1,
#         NMDS3 = NMDS3 + abs(min(nmds_df_plot$NMDS3)) + 1)

nmds_df_plot %>%  write.csv("data/nmds_df_plot_hellinger.csv", row.names = F)


####################### 2.2. STATISTICAL ANALYSIS: PERMANOVA ##############################


## Diferences between treatments? 

adonis_plot <- adonis2(
  distance_matrix_bc_plot ~ treatment,  # puedes agregar más variables si quieres
  data = sp_wide_plot,                 # debe tener las variables explicativas
  permutations = 999,                      # número de permutaciones
  method = "bray"
)

print(adonis_plot)

## Differences due to dispersion within treatments?
bd <- betadisper(distance_matrix_bc_plot, sp_wide_plot$treatment)
anova(bd) # we got significant differences in dispersion. So adonis can be influences by this

permutest(bd) 
plot(bd)
boxplot(bd)

permutest(bd, pairwise = TRUE)
TukeyHSD(bd)

library(pairwiseAdonis)

# Ejecutamos las comparaciones por pares
pw_adonis <- pairwise.adonis(
  x           = distance_matrix_bc_plot,                 # tu matriz de distancias Hellinger–Bray
  factors     = sp_wide_plot$treatment,  # factor con los cuatro tratamientos
  perm        = 999,                         # número de permutaciones
  p.adjust.m  = "BH"                         # corrección de p por Benjamini–Hochberg
)

print(pw_adonis)


ggNMDS13_allplots <-
  ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS3, color = treatment)) +
    stat_ellipse(geom = "polygon", aes(fill = treatment),
                  alpha = 0.1, show.legend = FALSE, level = 0.9) + 
  geom_point(size = 1.5, aes(shape= treatment)) +
  #geom_text_repel(aes(label = paste0(sampling, "," ,plot)), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  #geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  labs(title = "NMDS Bray-Curtis: abundance of species at plot level",
       subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
         x = "NMDS1", y = "NMDS3", color = "Treatment")
  # Print the plot


ggNMDS23_allplots<-
  ggplot(nmds_df_plot, aes(x = NMDS2, y = NMDS3, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.9) + 
  geom_point(size = 1.5, aes(shape= treatment)) +
  #geom_text_repel(aes(label = paste0(sampling, "," ,plot)), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  #geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  labs(title = "NMDS Bray-Curtis: abundance of species at plot level",
       subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
         x = "NMDS2", y = "NMDS3", color = "Treatment")
  # Print the plot


# NMDS con distancias hellinger
source("code/nmds_bray_withhellinger_plot.R")



##############################################################################
#### SPECIES COMPOSITION DIFFERENCES AT PLOT X SAMPLING LEVEL ###########################
##############################################################################


# Initialize a list to store NMDS results
samps <- sort(unique(species_ab_plot$sampling))
nmds_list <- list()
stress_values <- data.frame()

for (i in seq_along(samps)) {
  # Filter for each sampling event
  sp_wide_subset <- species_ab_plot %>%
    filter(sampling == samps[i]) %>%
    pivot_wider(id_cols = c(sampling, date, treatment, plot),
                names_from = code,
                values_from = abundance_s,
                values_fill = list(abundance = 0))%>%
    mutate(across(everything(), ~ replace_na(., 0)))
  
  # Prepare abundance data
  abundance_data_subset <- sp_wide_subset %>% select(-treatment, -sampling, -date, -plot)
  
  # Compute Bray-Curtis distance
  if (nrow(abundance_data_subset) > 1) {  # Avoid NMDS failure for single-row cases
    distance_matrix_bc <- vegan::vegdist(abundance_data_subset, method = "bray")
    
    # Run NMDS
    nmds_result <- metaMDS(distance_matrix_bc, k = 2, trymax = 250, maxit = 999)
    
    # Store NMDS scores and stress values
    nmds_df_temp <- data.frame(
      NMDS1 = nmds_result$points[, 1],
      NMDS2 = nmds_result$points[, 2],
      treatment = sp_wide_subset$treatment,
      sampling = sp_wide_subset$sampling,
      plot = sp_wide_subset$plot,
      date = sp_wide_subset$date
    )
    
    # Store stress values
    stress_values <- rbind(stress_values, data.frame(sampling = samps[i], stress = round(nmds_result$stress, 3)))
    
    # Store NMDS results
    nmds_list[[i]] <- nmds_df_temp
  }
}

# Combine NMDS results into one dataframe
nmds_df_1x1sampling <- bind_rows(nmds_list)

# Merge stress values into NMDS dataframe
nmds_df_1x1sampling <- left_join(nmds_df_1x1sampling, stress_values, by = "sampling")

nmds_df_1x1sampling <- nmds_df_1x1sampling %>% 
  mutate(date_sampling = as.factor(date))

# Create NMDS plot with stress values in facet labels


{gg_samplings <- 
ggplot(nmds_df_1x1sampling, aes(x = NMDS1, y = NMDS2, color = treatment, fill = treatment)) +
  #facet_wrap(~ paste0(sampling, " (Stress = ", stress, ")"), ncol = 5, nrow = 5) +
  facet_wrap(~ date_sampling, ncol = 5, nrow = 5, scales = "free") + 
  geom_point(size = 1.5) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.12, show.legend = FALSE, level = 0.68) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
  scale_colour_manual(values = palette_CB) +
  scale_fill_manual(values = palette_CB) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
    
    scale_color_manual(values = palette_CB, labels = labels, guide = "legend") +
    
    scale_fill_manual(values = palette_CB, guide = "none" ) +
    
    scale_shape_manual(values = point_shapes, guide = "none") +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9, face = "bold"), # Adjust facet label size
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    #legend.axis.line = element_text(size = 12)
  ) + 
  

  labs(color = "Treatment")

print(gg_samplings)
}

ggsave("results/Plots/protofinal/species_composition_allsamplings.png", plot = gg_samplings, dpi = 300)

