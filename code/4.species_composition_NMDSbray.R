

# probar con log(abundance) u otras transformaciones 



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, DT, viridis, ggrepel, codyn, vegan, eulerr, ggplot2, ggthemes, ggpubr, ggforce )#
source("code/1.first_script.R"); rm(list = setdiff(ls(), c("flora_abrich", "biomass_imp", "biomass_noimp")))

source("code/palettes_labels.R")
palette <- palette5
labels <- labels3



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
print(list_jaccard)

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


sorensen_df %>% 
  filter(comparison %in% c("c-p", "c-w", "c-wp")) %>% 
ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  facet_wrap(~ comparison) + 
  geom_point() + 
  geom_line() +
  geom_smooth(
    se = TRUE, aes(color = comparison, fill = comparison),
    method = "lm", span = 0.6, alpha = 0.2 ) + 
  scale_color_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  scale_fill_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  labs( y = "Beta-diversity Sorensen")

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
  labs( y = "Beta-diversity Sorensen")



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

# Compute Bray-Curtis distance matrix
distance_matrix_sampling_bc <- vegan::vegdist(abundance_data_sampling, method = "bray")

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

# Plot NMDS results using ggplot

ggnmds_alltreatments <- ggplot(nmds_df_sampling, aes(x = NMDS1, y = NMDS2, color = treatment)) +
    stat_ellipse(geom = "polygon", aes(fill = treatment),
                 alpha = 0.1, show.legend = FALSE, level = 0.9) + 
    geom_point(size = 1.5, aes(shape = treatment), show.legend =T) +
    geom_text_repel(aes(label = sampling), max.overlaps = 100, size = 3, show.legend = F) +
    geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
    geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
    geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
    scale_color_manual(values = palette, labels = labels, guide = "legend") +
    scale_fill_manual(values = palette, guide = "none" ) +
    scale_shape_manual(values = point_shapes, guide = "none") +
    labs(title = "NMDS Bray-Curtis: mean abundance of species at sampling level",
         subtitle = paste0("Stress = ", round(nmds_bc_sampling$stress, 3)),
         x = "NMDS1", y = "NMDS2", color = " ") +
    theme(legend.position = "bottom")
  # Print the plot




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



ggNMDS1_dynamics_sampling <- 
ggplot(nmds_df_sampling, aes(x = date, y = NMDS1, color = treatment, fill = treatment)) +
  geom_point(show.legend = TRUE) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.9, alpha = 0.1, show.legend = TRUE
  )+
  geom_path(aes(group = treatment), show.legend = FALSE) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette, guide = "none") +
  labs(title = "NMDS1 Bray-Curtis: mean abundance at sampling level ",
       subtitle = paste0("Stress = ", round(nmds_bc_sampling$stress, 3)),
       x = "Date", y = "NMDS1", color = "Treatment")


ggNMDS2_dynamics_sampling <- 
ggplot(nmds_df_sampling, aes(x = date, y = NMDS2, color = treatment, fill = treatment)) +
  geom_point(show.legend = TRUE) +  # Hide extra legend from points
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.9, alpha = 0.1, show.legend = TRUE # Keep one legend
  ) +
  geom_path(aes(group = treatment), show.legend = FALSE) + # Hide extra legend from paths
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette, guide = "none") +  # Hide fill legend
  labs(title = "NMDS1 Bray-Curtis: mean abundance at sampling level ",
       subtitle = paste0("Stress = ", round(nmds_bc_sampling$stress, 3)),
       x = "Date", y = "NMDS2", color = "Treatment")



########## DIFFERENCES AT TREATMENT x SAMPLING x PLOT level ##


# NMDS for abundance of species at plot level in the same matrix
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

abundance_data_plot <- sp_wide_plot %>% select(-treatment, -sampling, -date, -plot)

# Compute Bray-Curtis distance matrix
distance_matrix_bc_plot <- vegan::vegdist(abundance_data_plot, method = "bray")

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

# Arrange by sampling order

min(nmds_df_plot$NMDS1)
min(nmds_df_plot$NMDS2)
min(nmds_df_plot$NMDS3)

# Changes on NMDS values to avoid pressence of 0 and negative values since log(RR) do not work with those

nmds_df_plot<- nmds_df_plot %>% 
  mutate(NMDS1 = NMDS1 + abs(min(nmds_df_plot$NMDS1)) + 1,
         NMDS2 = NMDS2 + abs(min(nmds_df_plot$NMDS2)) + 1,
         NMDS3 = NMDS3 + abs(min(nmds_df_plot$NMDS3)) + 1)

nmds_df_plot %>%  write.csv("data/nmds_df_plot.csv", row.names = F)

source("code/meta_function/meta_function.R")

meta_function(nmds_df_plot, "NMDS1", "treatment")
gg_all1n
gg_delta_RR

source("code/meta_function/RR_TREATMENT_c.R")
RR_treatment_c(nmds_df_plot, "NMDS1")
gg_RR


source("code/meta_function/RR_TREATMENT_wp.R")
RR_treatment_wp(nmds_df_plot, "NMDS1")
gg_RR_wp


cor1 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,1]), method = "pearson")
cor2 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,2]), method = "pearson")
cor3 <- cor(distance_matrix_bc_plot, dist(nmds_bc_plot$points[,3]), method = "pearson")


print(explained_NMDS1 <- (cor1^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)
print(explained_NMDS2 <- (cor2^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)
print(explained_NMDS3 <- (cor3^2 / (cor1^2 + cor2^2 + cor3^2)) * 100)



ggNMDS12_allplots <-
  ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS2, color = treatment)) +
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
         x = "NMDS1", y = "NMDS2", color = "Treatment")
  # Print the plot

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



hist(nmds_df_plot$NMDS1, breaks = 50)


ggNMDS2_boxplot_plot <- 
  ggplot(nmds_df_plot, aes(y = NMDS2, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = palette) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))




###### DIFFERENCES AT TREATMENT x SAMPLING x PLOT level but with different distance-matrix at sampling level 


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


# Create NMDS plot with stress values in facet labels

gg_samplings <- 
ggplot(nmds_df_1x1sampling, aes(x = NMDS1, y = NMDS2, color = treatment, fill = treatment)) +
  #facet_wrap(~ paste0(sampling, " (Stress = ", stress, ")"), ncol = 5, nrow = 5) +
  facet_wrap(~ sampling, ncol = 5, nrow = 5, scales = "free") + 
  geom_point(size = 1.5) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.2, show.legend = FALSE, level = 0.68) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10), # Adjust facet label size
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  ) + 
  labs(color = "Treatment")


nmds_df_plot_matrixxsampling <- nmds_df_1x1sampling

min(nmds_df_plot_matrixxsampling$NMDS1)
min(nmds_df_plot_matrixxsampling$NMDS2)

nmds_df_plot_matrixxsampling <- nmds_df_plot_matrixxsampling %>%
  mutate(NMDS1 = NMDS1 + abs(min(nmds_df_plot_matrixxsampling$NMDS1)) + 1,
         NMDS2 = NMDS2 + abs(min(nmds_df_plot_matrixxsampling$NMDS2)) + 1)

nmds_df_plot_matrixxsampling %>%
  write.csv("data/nmds_df_plot_matrixxsampling.csv", row.names = F)

source("code/meta_function/meta_function.R")

meta_function(nmds_df_plot_matrixxsampling, "NMDS1", "treatment")

gg_all1n
gg_delta_RR

source("code/meta_function/RR_TREATMENT_c.R")
RR_treatment_c(nmds_df_plot_matrixxsampling, "NMDS1")
gg_RR


source("code/meta_function/RR_TREATMENT_wp.R")
RR_treatment_wp(nmds_df_plot_matrixxsampling, "NMDS1")
gg_RR_wp

