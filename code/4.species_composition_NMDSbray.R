

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


species_ab_plot <- flora_abrich %>% 
  select(date, code, sampling, plot, treatment, family, genus_level, species_level, abundance_s)

species_ab_plots <- species_ab_plot %>% 
  mutate(id = paste0(treatment, "/", sampling, "/", plot))






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


sp_wide_sampling <- species_ab_sampling %>%
  pivot_wider(id_cols = c(sampling, date, treatment, id),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) 


# create a distance matrix 
abundance_data_sampling <- sp_wide_sampling %>% 
  select(-treatment, -sampling, -date, -id)


list_sorensen <- list()
list_jaccard <- list()
list_sorensen_df <- list()
list_jaccard_df <- list()
samps <- sort(unique(flora_abrich$sampling))

for(i in i: length(samps)){
  
  sp_wide <- species_ab_sampling %>% 
    filter(sampling == samps[i]) %>% 
    pivot_wider(id_cols = c(sampling, date, treatment, id),
                names_from = code,
                values_from = abundance,
                values_fill = list(abundance = 0))
  
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
  
  
  jaccard <- vegdist(abundance_data, method = "jaccard", binary = TRUE)
  jaccard <- as.matrix(jaccard)  
  rownames(jaccard) <- sp_wide$id
  colnames(jaccard) <- sp_wide$id
  jaccard[upper.tri(jaccard)] <- NA
  
  jaccard_df <- jaccard %>% 
    as.data.frame() %>%
    rownames_to_column(var = "row_name") %>%
    pivot_longer(-row_name, names_to = "col_name", values_to = "value") %>% 
    filter(!is.na(value))
  
  list_jaccard[[i]] <- jaccard
  list_jaccard_df[[i]] <- jaccard_df

  
}


print(list_sorensen)
print(list_jaccard)

sorensen_df <- bind_rows(list_sorensen_df) %>% 
  filter(!value == "0")

sorensen_df <- sorensen_df %>%
  separate(col_name, into = c("treatment_x", "sampling_x"), sep = "/") %>%
  separate(row_name, into = c("treatment_y", "sampling_y"), sep = "/") %>% 
  select(-sampling_x) %>% 
  rename(sampling = sampling_y) %>% 
  mutate(sampling = factor(as.numeric(sampling), levels = sort(unique(as.numeric(sampling))))) %>% 
  arrange(sampling) %>% 
  mutate(comparison = paste0(treatment_x, "-", treatment_y)) %>% 
  select(-treatment_x, -treatment_y)


sorensen_df %>% 
  filter(comparison %in% c("c-p", "c-w", "c-wp")) %>% 
ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  labs( y = "Beta-diversity Sorensen")

sorensen_df %>% 
  filter(comparison %in% c("p-wp", "w-wp")) %>% 
  ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("w-wp" = "#D08A00", "p-wp" = "#3A3A3A")) +
  labs( y = "Beta-diversity Sorensen")



jaccard_df <- bind_rows(list_jaccard_df) %>% 
  filter(!value == "0")

jaccard_df <- jaccard_df %>%
  separate(col_name, into = c("treatment_x", "sampling_x"), sep = "/") %>%
  separate(row_name, into = c("treatment_y", "sampling_y"), sep = "/") %>% 
  select(-sampling_x) %>% 
  rename(sampling = sampling_y) %>% 
  mutate(sampling = factor(as.numeric(sampling), levels = sort(unique(as.numeric(sampling))))) %>% 
  arrange(sampling) %>% 
  mutate(comparison = paste0(treatment_x, "-", treatment_y )) %>% 
  select(-treatment_x, -treatment_y)


jaccard_df %>% 
  filter(comparison %in% c("c-p", "c-w", "c-wp")) %>% 
  ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  labs( y = "Beta diviersity - Jaccard")

jaccard_df %>% 
  filter(comparison %in% c("p-wp", "w-wp")) %>% 
  ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("w-wp" = "#D08A00", "p-wp" = "#3A3A3A")) +
  labs( y = "Beta-diversity Jaccard")







sp_wide_sampling <- species_ab_sampling %>% 
  pivot_wider(id_cols = c(sampling, date, treatment, id),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

abundance_data_sampling <- sp_wide_sampling %>% 
  select(-treatment, -sampling, -date, -id)

sorensen <- vegdist(abundance_data_sampling, method = "bray", binary = TRUE)
sorensen <- as.matrix(sorensen)  
rownames(sorensen) <- sp_wide$id
colnames(sorensen) <- sp_wide$id
sorensen[upper.tri(sorensen)] <- NA

sorensen_df<- sorensen %>% 
  as.data.frame() %>%
  rownames_to_column(var = "row_name") %>%
  pivot_longer(-row_name, names_to = "col_name", values_to = "value") %>% 
  filter(!is.na(value))

#sorensen_df <- sorensen_df %>%
#  separate(col_name, into = c("treatment_x", "sampling_x"), sep = "/") %>%
#  separate(row_name, into = c("treatment_y", "sampling_y"), sep = "/") %>% 
#  select(-sampling_x) %>% 
#  rename(sampling = sampling_y) %>% 
#  mutate(sampling = factor(as.numeric(sampling), levels = sort(unique(as.numeric(sampling))))) %>% 
#  arrange(sampling) %>% 
#  mutate(comparison = paste0(treatment_x, "-", treatment_y)) %>% 
#  select(-treatment_x, -treatment_y)


sorensen_df <- sorensen_df %>%
  separate(col_name, into = c("treatment_x", "sampling_x"), sep = "/") %>%
  separate(row_name, into = c("treatment_y", "sampling_y"), sep = "/") %>% 
  filter(sampling_x == sampling_y) %>% 
  select(-sampling_x) %>% 
  rename(sampling = sampling_y) %>% 
  mutate(sampling = factor(as.numeric(sampling), levels = sort(unique(as.numeric(sampling))))) %>% 
  arrange(sampling) %>% 
  mutate(comparison = paste0(treatment_x, "-", treatment_y)) %>% 
  select(-treatment_x, -treatment_y)

sorensen_df %>% 
  filter(comparison %in% c("c-p", "c-w", "c-wp")) %>% 
  ggplot(aes(x = sampling, y = value, color = comparison, group = comparison)) + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c("c-p" = "#0077FF", "c-w" = "#E0352F", "c-wp" = "#A238A2")) +
  labs( y = "Beta-diversity Sorensen")

# NMDS


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

cor1 <- cor(distance_matrix_bc, dist(nmds_bc$points[,1]), method = "pearson") 
cor2 <- cor(distance_matrix_bc, dist(nmds_bc$points[,2]), method = "pearson") 

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
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
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
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "Date", y = "NMDS2", color = "Treatment")








########## DIFFERENCES AT TREATMENT x SAMPLING x PLOT level ##


# NMDS for abundance of species at plot level in the same matrix


species_ab_plot <- species_ab_plot %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))

sp_wide_plot <- species_ab_plot %>%
  pivot_wider(id_cols = c(sampling, date, treatment, plot),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

abundance_data_plot <- sp_wide_plot %>% select(-treatment, -sampling, -date, -plot)

# Compute Bray-Curtis distance matrix
distance_matrix_bc_plot <- vegan::vegdist(abundance_data_plot, method = "bray")

# Run NMDS (2 dimensions, 250 tries)
# here I expand the dimensions to 3 because stress = 0.23 if k = 2. We acn take a look to NMDS3
# With k = 3, stress = 0.16
nmds_bc_plot <- metaMDS(distance_matrix_bc_plot, k = 3, trymax = 250, maxit = 999)

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

nmds_df_plot <- nmds_df_plot %>%
  arrange(sampling) %>% 
  group_by(treatment, sampling) %>% 
  mutate(mean_NMDS1 = mean (NMDS1, na.rm = T), 
         mean_NMDS2 = mean (NMDS2, na.rm = T), 
         mean_NMDS3 = mean (NMDS3, na.rm = T), 
         sd_NMDS1 = sd (NMDS1, na.rm = T), 
         sd_NMDS2 = sd (NMDS2, na.rm = T), 
         sd_NMDS3 = sd (NMDS3, na.rm = T)) %>% 
  mutate(cv_NMDS1 = sd_NMDS1/mean_NMDS1,
         cv_NMDS2 = sd_NMDS2/mean_NMDS2,
         cv_NMDS3 = sd_NMDS1/mean_NMDS3) %>% 
  ungroup()



nmds_df_treatmeans <- nmds_df_plot %>%
  distinct(treatment, plot, sampling, date, .keep_all = TRUE) %>% 
  group_by(treatment) %>% 
  mutate(mean_NMDS1 = mean (NMDS1, na.rm = T), 
         mean_NMDS2 = mean (NMDS2, na.rm = T), 
         mean_NMDS3 = mean (NMDS3, na.rm = T), 
         sd_NMDS1 = sd (NMDS1, na.rm = T), 
         sd_NMDS2 = sd (NMDS2, na.rm = T), 
         sd_NMDS3 = sd (NMDS3, na.rm = T), 
         n = n()
         ) %>% 
  mutate(cv_NMDS1 = sd_NMDS1/mean_NMDS1,
         cv_NMDS2 = sd_NMDS2/mean_NMDS2,
         cv_NMDS3 = sd_NMDS1/mean_NMDS3) %>% 
  select(treatment, n,
         mean_NMDS1, sd_NMDS1, cv_NMDS1,
         mean_NMDS2, sd_NMDS2, cv_NMDS2,
         mean_NMDS3, sd_NMDS3, cv_NMDS3) %>% 
  distinct(treatment, n, 
           mean_NMDS1, sd_NMDS1, cv_NMDS1,
           mean_NMDS2, sd_NMDS2, cv_NMDS2,
           mean_NMDS3, sd_NMDS3, cv_NMDS3)




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

ggNMDS1_dynamics_plot <- 
ggplot(nmds_df_plot,
       aes(x = date, y =  NMDS1)) +
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  geom_errorbar(aes(ymax = mean_NMDS1 + sd_NMDS1, ymin = mean_NMDS1 - sd_NMDS1, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  geom_point(aes(x = date, y = mean_NMDS1, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  labs(y = "NMDS1", x = NULL) #






ggboxplot(nmds_df_plot, x = "treatment", y = "NMDS1", fill = "treatment") +
  stat_compare_means(comparisons = list(c("c", "w"), c("c", "p"), c("c", "wp"), c("w", "wp"), c("p", "wp")),
                     method = "t.test",
                     label = "p.signif") +  # Show significance stars (*, **, ***)
  scale_fill_manual(values = palette) +
  scale_x_discrete(labels = labels) +
  labs( x = NULL, y = "NMDS1", fill = "Treatment") +
  theme(legend.position = "none")


ggNMDS1_boxplot_plot <- 
ggplot(nmds_df_plot, aes(y = NMDS1, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = palette) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))


hist(nmds_df_plot$NMDS2, breaks = 50)

ggNMDS2_dynamics_plot <- 
  ggplot(nmds_df_plot,
         aes(x = date, y =  NMDS2)) +
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  geom_errorbar(aes(ymax = mean_NMDS2 + sd_NMDS2, ymin = mean_NMDS2 - sd_NMDS2, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  geom_point(aes(x = date, y = mean_NMDS2, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  labs(y = "NMDS2", x = NULL) #


ggNMDS2_boxplot_plot <- 
  ggplot(nmds_df_plot, aes(y = NMDS2, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = palette) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))




hist(nmds_df_plot$NMDS3, breaks = 50)

ggNMDS3_dynamics_plot <- 
  ggplot(nmds_df_plot,
         aes(x = date, y =  NMDS3)) +
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  geom_errorbar(aes(ymax = mean_NMDS3 + sd_NMDS3, ymin = mean_NMDS3 - sd_NMDS3, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  geom_point(aes(x = date, y = mean_NMDS3, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  scale_colour_manual(values = palette) +
  scale_fill_manual(values = palette) +
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  geom_vline(xintercept = as.Date("2023-05-11"), linetype = "dashed", color = "gray40") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  labs(y = "NMDS3", x = NULL) #


ggNMDS3_boxplot_plot <- 
  ggplot(nmds_df_plot, aes(y = NMDS3, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = palette) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))







###### DIFFERENCES AT TREATMENT x SAMPLING x PLOT level but with distance-matrix at sampling level 


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
                values_from = abundance,
                values_fill = list(abundance = 0))
  
  # Prepare abundance data
  abundance_data_subset <- sp_wide_subset %>% select(-treatment, -sampling, -date, -plot)
  
  # Compute Bray-Curtis distance
  if (nrow(abundance_data_subset) > 1) {  # Avoid NMDS failure for single-row cases
    distance_matrix_bc <- vegan::vegdist(abundance_data_subset, method = "bray")
    
    # Run NMDS
    nmds_result <- metaMDS(distance_matrix_bc, k = 3, trymax = 250, maxit = 999)
    
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
  facet_wrap(~ sampling, ncol = 5, nrow = 5) + 
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








