
# probar con log(abundance) u otras transformaciones 



rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, tidyverse, DT, viridis, ggrepel, codyn, vegan, eulerr, ggplot2, ggthemes, ggpubr, ggforce )

source("code/1.first_script.R"); rm(list = setdiff(ls(), c("flora_abrich", "biomass_imp", "biomass_noimp")))


theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))


species_ab <-  summarise(group_by(flora_abrich, date, code, sampling, treatment, family,  genus_level, species_level),
                         abundance = mean(abundance, na.rm = T)) #mean abundance of species per treatment and sampling  

species_ab_plot <- flora_abrich %>% 
  select(date, code, sampling, plot, treatment, family, genus_level, species_level, abundance)


# Here there is a problem: the total abundance per sampling and treatment is sometimes higher than 200%
totals_df <- summarise(group_by(species_ab, sampling, treatment), #adding number of species per treatment and sampling to species_ab
                       n_species = n_distinct(code),
                       total_abundance = sum(abundance))


species_ab <- merge(species_ab, totals_df)




hist(species_ab$abundance)
hist(log(species_ab$abundance))
hist(sqrt(species_ab$abundance))





species_ab <- species_ab %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))



treats <- unique(flora_abrich$treatment)

list1 <- list()
gglist1 <- list()
gglist2 <- list()
count = 0

for(i in 1:length(treats)){
  
  count = count + 1
  
  list1[[count]] <- subset(species_ab, treatment == treats[i])
  
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

#
##Transformar log(abundance) ? 
#species_ab <- species_ab %>% 
#  mutate(log_abundance = log(abundance), na.rm = T)

sp_wide <- species_ab %>%
  pivot_wider(id_cols = c(sampling, date, treatment),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) 
  

# create a distance matrix using Hellinger distances
abundance_data <- sp_wide %>% select(-treatment, -sampling, -date)

# Compute Bray-Curtis distance matrix
distance_matrix_bc <- vegan::vegdist(abundance_data, method = "bray")

# Run NMDS (2 dimensions, 100 tries)
nmds_bc <- metaMDS(distance_matrix_bc, k = 2, trymax = 250, maxit = 999)

# Extract NMDS coordinates
nmds_df <- data.frame(
  NMDS1 = nmds_bc$points[, 1],
  NMDS2 = nmds_bc$points[, 2],
  treatment = sp_wide$treatment,
  sampling = sp_wide$sampling,
  date = sp_wide$date
)

# Arrange by sampling order
nmds_df <- nmds_df %>% arrange(sampling)

# Plot NMDS results using ggplot

{ggnmds_alltreatments <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.90) + 
  geom_point(size = 1.5, aes(shape = treatment)) +
  geom_text_repel(aes(label = sampling), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS Bray-Curtis: mean abundance of species at sampling level",
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "NMDS1", y = "NMDS2", color = "Treatment")
# Print the plot
print(ggnmds_alltreatments)}

ggplot(nmds_df, aes(x = date, y = NMDS1, color = treatment, fill = treatment)) +
  geom_point(show.legend = TRUE) + 
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.9, alpha = 0.1, show.legend = TRUE
  )+
  geom_path(aes(group = treatment), show.legend = FALSE) +
  scale_color_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D"), guide = "none") +
  labs(title = "NMDS1 Bray-Curtis: mean abundance at sampling level ",
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "Date", y = "NMDS1", color = "Treatment")

ggplot(nmds_df, aes(x = date, y = NMDS2, color = treatment, fill = treatment)) +
  geom_point(show.legend = TRUE) +  # Hide extra legend from points
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.9, alpha = 0.1, show.legend = TRUE # Keep one legend
  ) +
  geom_path(aes(group = treatment), show.legend = FALSE) + # Hide extra legend from paths
  scale_color_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D"), guide = "none") +  # Hide fill legend
  labs(title = "NMDS1 Bray-Curtis: mean abundance at sampling level ",
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "Date", y = "NMDS2", color = "Treatment")








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

print(nmds_bc_plot$stress)

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
nmds_df_plot <- nmds_df_plot %>%
  arrange(sampling) %>% 
  group_by(treatment, sampling) %>% 
  mutate(mean_NMDS1 = mean (NMDS1, na.rm = T), 
         mean_NMDS2 = mean (NMDS2, na.rm = T), 
         mean_NMDS3 = mean (NMDS3, na.rm = T), 
         sd_NMDS1 = sd (NMDS1, na.rm = T), 
         sd_NMDS2 = sd (NMDS2, na.rm = T), 
         sd_NMDS3 = sd (NMDS3, na.rm = T)) %>% 
  ungroup()



{ggnmds_allplots_12<- ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.9) + 
  geom_point(size = 1.5) +
  #geom_text_repel(aes(label = paste0(sampling, "," ,plot)), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  #geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS Bray-Curtis: abundance of species at plot level",
       subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
       x = "NMDS1", y = "NMDS2", color = "Treatment")
# Print the plot
print(ggnmds_allplots_12)}

{ggnmds_allplots_13<- ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS3, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.68) + 
  geom_point(size = 1.5) +
  #geom_text_repel(aes(label = paste0(sampling, "," ,plot)), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  #geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS Bray-Curtis: abundance of species at plot level",
       subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
       x = "NMDS1", y = "NMDS3", color = "Treatment")
# Print the plot
print(ggnmds_allplots_13)}


{ggnmds_allplots_23<- ggplot(nmds_df_plot, aes(x = NMDS2, y = NMDS3, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.68) + 
  geom_point(size = 1.5) +
  #geom_text_repel(aes(label = paste0(sampling, "," ,plot)), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  #geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS Bray-Curtis: abundance of species at plot level",
       subtitle = paste0("Stress = ", round(nmds_bc_plot$stress, 3)),
       x = "NMDS2", y = "NMDS3", color = "Treatment")
# Print the plot
print(ggnmds_allplots_23)}


hist(nmds_df_plot$NMDS1, breaks = 50)

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
  #geom_line(aes(x = date, y = mean_NMDS1, color = treatment)) + 
  #
  geom_point(aes(x = date, y = mean_NMDS1, color = treatment), fill = "white", 
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
  
  labs(y = "NMDS1", x = NULL) #

ggplot(nmds_df_plot, aes(y = NMDS1, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))


hist(nmds_df_plot$NMDS2, breaks = 50)

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
  #geom_line(aes(x = date, y = mean_NMDS2, color = treatment)) + 
  #
  geom_point(aes(x = date, y = mean_NMDS2, color = treatment), fill = "white", 
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
  
  labs(y = "NMDS2", x = NULL, color = "Treatment")


ggplot(nmds_df_plot, aes(y = NMDS2, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))



hist(nmds_df_plot$NMDS3, breaks = 50)

ggplot(nmds_df_plot,
       aes(x = date, y =  NMDS3)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_NMDS2 + sd_NMDS2, ymin = mean_NMDS2 - sd_NMDS2, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  #geom_line(aes(x = date, y = mean_NMDS2, color = treatment)) + 
  #
  geom_point(aes(x = date, y = mean_NMDS2, color = treatment), fill = "white", 
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
  
  labs(y = "NMDS3", x = NULL, color = "Treatment")



ggplot(nmds_df_plot, aes(y = NMDS3, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))



# NMDS at sampling level


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
nmds_df_plot <- bind_rows(nmds_list)

# Merge stress values into NMDS dataframe
nmds_df_plot <- left_join(nmds_df_plot, stress_values, by = "sampling")


# Create NMDS plot with stress values in facet labels
ggplot(nmds_df_plot, aes(x = NMDS1, y = NMDS2, color = treatment, fill = treatment)) +
  #facet_wrap(~ paste0(sampling, " (Stress = ", stress, ")"), ncol = 5, nrow = 5) +
  facet_wrap(~ sampling, ncol = 5, nrow = 5) + 
  geom_point(size = 1.5) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.2, show.legend = FALSE, level = 0.68) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme(
    legend.position = "null",
    strip.text = element_text(size = 10), # Adjust facet label size
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


