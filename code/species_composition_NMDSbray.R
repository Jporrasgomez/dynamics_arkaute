
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
hist((species_ab$abundance))




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
  nmds_res <- metaMDS(sp_wide, distance = "bray", k = 3, trymax = 250, maxit = 999) 
  
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
  
  #the same but for PRESENCE-ABSENCE
  
  sp_wide_pa <- sp_wide %>% 
    mutate_all(~ ifelse(. > 0, 1, 0))
  
  # Perform NMDS using Bray-Curtis distance
  nmds_res <- metaMDS(sp_wide_pa, distance = "bray", k = 3, trymax = 250, maxit = 999) 
  
  # Extract NMDS sample scores
  nmds_samples <- as.data.frame(scores(nmds_res, display = "sites"))
  
  # Extract NMDS species scores (optional)
  nmds_species <- as.data.frame(scores(nmds_res, display = "species"))
  
  gglist2[[count]] <- ggplot() +
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

ggarrange(
  gglist2[[2]],
  gglist2[[1]],
  gglist2[[3]],
  gglist2[[4]], 
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

{sp_wide <- species_ab %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0)) 
  

# create a distance matrix using Hellinger distances
abundance_data <- sp_wide %>% select(-treatment, -sampling)

# Compute Bray-Curtis distance matrix
distance_matrix_bc <- vegan::vegdist(abundance_data, method = "bray")

# Run NMDS (2 dimensions, 100 tries)
nmds_bc <- metaMDS(distance_matrix_bc, k = 3, trymax = 250, maxit = 999)

# Extract NMDS coordinates
nmds_df <- data.frame(
  NMDS1 = nmds_bc$points[, 1],
  NMDS2 = nmds_bc$points[, 2],
  treatment = sp_wide$treatment,
  sampling = sp_wide$sampling
)

# Arrange by sampling order
nmds_df <- nmds_df %>% arrange(sampling)

# Plot NMDS results using ggplot
ggnmds_alltreatments <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.68) + 
  geom_point(size = 1.5) +
  geom_text_repel(aes(label = sampling), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS using Bray-Curtis distance",
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "NMDS1", y = "NMDS2", color = "Treatment")
# Print the plot
print(ggnmds_alltreatments)}

ggplot(nmds_df, aes(x = sampling, y = NMDS1, color = treatment)) +
facet_wrap(~ treatment, ncol = 4, nrow = 1) +
  geom_point() + 
  geom_path(aes(group = treatment)) +
  scale_color_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D"))

ggplot(nmds_df, aes(x = sampling, y = NMDS2, color = treatment)) +
  facet_wrap(~ treatment, ncol = 4, nrow = 1) +
  geom_point() + 
  geom_path(aes(group = treatment)) +
  scale_color_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D"))



#PRESENCE-ABSENCE


# create a distance matrix using Hellinger distances
{abundance_data_pa <- abundance_data %>% 
  mutate_all(~(ifelse(. > 0, 1, 0)))

# Compute Bray-Curtis distance matrix
distance_matrix_bc <- vegan::vegdist(abundance_data_pa, method = "bray")

# Run NMDS (2 dimensions, 100 tries)
nmds_bc <- metaMDS(distance_matrix_bc, k = 3, trymax = 250, maxit = 999)

# Extract NMDS coordinates
nmds_df <- data.frame(
  NMDS1 = nmds_bc$points[, 1],
  NMDS2 = nmds_bc$points[, 2],
  treatment = sp_wide$treatment,
  sampling = sp_wide$sampling
)

# Arrange by sampling order
nmds_df <- nmds_df %>% arrange(sampling)

# Plot NMDS results using ggplot
ggnmds_alltreatments <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = treatment)) +
  stat_ellipse(geom = "polygon", aes(fill = treatment),
               alpha = 0.1, show.legend = FALSE, level = 0.68) + 
  geom_point(size = 1.5) +
  geom_text_repel(aes(label = sampling), max.overlaps = 100, size = 3, show.legend = F) +
  geom_hline(yintercept = 0, color = "gray52", linetype = "dashed") +
  geom_path(aes(group = treatment), linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "gray52", linetype = "dashed") +
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  labs(title = "NMDS using Bray-Curtis distance",
       subtitle = paste0("Stress = ", round(nmds_bc$stress, 3)),
       x = "NMDS1", y = "NMDS2", color = "Treatment")
# Print the plot
print(ggnmds_alltreatments)}



# podría hacer 4 NMDS, uno por réplica de treatent. Y así poder tener 4 puntos y una media para estos gráficos? 

species_ab_plot <- species_ab_plot %>% 
  filter(!(sampling == "1" & treatment %in% c("p", "wp")))
  

sp_wide_plot <- species_ab_plot %>%
  pivot_wider(id_cols = c(sampling, date, treatment, plot),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

# create a distance matrix using Hellinger distances
abundance_data_plot <- sp_wide_plot %>% select(-treatment, -sampling, -date, -plot)

# Compute Bray-Curtis distance matrix
distance_matrix_bc_plot <- vegan::vegdist(abundance_data_plot, method = "bray")

# Run NMDS (2 dimensions, 100 tries)
nmds_bc_plot <- metaMDS(distance_matrix_bc_plot, k = 3, trymax = 250, maxit = 999)

# Extract NMDS coordinates
nmds_df_plot <- data.frame(
  NMDS1 = nmds_bc_plot$points[, 1],
  NMDS2 = nmds_bc_plot$points[, 2],
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
         sd_NMDS1 = sd (NMDS1, na.rm = T), 
         sd_NMDS2 = sd (NMDS2, na.rm = T)) %>% 
  ungroup()


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
  
  labs(y = "NMDS2", x = NULL)


ggplot(nmds_df_plot, aes(y = NMDS2, x = treatment)) +
  geom_boxplot(aes(fill = treatment), color = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA), # Transparent background
        plot.background = element_rect(fill = NA, colour = NA))
