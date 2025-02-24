





#We calculate the mean area of the individual by taking Ah and Ab both and transform it into m2 (cm2/10000)
biomass_nolm <- flora_biomass_raw

biomass_nolm <- merge(biomass_nolm, species_code)

biomass_nolm <- left_join(biomass_nolm, nind0)

sum(is.na(biomass_nolm$nind_m2)) 

sum(is.na(biomass_nolm$nind_m2)) /length(biomass_nolm$code) * 100

#Where are the NA's?









biomass_nolm %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(x = as.numeric(sampling))) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows", y = "Sampling") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 21))

biomass_nolm %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  ggplot(aes(x = as.numeric(plot))) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows", x = "Plot") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 16))


biomass_nolm %>% 
  mutate(cell_content = case_when(is.na(nind_m2) ~ "NA",
                                  !is.na(nind_m2)  ~ "nind_available")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(y = code)) +
  geom_bar(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(x = "Number of rows")

summary(na)

na <- biomass_nolm

na$nind_m2_na <- ifelse(is.na(na$nind_m2), -1, na$nind_m2)

plots <- unique(biomass_nolm$plot)

i = 6

ggplot(subset(na, plot == plots[i]), aes(x = sampling, y = code, fill = nind_m2_na)) +
  geom_tile(color = "gray13") +  # Keep black grid lines
  geom_text(aes(label = nind_m2_na), size = 2.3) +  # Add text labels
  scale_fill_gradientn(
    colors = c("red3", "#A9D8F2", "#2A5D9C"),  # Very pale gray (#F0F0F0), white, orange
    values = scales::rescale(c(-1, 1, max(na$nind_m2, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove default grid lines
    axis.text.y = element_text(face = "italic"),  # Italicize species names
    legend.position = "bottom"  # Move legend to the bottom
  ) +
  labs(x = "Samplings", y = "Species")

  






par(mfrow = c(1,1))
hist(biomass_nolm$nind_m2, breaks = 100)
boxplot(biomass_nolm$nind_m2)
hist(log(biomass_nolm$nind_m2), breaks = 100)

nrow(subset(biomass_nolm, nind_m2 > 20))/nrow(biomass_nolm) *100

ggplot(biomass_nolm, aes(x = log(nind_m2), fill = treatment)) +
  geom_histogram(bins = 50) +
  scale_fill_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D"))

hist(biomass_nolm$biomass_i, breaks = 100)
hist(log(biomass_nolm$biomass_i), breaks = 100)

ggplot(biomass_nolm, aes(x = log(biomass_i), fill = treatment)) +
  geom_histogram(bins = 50) +
  scale_fill_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D"))


biomass_nolm$biomass_s <- biomass_nolm$biomass_i * biomass_nolm$nind_m2

par(mfrow = c(1,1))
hist(biomass_nolm$biomass_s, breaks = 100)
boxplot(biomass_nolm$biomass_s)
hist(log(biomass_nolm$biomass_s), breaks = 100)


Q1 <- quantile(log(biomass_nolm$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_nolm$biomass_s), 0.75, na.rm = TRUE)
IQR_value <- IQR(log(biomass_nolm$biomass_s), na.rm = TRUE)
upper_bound <- Q3 + 1.5 * IQR_value

biomass_nolm_clean <- biomass_nolm %>%
  filter(log(biomass_s) <= upper_bound)

hist(biomass_nolm_clean$biomass_s, breaks = 100)
boxplot(biomass_nolm_clean$biomass_s)
hist(log(biomass_nolm_clean$biomass_s), breaks = 100)



biomass_nolm <- biomass_nolm %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()

hist(biomass_nolm$biomass_community, breaks = 100)
hist(log(biomass_nolm$biomass_community), breaks = 100)


biomass_nolm_dynamics <- biomass_nolm %>%
  group_by(treatment, sampling, date, month) %>% 
  mutate(mean_biomass = mean(biomass_community, na.rm = T),
         sd_biomass = sd(biomass_community, na.rm = T)) %>% 
  ungroup() %>% 
  select(treatment, sampling, date, plot, code, biomass_s, biomass_community, mean_biomass, 
         sd_biomass)


ggplot(biomass_nolm_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 



shapiro.test(biomass_nolm_dynamics$biomass_community)
car::leveneTest(biomass_community ~ treatment, data = biomass_nolm_dynamics)
kruskal.test(biomass_community ~ treatment, data = biomass_nolm_dynamics)
library(dunn.test)
dunn.test(biomass_nolm_dynamics$biomass_community, biomass_nolm_dynamics$treatment, method = "bonferroni")

ggplot(biomass_nolm_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("NS", "***", "NS","NS", "***"), # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(7500, 7900, 8200, 8500, 8800, 9100),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  )



biomass_nolm_dynamics %>% 
  select(sampling, plot, treatment, date, biomass_community) %>% 
  distinct() %>% 
  ggplot(aes(x = date, y = biomass_community, color = treatment, fill = treatment)) + 
  geom_point() +
  geom_smooth(method = "loess", span = 0.6, alpha = 0.2) +
  scale_color_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) +
  scale_fill_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D"))



biomass_nolm_dynamics %>% 
  select(sampling, plot, treatment, date, biomass_community) %>% 
  distinct() %>% 
  ggplot(aes(x = date, y = biomass_community, color = treatment, label = paste(plot, sampling, sep = ", "))) + 
  geom_point() +
  scale_color_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) + 
    geom_text_repel(
      size = 3,                # Text size
      min.segment.length = 0.2,  # Ensures lines are always drawn
      segment.color = "gray50",  # Line color
      segment.size = 0.5         # Line thickness
    )



biomass_nolm_dynamics %>% 
  select(sampling, plot, treatment, date, code, biomass_s, biomass_community) %>% 
  distinct() %>% 
  filter(plot == "15") %>% 
  filter(sampling %in% c ("6", "7", "8", "9", "10")) %>% 
  ggplot(aes(x = date, y = biomass_s, color = treatment, label = paste(code, sampling, sep = ", "))) + 
  geom_point() +
  scale_color_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) + 
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.2,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )


biomass_nolm_dynamics %>% 
  select(sampling, plot, treatment, date, code, biomass_s, biomass_community) %>% 
  distinct() %>% 
  filter(plot == "10") %>% 
  filter(sampling %in% c("15", "16", "17", "18", "19", "20")) %>% 
  ggplot(aes(x = date, y = biomass_s, color = treatment, label = paste(code, sampling, sep = ", "))) + 
  geom_point() +
  scale_color_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) + 
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.2,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )






a_biomass_nolm <-
  ggplot(biomass_nolm_dynamics,
         aes(x = date, y = biomass_community)) + 
  
  geom_smooth(
    se = TRUE, aes(color = treatment, fill = treatment),
    method = "loess", span = 0.6, alpha = 0.2 
  ) +
  
  geom_point(aes(color = treatment),
             alpha = 0.5, position = position_dodge(width = 8)) +
  
  geom_errorbar(aes(ymax = mean_biomass + sd_biomass, ymin = mean_biomass - sd_biomass, color = treatment),
                , alpha = 0.2, position = position_dodge(width = 8)) + 
  
  geom_point(aes(x = date, y = mean_biomass, color = treatment), fill = "white", 
             shape = 21, size = 2, position = position_dodge(width = 8))+
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_x_date(
    date_breaks = "4 weeks", # Specify the interval (e.g., every 2 weeks)
    date_labels = "%d-%b-%y" # Customize the date format (e.g., "04-May-23")
  ) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
  ) +
  
  # scale_y_log10() +
  
  labs(y = "Community biomass", x = NULL) #








# Let's gap filling with RANDOM FOREST package


biomass_nolm <- biomass_nolm %>% 
  select(sampling, plot, treatment, code, abundance, abundance_community, nind_m2)

biomass_nolm <- biomass_nolm %>% 
  filter(!is.na(Ah)) %>% 
  filter(!is.na(Ab))
str(biomass_nolm)

biomass_nolm <- biomass_nolm %>% mutate(across(where(is.character), as.factor))


summary(biomass_nolm)




library(missForest)

imputed_data <- missForest(poaceae, ntree = 100) #default ntrees = 100
# Access the imputed data
imputed_biomass <- imputed_data$ximp
imputed_data$OOBerror # high error. Should we go ahead?


head(imputed_biomass)
summary(imputed_biomass)


library(mice)

# Aplicar imputación con Random Forest
biomass_nolm_mice <- mice(biomass_nolm, method = "rf", m = 5, maxit = 50)
plot(biomass_nolm_mice) # Check if lines stabilize


# Obtener dataset imputado
biomass_nolm_imputed <- complete(biomass_nolm_mice)

summary(biomass_nolm_imputed)

plot(biomass_nolm_mice)


ggplot() +
  geom_density(aes(x = biomass_nolm$nind_m2), color = "red", linetype = "dashed") +
  geom_density(aes(x = biomass_nolm_imputed$nind_m2), color = "blue") +
  labs(title = "Density Plot of Original (Red) vs Imputed (Blue) nind_m2")






