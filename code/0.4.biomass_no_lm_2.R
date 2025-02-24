





#We calculate the mean area of the individual by taking Ah and Ab both and transform it into m2 (cm2/10000)
biomass_nolm <- flora_biomass_raw

biomass_nolm <- merge(biomass_nolm, species_code)

biomass_nolm <- left_join(biomass_nolm, nind0)





#biomass_nolm <- biomass_nolm %>%
# mutate(area_i = case_when(
#   growing_type == "crawler" ~ (Ah + Ab + (pi*(height/2)^2)) / 3,  # For "crawler"
#   TRUE ~ pmax(Ah, Ab, na.rm = TRUE)  # For all others, take the max of Ah and Ab
# ))


# assigning an area at individual level based on the growing type of the species. 

# Probar así: 
biomass_nolm <- biomass_nolm %>%
  mutate(max_a = pmax(Ah, Ab, na.rm = T )) %>% 
  mutate(area_i = case_when(
    growing_type == "crawler" ~ (pi*(height/2)^2), # The area is the diameter of the height since the longest stems are growing by crawling on the ground
    # There is a problem here because we are assuming that same length stems are growing in all directions. And we might have one long stem. 
    growing_type == "vertical"~ max_a, # I take the max value of Ah and Ab to ensure minimizing the effect of the compression of tissues at measuring
    growing_type == "cone" ~ (max_a + (pi*(height/2)^2))/2, # I add the height in order to reduce the effect of compressing the tissues
    growing_type == "spherical" ~ (max_a + (pi*(height/2)^2))/2)) # I add the height in order to reduce the effect of compressing the tissues



##Probar así
#biomass_nolm <- biomass_nolm %>%
#  mutate(max_a = pmax(Ah, Ab, na.rm = T )) %>% 
#  mutate(area_i = case_when(
#    growing_type == "crawler" ~ (pi*(height/2)^2), # The area is the diameter of the height since the longest stems are growing by crawling on the ground
#    growing_type == "vertical"~ max_a, # I take the max value of Ah and Ab to ensure minimizing the effect of the compression of tissues at measuring
#    growing_type == "cone" ~ max_a, # I add the height in order to reduce the effect of compressing the tissues
#    growing_type == "spherical" ~ max_a))

# Probar así: 
#biomass_nolm <- biomass_nolm %>%
#  mutate(max_a = pmax(Ah, Ab, na.rm = T )) %>% 
#  mutate(area_i = (max_a + (pi*(height/2)^2))/2)


biomass_nolm$area_i <- biomass_nolm$area_i/10000 # transforming from cm2 to m2

hist(biomass_nolm$area_i, breaks = 100)
#Hay areas individuales superiores al metro cuadrado
# Hay que corregir eso


#biomass_nolm$area_i <- ifelse(biomass_nolm$area_i > (biomass_nolm$abundance/100), (biomass_nolm$abundance/100), biomass_nolm$area_i)

# Si hago esta corrección, me sale muchisimos numeros de individuos = 1. Igualmente, si no la hago, salen numeros de individuos menor que 1, 
# Cosa que es imposible. 
# El problema está que, si no incluimos la altura, vamos a estrar sobre estimando el numero de individuos de forma no homogénea en toda la
# base de datos.

hist(biomass_nolm$area_i, breaks = 100)



# We estimate the number of individuals by dividing the total space occupied by a species (abundance)
# by the average space occupied by one individual of that species (area_i). 

biomass_nolm <- biomass_nolm %>% 
  mutate(nind_m2_estimated = (abundance/100)/area_i) # I divide abundance/100 because abundance its %coverage of m2 and i want it to be m2.  

par(mfrow = c(1,1))
hist(biomass_nolm$nind_m2_estimated, breaks = 1000)
hist(log(biomass_nolm$nind_m2_estimated), breaks = 100)

ggplot(biomass_nolm, aes(x = log(nind_m2_estimated), fill = treatment)) +
  geom_histogram(bins = 100) +
  scale_fill_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) +
  scale_x_continuous(breaks = scales::breaks_extended(n = 20))+
  geom_vline(xintercept = 4.65, linetype = "dashed", color = "gray40")

mode_1 <- biomass_nolm %>% 
  filter(log(nind_m2_estimated) < 4.65)
hist(log(mode_1$nind_m2_estimated), breaks = 100)

Q1 <- quantile(log(mode_1$nind_m2_estimated), 0.25, na.rm = TRUE)
IQR_value <- IQR(log(mode_1$nind_m2_estimated), na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_value


mode_1_clean <- mode_1 %>%
  filter(log(nind_m2_estimated) >= lower_bound)

hist(mode_1_clean$nind_m2_estimated, breaks = 100)
hist(log(mode_1_clean$nind_m2_estimated), breaks = 100)



mode_2 <- biomass_nolm %>% 
  filter(log(nind_m2_estimated) >= 4.65)
hist(log(mode_2$nind_m2_estimated), breaks = 100)

Q3 <- quantile(log(mode_2$nind_m2_estimated), 0.75, na.rm = TRUE)
IQR_value <- IQR(log(mode_2$nind_m2_estimated), na.rm = TRUE)
upper_bound <- Q3 + 1.5 * IQR_value

mode_2_clean <- mode_2 %>%
  filter(log(nind_m2_estimated) <= upper_bound)

hist(mode_2_clean$nind_m2_estimated, breaks = 100)
hist(log(mode_2_clean$nind_m2_estimated), breaks = 100)

mode_2_clean <- mode_2_clean %>% 
  filter(nind_m2_estimated < 1000000)

hist(mode_2_clean$nind_m2_estimated, breaks = 100)
hist(log(mode_2_clean$nind_m2_estimated), breaks = 100)

max_ind_m2 <- max(biomass_nolm$nind_m2/(biomass_nolm$abundance/100), na.rm = T) # máximo numero de individuos por metro cuadrado observado en arkaute

mode_2_clean <- mode_2_clean %>%
  mutate(nind_m2_scaled = (nind_m2_estimated - min(nind_m2_estimated, na.rm = TRUE)) /
           (max(nind_m2_estimated, na.rm = TRUE) - min(nind_m2_estimated, na.rm = TRUE)) *
           (max_ind_m2 - min(nind_m2_estimated, na.rm = T)) + min(nind_m2_estimated, na.rm = T)) #Máximo numero de inviduos observado en arkaute en 1 m2. 


hist(mode_2_clean$nind_m2_scaled, breaks = 100)
hist(log(mode_2_clean$nind_m2_scaled), breaks = 100)


mode_1_clean$nind_m2_scaled <- mode_1_clean$nind_m2_estimated

biomass_nolm_clean <- bind_rows(mode_1_clean, mode_2_clean)

hist(biomass_nolm_clean$nind_m2_estimated, breaks  = 100)
hist(log(biomass_nolm_clean$nind_m2_estimated), breaks  = 100)

hist(biomass_nolm_clean$nind_m2_scaled, breaks  = 100)
hist(log(biomass_nolm_clean$nind_m2_scaled), breaks  = 100)





hist(biomass_nolm_clean$biomass_i, breaks = 100)
hist(log(biomass_nolm_clean$biomass_i), breaks = 100)

ggplot(biomass_nolm_clean, aes(x = log(biomass_i), fill = treatment)) +
  geom_histogram(bins = 100) +
  scale_fill_manual(values = c("c" = "#5FC0AD", "w" = "#E05C50", "p" = "#4B8ED1", "wp" = "#7A578D")) +
  scale_x_continuous(breaks = scales::breaks_extended(n = 20))
#Clear stratification by treatment

#Let's clear it

Q1 <- quantile(log(biomass_nolm_clean$biomass_i), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_nolm_clean$biomass_i), 0.75, na.rm = TRUE)
IQR_value <- IQR(log(biomass_nolm_clean$biomass_i), na.rm = TRUE)
upper_bound <- Q3 + 1.5 * IQR_value

biomass_nolm_clean <- biomass_nolm_clean %>%
  filter(log(biomass_i) <= upper_bound)

hist(biomass_nolm_clean$biomass_i, breaks = 100)
hist(log(biomass_nolm_clean$biomass_i), breaks = 100)


biomass_nolm_clean$biomass_s <- biomass_nolm_clean$biomass_i * biomass_nolm_clean$nind_m2_scaled
# Let's check the results: 

flora_biomass_nolm_clean <- biomass_nolm_clean

flora_biomass_nolm_clean <- flora_biomass_nolm_clean %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()


biomass_nolm_clean_dynamics <- flora_biomass_nolm_clean %>%
  filter(!sampling %in% c("0", "1", "2", "12")) %>% 
  group_by(treatment, sampling, date, month) %>% 
  mutate(mean_biomass = mean(biomass_community, na.rm = T),
         sd_biomass = sd(biomass_community, na.rm = T)) %>% 
  ungroup() %>% 
  select(treatment, sampling, date, plot, code, biomass_s, biomass_community, mean_biomass, 
         sd_biomass)


ggplot(biomass_nolm_clean_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 



shapiro.test(biomass_nolm_clean_dynamics$biomass_community)
car::leveneTest(biomass_community ~ treatment, data = biomass_nolm_clean_dynamics)
kruskal.test(biomass_community ~ treatment, data = biomass_nolm_clean_dynamics)
library(dunn.test)
dunn.test(biomass_nolm_clean_dynamics$biomass_community, biomass_nolm_clean_dynamics$treatment, method = "bonferroni")

ggplot(biomass_nolm_clean_dynamics, aes(x = treatment, y = biomass_community)) +
  geom_boxplot(aes(fill = treatment), colour = "black", alpha = 0.5) + # Set the outline color to black
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  ggsignif::geom_signif(
    comparisons = list(c("c","w"), c("c", "p"), c("c", "wp"), c("wp", "w"), c("wp", "p")),  # Significant comparisons
    annotations = c("***", "***", "***","***", "***"), # Asterisks for significance
    map_signif_level = TRUE,  # Automatically map significance levels if p-values provided
    y_position = c(1500, 1600, 1700, 1800, 1900, 2000),  # Adjust bracket positions
    tip_length = 0.01,  # Length of bracket tips
    textsize = 4  # Size of asterisks
  )


a_biomass_nolm_clean <-
  ggplot(biomass_nolm_clean_dynamics,
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




