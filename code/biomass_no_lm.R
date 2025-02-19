



#We calculate the mean area of the individual by taking Ah and Ab both and transform it into m2 (cm2/10000)
biomass_nolm <- flora_biomass_raw

biomass_nolm <- merge(biomass_nolm, species_code)


#biomass_nolm <- biomass_nolm %>%
# mutate(area_i = case_when(
#   growing_type == "crawler" ~ (Ah + Ab + (pi*(height/2)^2)) / 3,  # For "crawler"
#   TRUE ~ pmax(Ah, Ab, na.rm = TRUE)  # For all others, take the max of Ah and Ab
# ))


# assigning an area at individual level based on the growing type of the species. 

biomass_nolm <- biomass_nolm %>%
  mutate(max_a = pmax(Ah, Ab, na.rm = T )) %>% 
  mutate(area_i = case_when(
    growing_type == "crawler" ~ (pi*(height/2)^2),
    growing_type == "vertical"~ max_a, 
    growing_type == "cone" ~ (max_a + (pi*(height/2)^2))/2, 
    growing_type == "spherical" ~ (max_a + (pi*(height/2)^2))/2))
    

  
biomass_nolm$area_i <- biomass_nolm$area_i/10000 # transforming from cm2 to m2


biomass_nolm <- left_join(biomass_nolm, nind0)

biomass_nolm <- biomass_nolm %>% 
  mutate(nind_m2_estimated = (abundance/100)/area_i)

#let's get rid of the outliers
hist(biomass_nolm$nind_m2_estimated, breaks = 100)
hist(log(biomass_nolm$nind_m2_estimated), breaks = 100)

# WHATTT SALE BIMODAL!

# Step 1: Calculate IQR for log-transformed biomass_s
#Q1 <- quantile(log(biomass_nolm$nind_m2_estimated), 0.25, na.rm = TRUE)
#Q3 <- quantile(log(biomass_nolm$nind_m2_estimated), 0.75, na.rm = TRUE)
#IQR <- Q3 - Q1
#
## Step 2: Remove outliers
#biomass_nolm_clean <- biomass_nolm %>%
#  filter(log(nind_m2_estimated) >= Q1 - 1.5 * IQR & log(nind_m2_estimated) <= Q3 + 1.5 * IQR)

biomass_nolm_clean <- biomass_nolm %>% 
  filter(nind_m2_estimated < 95000) 
  

hist(biomass_nolm_clean$nind_m2_estimated, breaks = 100)
hist(log(biomass_nolm_clean$nind_m2_estimated), breaks = 100)



biomass_nolm_clean <- biomass_nolm_clean %>%
  mutate(nind_m2_scaled = (nind_m2_estimated - min(nind_m2_estimated, na.rm = TRUE)) /
           (max(nind_m2_estimated, na.rm = TRUE) - min(nind_m2_estimated, na.rm = TRUE)) *
           (100 - 1) + 1)

ggplot(biomass_nolm_clean, aes(x = nind_m2, y = nind_m2_scaled)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#At this point 


biomass_nolm_clean <- biomass_nolm_clean %>% 
  mutate(biomass_s = biomass_i * nind_m2_scaled)


flora_biomass_nolm <- biomass_nolm_clean
