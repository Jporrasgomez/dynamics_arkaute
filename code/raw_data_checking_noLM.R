




# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel)

theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))





flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)


flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))

flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)

boxplot(flora_raw$abundance, main = "Abundance")
boxplot(flora_raw$height, main = "Height")
boxplot(flora_raw$Dm, main = "Dm")
boxplot(flora_raw$Cm, main = "Cm")
boxplot(flora_raw$Db, main = "Db")
boxplot(flora_raw$Cb, main = "Cb")

# !! Checking for errors:

# is there any line with less than 2 NA between height, cb, cm, db and dm?  This is: did we slip some extra data in some lines?
nrow({checking <- flora_raw %>%
  filter(rowSums(is.na(select(., height, Cb, Db, Cm, Dm))) < 2)})

#is there any line with more than 2 NA among the same variables? This is: are we missing some data?
nrow({checking2 <- flora_raw %>%
  filter(rowSums(is.na(select(., height, Cb, Db, Cm, Dm))) > 2) %>%
  filter(!sampling %in% c("0", "1", "2","3", "12"))})


# is there any dm-cb /db-cm relationship that is not expected?

library(ggrepel)

ggcbdm <- 
  ggplot( flora_raw %>% filter(!is.na(Cb) & !is.na(Dm)),
          aes(x = log(Cb), y = Dm, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  #geom_density_2d() +
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  scale_y_continuous(limits = c(0, 0.6)) + 
  labs(
    x = "Log(Cb)",       # Label for x-axis
    y = "Dm",            # Label for y-axis
    color = "Treatment"  # Legend title for color
  )


ggcmdb <- 
  ggplot( flora_raw %>% filter(!is.na(Cm) & !is.na(Db)),
          aes(x = log(Cm), y = Db, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  
  labs(
    x = "Log(Cm)",       # Label for x-axis
    y = "Db",            # Label for y-axis
    color = "Treatment"  # Legend title for color
  )


ggdbdm <- 
  ggplot(
    flora_raw %>% filter(!is.na(Db) & !is.na(Dm)),  # Filter rows where Db and Dm are not NA
    aes(x = Db, y = Dm, color = treatment, label = paste(sampling, plot, code, sep = ", "))
  ) +
  geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) +
  labs(
    x = "Db",       # Label for x-axis
    y = "Dm",       # Label for y-axis
    color = "Treatment"  # Legend title for color
  )


ggcbcm <- 
  ggplot( flora_raw %>% filter(!is.na(Cb) & !is.na(Cm)),
          aes(x = Cb, y = Cm, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  #geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_density_2d() +
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  labs(
    x = "Cb",       # Label for x-axis
    y = "Cm",            # Label for y-axis
    color = "Treatment"  # Legend title for color
  )

#ggsave("results/ggcbdm.png", plot = ggcbdm, width = 6, height = 4, dpi = 300)
#ggsave("results/ggcmdb.png", plot = ggcmdb, width = 6, height = 4, dpi = 300)
#ggsave("results/ggdbdm.png", plot = ggdbdm, width = 6, height = 4, dpi = 300)
#ggsave("results/ggcbcm.png", plot = ggcbcm, width = 6, height = 4, dpi = 300)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$date <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$date, label = TRUE)
sampling_dates$day <- day(sampling_dates$date)
sampling_dates$year <- year(sampling_dates$date)

sampling_dates <- sampling_dates %>% 
  select(sampling, date, day, month, year, one_month_window, omw_date)

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))


flora_rare <- flora_raw %>%
  select(sampling, one_month_window, omw_date, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)



#Checking if we have mistakes (missing more than 2 variables with NA's)
nrow({checkingNA <- flora_rare %>%
  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2) %>%
  filter(!sampling %in% c("0", "1", "2", "3", "12"))}) #quitamos estos muestreos porque ya sé que aqui hay muchos valores con NA

#
##View(checkingNA) #Hay 4 líneas solo. Las dejamos. 
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Adding species information
species_code <- read.csv("data/species_code.csv")
species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))


# WE WORK WITH IDENTIFIED SPECIES!!

flora_rare <- merge(flora_rare, species_code, by = "code") 

# Cambios de los datos morfológicos antes de agrupar la base de datos por especie, muestro y sampling 

#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora_rare$Dm <- flora_rare$Dm + 0.01
flora_rare$Db <- flora_rare$Db + 0.01



## BIOMASS AT INDIVIDUAL LEVEL ####

# Ecuación biomasa####                 
#Transforming diameters into circumferences
flora_rare$cm <- round(ifelse(!is.na(flora_rare$Dm), flora_rare$Dm * pi, flora_rare$Cm), 2)
flora_rare$cb <- round(ifelse(!is.na(flora_rare$Db), flora_rare$Db * pi, flora_rare$Cb), 2)

#Calculating area of circle with circumference values
flora_rare$Ah <- ((flora_rare$cm)^2)/(4*pi)
flora_rare$Ab <- ((flora_rare$cb)^2)/(4*pi)

nrow({checkingNA <- flora_rare %>%
  filter(rowSums(is.na(select(., height, Ah, Ab))) > 1) %>%  #Checking if the area of any individual is above 1 square meter
  filter(!sampling %in% c("0", "1", "2", "3", "12"))})

ggabah <-  
  ggplot(flora_rare, aes(x = Ab, y = Ah, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  theme_minimal() +
  labs(
    x = "Ab",       # Label for x-axis
    y = "Ah",            # Label for y-axis
    color = "Treatment"  # Legend title for color
  )

#At the first samplings, we were collecting information of all possible species. With the pass of time, we
# realized we could not identify every species at every sampling, 
taxongroups <- flora_rare %>%
  filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
  group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = sum(unique(abundance), na.rm = T), #here we sum abundances 
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))


species <- anti_join(flora_rare, taxongroups, by = "code") %>%
  group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = mean(abundance, na.rm = T), #here we mean abundances (fake mean, species in the same plot and sampling have the same abundance info)
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))

flora_medium <- bind_rows(taxongroups, species)


d <- 1.96
z <- 2/3
flora_medium$x <- (flora_medium$height/2)*(flora_medium$Ab + flora_medium$Ah)

flora_medium$biomass_i <- d*(flora_medium$x^z)


# RICHNESSS ########

flora_medium <- flora_medium %>% 
  group_by(plot, sampling) %>% 
  mutate(richness = n_distinct(code, na.rm = T)) %>% 
  ungroup()


# ABUNDANCE ########


flora_medium <- flora_medium %>% 
  group_by(plot, sampling) %>% 
  mutate(abundance_community = sum(abundance, na.rm = T)) %>% 
  ungroup()


flora_abrich <- flora_medium


flora_medium$code_ordered <- reorder(flora_medium$code, -flora_medium$abundance, FUN = mean)

flora_nobs <- flora_medium %>% 
  group_by(code) %>% 
  summarize(n_observations= n(),
            mean_abundance = mean(abundance))

flora_nobs$abnobs <- flora_nobs$mean_abundance * flora_nobs$n_observations

flora_nobs$code_nobs <- factor(flora_nobs$code,
                               levels = flora_nobs$code[order(flora_nobs$n_observations, decreasing = TRUE)])

# !!Checking abundance

hist(flora_abrich$abundance_community, breaks = 100)
boxplot(flora_abrich$abundance_community)


ggplot(flora_abrich, aes(x = date, y = abundance_community,
                         color = treatment, fill = treatment,
                         group = treatment, label = sampling))+
  facet_wrap( ~ plot) + 
  
  #geom_point(alpha = 0.7, size = 1.2) +
  
  geom_line(alpha = 0.7) +
  
  geom_text(size = 3) +
  
  #geom_smooth(method = "loess", span = 0.3,alpha = 0.2, se = T) +
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 





ggplot(flora_medium, aes(x = code_ordered, y = abundance)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


ggplot(flora_nobs, aes(x = mean_abundance, y = n_observations, label = code, color = abnobs))+
  geom_point() + 
  scale_color_gradient(low = "blue4", high = "red2") +  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )


ggplot(flora_nobs, aes(x = code_nobs, y = n_observations, fill = n_observations)) +
  geom_col() +
  scale_fill_gradient(low = "blue4", high = "red2") +  # Apply color gradient from blue to red
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(fill = "n obs")  # Add a color scale label

flora_nobs$code_ab <- factor(flora_nobs$code, levels = flora_nobs$code[order(flora_nobs$mean_abundance, decreasing = TRUE)])
ggplot(flora_nobs, aes(x = code_ab, y = mean_abundance, fill = mean_abundance)) +
  geom_col() +
  scale_fill_gradient(low = "yellow3", high = "green4") +  # Apply color gradient from blue to red
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(fill = "mean abudance")  # Add a color scale label

flora_nobs$code_abnobs <- factor(flora_nobs$code, levels = flora_nobs$code[order(flora_nobs$abnobs, decreasing = TRUE)])
ggplot(flora_nobs, aes(x = code_abnobs, y = abnobs, fill = abnobs)) +
  geom_col() +
  scale_fill_gradient(low = "blue4", high = "yellow3") +  # Apply color gradient from blue to red
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  labs(fill = "abnobs") # Add a color scale label


# Species presence

species_presence_treat <- flora_raw %>%
  distinct(treatment, sampling, plot, code) %>%
  count(treatment, code, name = "n_obs_xtreat") %>%
  group_by(code) %>%
  mutate(total_obs = sum(n_obs_xtreat),
         perc_obs = round(n_obs_xtreat / total_obs, 2))

flora_nobs_presence <- merge(flora_nobs, species_presence_treat, by = "code") %>% 
  select(treatment, code, n_obs_xtreat, total_obs, perc_obs) 

# Ensure all (treatment, code) combinations exist
flora_nobs_presence <- expand_grid(
  treatment = unique(flora_nobs_presence$treatment),
  code = unique(flora_nobs_presence$code)) %>%
  left_join(flora_nobs_presence, by = c("treatment", "code"))

flora_nobs_presence <- merge(flora_nobs_presence, species_code, by = "code")

# Convert perc_obs to a factor to handle missing values separately
flora_nobs_presence <- flora_nobs_presence %>%
  mutate(perc_obs = ifelse(is.na(perc_obs), 0 , perc_obs), 
         n_obs_xtreat = ifelse(is.na(n_obs_xtreat), 0, n_obs_xtreat),
         total_obs = ifelse(is.na(total_obs), 0, total_obs))

flora_nobs_presence <- flora_nobs_presence %>%
  mutate(code = reorder(code, total_obs))



 
  ggplot(flora_nobs_presence, aes(x = treatment, y = code, fill = n_obs_xtreat)) +
  geom_tile(color = "gray13") +  # Keep black grid lines
    coord_fixed() +
  geom_text(aes(label = n_obs_xtreat), size = 2) +  # Add text labels
  scale_fill_gradientn(
    colors = c("white", "#FFF5E1", "orange2"),  # Very pale gray (#F0F0F0), white, orange
    values = scales::rescale(c(0, 1, max(flora_nobs_presence$n_obs_xtreat, na.rm = TRUE))),
    name = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove default grid lines
    axis.text.y = element_text(face = "italic"),  # Italicize species names
    legend.position = "bottom"  # Move legend to the bottom
  ) +
  labs(x = "Treatment", y = "Species")


## BIOMASS AT SPECIES LEVEL ###########

#For biomass, we do not have information for samplings 0, 1, 2 and 12. 

flora_biomass_raw <- flora_medium %>% 
  filter(!sampling %in% c("0", "1", "2", "12"))


# We can approach the calculation of biomass at species level (biomass_s) 
  
flora_biomass_raw$mean_area_i <- (flora_biomass_raw$Ah + flora_biomass_raw$Ab)/2


flora_biomass_raw$biomass_s <- (flora_biomass_raw$biomass_i * flora_biomass_raw$abundance)/flora_biomass_raw$mean_area_i


ggplot(flora_biomass_raw, 
       aes(y = biomass_i, x = log(mean_area_i), 
           color = treatment, 
           label = paste(code, sampling, plot, sep = ", "))) + 
  geom_point()+
    geom_text_repel(
      size = 3.5,                # Text size
      min.segment.length = 0.1,  # Ensures lines are always drawn
      segment.color = "gray50",  # Line color
      segment.size = 0.5         # Line thickness
    ) 



# Removing outliers from biomass_s
# At this point, it is worth considering the removal of outlier for biomass_s. To reach this information we have: 
# 1) Used abundance data gathered from the field by direct observations
# 2) Used morphological measurements taken in the field in the application of a biomass equation
# 3) Used the available data of number of individuals / m2 and species that was gathered in the field by visual estimation
# 4) Used a linear model to estimate the number of individuals per m2 for all species, samplings and plots. 
# 5) Calculated the biomass at species level by biomass_i * nind_m2. 
# So, here there is an accumulation of calculations that has undoubtfully led to error accumulation. 


par(mfrow = c(1,2))
boxplot(flora_biomass$biomass_s, main = "biomass_s")
hist(flora_biomass$biomass_s, breaks = 50, main = "biomass_s")

#With log transformation
boxplot(log(flora_biomass$biomass_s), main = "log(biomass_s)")
hist(log(flora_biomass$biomass_s), breaks = 50, main = "log(biomass_s)")



# Step 1: Calculate IQR for log-transformed biomass_s
Q1 <- quantile(log(flora_biomass$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(flora_biomass$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Step 2: Remove outliers
flora_biomass_clean <- flora_biomass %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)

# View the cleaned data

# Without ouliers 
boxplot(flora_biomass_clean$biomass_s, main = "Without ouliers")
hist(flora_biomass_clean$biomass_s, breaks = 50, main = "Without outliers")

# Without ouliers + logtrasnform
boxplot(log(flora_biomass_clean$biomass_s), main = "No outliers (log(biomass_s))")
hist(log(flora_biomass_clean$biomass_s), breaks = 50, main = "Without outliers (log(biomass_S))")

#There is almost no difference. Mark proposes to work on biomass as log(biomass) since it is the most common way of working
# in ecology





#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
# I will keep the flora_biomass not cleaned in order to compare results with and without outliers

flora_biomass <- flora_biomass %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()

flora_biomass_clean <- flora_biomass_clean %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()




rm(list = setdiff(ls(), c("flora_abrich", "flora_biomass", "flora_biomass_clean")))



