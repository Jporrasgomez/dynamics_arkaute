


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, rpivotTable, ggrepel, broom)

theme_set(theme_bw() +
            theme(
              legend.position = "right",
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold"),
              text = element_text(size = 11)))



# OPENING DATA ####

flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)


flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))

flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)

boxplot(flora_raw$Dm, main = "Dm")
boxplot(flora_raw$Cm, main = "Cm")
boxplot(flora_raw$Db, main = "Db")
boxplot(flora_raw$Cb, main = "Cb")

#Checking for errors:

# is there any line with less than 2 NA between height, cb, cm, db and dm?  This is: did we slip some extra data in some lines?
nrow({checking <- flora_raw %>%
  filter(rowSums(is.na(select(., height, Cb, Db, Cm, Dm))) < 2)})

#is there any line with more than 2 NA among the same variables? This is: are we missing some data?
nrow({checking2 <- flora_raw %>%
  filter(rowSums(is.na(select(., height, Cb, Db, Cm, Dm))) > 2) %>%
  filter(!sampling %in% c("0", "1", "2","3", "12"))})


# is there any dm-cb /db-cm relationship that is not expected?



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


# Adding sampling dates

sampling_dates <- read.csv("data/sampling_dates.csv")

sampling_dates$date <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$date, label = TRUE)
sampling_dates$day <- day(sampling_dates$date)
sampling_dates$year <- year(sampling_dates$date)

sampling_dates <- sampling_dates %>% 
  select(sampling, date, day, month, year, one_month_window, omw_date)

sampling_dates$sampling <- as.factor(sampling_dates$sampling )

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))

flora_rare <- flora_raw %>% select(sampling, one_month_window, omw_date, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)

# Adding species information

species_code <- read.csv("data/species_code.csv")
species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))

flora_rare <- merge(flora_rare, species_code, by = "code") 


# VARIABLES #######

## BIOMASS at individual level####

# Morphological values

#Transforming Diameters values by adding 0.01 since there are some values = 0.00 due to a fail in the measurement tool. 
flora_rare$Dm <- flora_rare$Dm + 0.01
flora_rare$Db <- flora_rare$Db + 0.01

#Transforming diameters into circumferences
flora_rare$cm <- round(ifelse(!is.na(flora_rare$Dm), flora_rare$Dm * pi, flora_rare$Cm), 2)
flora_rare$cb <- round(ifelse(!is.na(flora_rare$Db), flora_rare$Db * pi, flora_rare$Cb), 2)

#Calculating area of circle with circumference values
flora_rare$Ah <- ((flora_rare$cm)^2)/(4*pi)
flora_rare$Ab <- ((flora_rare$cb)^2)/(4*pi)


# !! Checking if the area of any individua is above 1 square meter
nrow({checkingNA <- flora_rare %>%
  filter(rowSums(is.na(select(., height, Ah, Ab))) > 1) %>%  
  filter(!sampling %in% c("0", "1", "2", "3", "12"))})

# !! Checking if there is any Ab - Ah relationship that is out of order

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


## Database simplification:  
### 1) We calculate average of morphological values per species, plot and sampling
### 2) Correction of asteraceae, poaceae and another taxonomical froups that contain different morph. values and abundances due to changes on identification criteria along the experiment 
###    (ie. some species of poaceae were identified in the first samplings, but after many samplings we realized it was very difficult to keep identifying them, so we aggregate them under poaceae)
###    I use sum(unique(abundance)) in order to add all different abundance data that a same taxonomic group contains
###    For Ah, Ab and height I calculate the mean value of the different values contained within each taxonomic group. 

taxongroups <- flora_rare %>%
  filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
  group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = sum(unique(abundance), na.rm = T), #here we sum abundances 
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))

# Here, I just calculate the mean values of height, Ab, Ah and abundance per species, plot and sampling (abunance being a fake mean, since all abundance values are the same)
species <- anti_join(flora_rare, taxongroups, by = "code") %>%
  group_by(code, sampling, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = mean(abundance, na.rm = T), #here we mean abundances (fake mean, species in the same plot and sampling have the same abundance info)
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))

flora_medium <- bind_rows(taxongroups, species)


#Application of equation proposed by paper Perrone R. et al. 2020

d <- 1.96
z <- 2/3
flora_medium$x <- (flora_medium$height/2)*(flora_medium$Ab + flora_medium$Ah)
flora_medium$biomass_i <- d*(flora_medium$x^z)

hist(flora_medium$x)
hist(flora_medium$biomass_i)

# !! Checking biomass at individual level
# !!!!!!!!!!!!!!!!!!!! not working_ checcckkkkk
ggplot(flora_medium,
       aes(x = date, y = x, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  #geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_point() +
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  labs(
    x = "date", y = x,     # Label for x-axis
    color = "Treatment"  # Legend title for color
  )

ggplot(flora_medium,
       aes(x = date, y = biomass_i, color = treatment, label = paste(sampling, plot, code, sep = ", "))) +
  #geom_point(aes(alpha = 0.02, shape = treatment), size = 0.9) +  # Points with alpha and shape mapped
  geom_point() +
  geom_text_repel(
    size = 3.5,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5       # Line thickness
  ) +
  labs(
    y = "biomass_i", x = "date",       # Label for x-axis
    color = "Treatment"  # Legend title for color
  )


## RICHNESSS #####
flora_richness <- summarise(group_by(flora_medium, plot, sampling),
                            richness = n_distinct(code, na.rm = T)) ##adding number of species

flora_medium <- merge(flora_medium, flora_richness)

## ABUNDANCE #####

flora_abundance <- summarise(group_by(flora_medium, sampling, date, treatment, plot),
                             abundance_total = sum(abundance, na.rm = T)) ##adding number of species

flora_medium <- merge(flora_medium, flora_abundance)


flora_abrich <- flora_medium

# !! Checking abundance evolution

ggplot(flora_abundance, aes(x = date, y = abundance_total,
                            color = treatment, fill = treatment,
                            group = treatment, label = sampling))+
  facet_wrap( ~ plot) + 
  
  #geom_point(alpha = 0.7, size = 1.2) +
  
  geom_path(alpha = 0.7) +
  
  geom_text(size = 3) +
  
  #geom_smooth(method = "loess", span = 0.3,alpha = 0.2, se = T) +
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 


# !! Checking which are the most abundant speces

flora_medium$code_ordered <- reorder(flora_medium$code, -flora_medium$abundance, FUN = mean)

ggplot(flora_medium, aes(x = code_ordered, y = abundance)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

flora_nobs <- flora_medium %>% 
  group_by(code) %>% 
  summarize(n_observations= n(),
            mean_abundance = mean(abundance))

flora_nobs$abnobs <- flora_nobs$mean_abundance * flora_nobs$n_observations

ggplot(flora_nobs, aes(x = mean_abundance, y = n_observations, label = code, color = abnobs))+
  geom_point() + 
  scale_color_gradient(low = "blue4", high = "red2") +  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  )

# Checking which are the species with the highest number of observations (being an observation finding a species in one a plot at a certain time)
flora_nobs$code_nobs <- factor(flora_nobs$code,
                               levels = flora_nobs$code[order(flora_nobs$n_observations, decreasing = TRUE)])
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


# BIOMASS at species level ####

# Now that we have biomass at individual level (biomass_i), to calculate biomass at species (population) level,
# we need an estimation of the number of individuals/m2. Therefore: biomass_i * n_i/m2 = biomass/m2. The problem is that we do not have this information for all measurements
# (this is, all species, at all samplings and plots) since we start gathering the information from sampling 13. Therefore, we can use the available data of the 
# number of individuals and to create a linear regression model with the abundance data for each species. This way,we can extrapolate the gaps for number of individuals and estimate
# its value with the abundance information (eg: the species z had an abundance of x in the sampling s, plot p. We know that n_ind = a + bz. We need to know the linear regression parameters
# and its reliability (p-value and R2 per each species))

# To be able to estimate abundance based on the number of individuals we need data were abundance and number of individuals per species,
# plot and sampling were available. Since we have been learning the application of this non-destructive methodology in the go, we have 2 different groups of data:
# 
#  1. Since sampling 12, we have been estimating the number of individuals per species and plot based on direct field observations.
#

nind <- read.csv("data/n_individuals.csv")

nind$code <- as.factor(nind$code)

nind <- select(nind, sampling, plot, code, nind_m2, abundance)

#Same as before, we hace to make a correction fro the taxonomic groups like asteraceae, poaceae, etc. 
nind <- nind %>%
  group_by(sampling, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2),
            abundance = sum(abundance))  ## Las especies de asteraceaes que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

nind$sampling <- as.factor(nind$sampling)
nind$plot <- as.factor(nind$plot)
nind$nind_m2 <- as.numeric(nind$nind_m2)
nind$abundance <- as.numeric(nind$abundance)

#  2. Up to sampling 11, We only measured the morphological parameters of 5 individuals per specie as maximum.
# If there were less than 5 species, we know that number was the total amount of individuals in the plot.


flora_nind <- flora_raw %>%
  filter(!sampling %in% c("12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, plot, code) %>%
  mutate(n_individuals = n())

flora_nind <- flora_nind %>%
  group_by(sampling, plot, code,abundance) %>%
  summarize(n_individuals_mean = mean(n_individuals, na.rm = T))  #corrección de asteraceae y poaceae #corrección de asteraceae y poaceae

n_individuals_old_data <- flora_nind %>%
  filter(n_individuals_mean < 5)
names(n_individuals_old_data)[names(n_individuals_old_data) == "n_individuals_mean"] <- "nind_m2"


# Complete dataset for number of individuals

nind <- bind_rows(nind, n_individuals_old_data)



#Once that we have the dataset for stablishing the estimation, we have applied a linear regression model where we plot the number of individuals per m$^2$ as a function of abundance.
#There are some species for which just one individual was found in one plot at different samplings, meaning that these won't be considered in the regression
#and their biomass will be the biomass of the measured individual. These species are *Rumex crispus*, *Amaranthus sp.*, *Kickxia supuria* 
#and an unidentified species of the family *brasicaceae*. For the lineal regression we applied the code: lm(nind_m2 ~ abundance). On example of the output is the following:  


### Lineal regression. 


nind$code <- as.character(nind$code)

# There are some species for which the lm model is actually no needed because there has been
# just one individual found in one plot at different times. So the estimated biomass of that individual will be the biomass of the species for that plot and time, 
# These are "rucr", "amsp", "kips" and "brasicaceae" since these 

one_ind_species <- c("rucr", "amsp", "kisp","brasicaceae")


# Taking out these species from the nind database
nind <- nind %>% 
  filter(!code %in% one_ind_species)

#Preparation of the loop to iterate the lm to each species. 

code_levels <- unique(nind$code)
gglist <- list()

nind_lm_data <- matrix(nrow = length(code_levels), ncol = 6)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_observations")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ abundance, data = nind_i)
  
  # Extract coefficients, R^2, and p-value
  lm_i_summary <- summary(lm_i)
  lm_i_tidy <- tidy(lm_i)
  lm_i_glance <- glance(lm_i)
  
  intercept_i <- lm_i_tidy$estimate[1]
  slope_i <- lm_i_tidy$estimate[2]
  r_squared_i <- lm_i_glance$r.squared
  p_value_i <- lm_i_tidy$p.value[2]
  n_observations_i <- nrow(nind_i)
  
  counter <- counter + 1
  
  gglist[[counter]] <- ggplot(nind_i, aes(x = abundance, y = nind_m2)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linear Relationship for", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R^2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
         "Number of observations", n_observations_i),
         x = "Abundance",
         y = "Numbers of individual per m2") +
    theme_minimal()
  
  nind_lm_data$code[counter] <- code_levels[i]
  nind_lm_data$intercept[counter] <- intercept_i
  nind_lm_data$slope[counter] <- slope_i
  nind_lm_data$r_squared[counter] <- r_squared_i
  nind_lm_data$p_value[counter] <- p_value_i
  nind_lm_data$n_observations[counter] <- n_observations_i

  
}

#We can take a look to several plots
print(gglist[[6]])


# As we can see, for this example the R$^2$ is quite low ( R$^2$ = 0.23). The fact is that, if we take a look to
# how well the model fit for all of our species, there are many considerations. 

nind_lm_data$posneg_slope <- ifelse(nind_lm_data$slope < 0, paste("negative"), paste("positive"))

#Let's check results
results <- 
  ggplot(nind_lm_data, aes( x = r_squared, y = p_value, 
                            label = paste(code, n_observations, sep = ", "), color = posneg_slope))+
  geom_point()+ 
  geom_vline(xintercept = 0.3, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "gray40") +
  
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",# Line color
    segment.size = 0.5, 
    max.overlaps = 15# Line thickness
  )

print(results)

#The lm model fits well (R^2 >= 0.3 and p-value < 0.1) for a few species: 

species_lm <- nind_lm_data %>% 
  filter(r_squared > 0.3) %>% 
  filter(p_value < 0.1)

species_lm_codes <- species_lm$code

nind_lm_species <- nind_lm_data %>% 
  filter(code %in% species_lm_codes)


#So we have 

one_ind_species  #No need of lm
species_lm_codes  #Lm works 


# Total biomass of individuals for which there is no need of LM: 
flora_well_one_ind <- flora_medium %>% 
  filter(code %in% one_ind_species) 

flora_well_one_ind$biomass_total <- flora_well_one_ind$biomass_i


# Total biomass of species for which the LM fitted: 

flora_well_lm <-  merge(flora_medium, nind_lm_species, by = "code")


#calculation of number of indivuals per m2 with the regression data for each species
flora_well_lm$nind_m2 <- flora_well_lm$intercept + flora_well_lm$abundance * flora_well_lm$slope  

# Shitty correction
flora_well_lm <- flora_well_lm %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))

# Calculation of species biomass per square meter by multiplying biomass at individual level by the number of individuals per square meter.
flora_well_lm$biomass <- flora_well_lm$biomass_i *flora_well_lm$nind_m2

#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
flora_well_lm_biomass_total <- summarise(group_by(flora_well_lm, plot, sampling),
                                      biomass_total =  sum(biomass, na.rm = TRUE))


flora_well_lm <- merge(flora_well_lm, flora_well_lm_biomass_total)

flora_well_biomass_lm <- bind_rows(flora_well_lm, flora_well_one_ind)

flora_biomass_lm_complete <- flora_well_biomass_lm %>%
  select(sampling, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family, genus_level,
         species_level, height, Ah, Ab, biomass_i, nind_m2, intercept, slope, r_squared,
         p_value, biomass, richness, abundance_total, biomass_total)

flora_biomass_lm <- flora_well_biomass_lm %>%
  select(sampling, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family,
         genus_level, species_level, richness, abundance_total, biomass_total)




rm(list = setdiff(ls(), c("flora_abrich", "flora_biomass_lm_complete", "flora_biomass_lm")))






