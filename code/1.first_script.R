





rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr, reshape2, tidyverse, lubridate, ggplot2, ggpubr, rpivotTable)

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

flora_raw <- flora_raw %>% 
  rename(abundance_s = abundance)

flora_raw$plot <- factor(flora_raw$plot)


flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))

flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$date <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$date, label = TRUE)
sampling_dates$day <- day(sampling_dates$date)
sampling_dates$year <- year(sampling_dates$date)

sampling_dates <- sampling_dates %>% 
  select(sampling, sampling_date, date, day, month, year, one_month_window, omw_date)

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))

flora_rare <- flora_raw %>% select(sampling, sampling_date, one_month_window,
                                   omw_date, plot, treatment, code, abundance_s,
                                   height, Cb, Db, Cm, Dm, date, month)




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
  filter(rowSums(is.na(select(., height, Ah, Ab))) > 1) %>%  #Checking if the area of any individua is above 1 square meter
  filter(!sampling %in% c("0", "1", "2", "3", "12"))})


taxongroups <- flora_rare %>%
  filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
  group_by(code, sampling, sampling_date, one_month_window, omw_date, plot, 
           treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance_s = sum(unique(abundance_s), na.rm = T), #here we sum abundances 
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))


species <- anti_join(flora_rare, taxongroups, by = "code") %>%
  group_by(code, sampling, sampling_date, one_month_window, omw_date, plot,
           treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance_s = mean(abundance_s, na.rm = T), #here we mean abundances (fake mean, species in the same plot and sampling have the same abundance info)
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
  mutate(abundance_community = sum(abundance_s, na.rm = T)) %>% 
  ungroup()


flora_abrich <- flora_medium

# !!Checking abundance




## BIOMASS AT SPECIES LEVEL ###########

#For biomass, we do not have information for samplings 0, 1, 2 and 12. 

flora_biomass_raw <- flora_medium %>% 
  filter(!sampling %in% c("0", "1", "2", "12"))

# To calculate the biomass at species level (biomass_s) on a plot at sampling based on the biomass at individual level (biomass_i) we
# need to know the number of individuals that were on that plot for that species. Because biomass_i * number of indivuals /m2 (ninc_m2). However, 
# the information about nind_m2 is not available for all species at all plots an samplings. Therefore, we have to estimate it. 

#But, before that, we know that there are some species for which we only have measured 1 individual per plot and sampling every time we have spot it. 
# Therefore, for these species we do not need to estimate the number of individuals. In this case, biomass_i = biomass_s. 


one_ind_species <- c("rucr", "amsp", "kisp","brasicaceae")

flora_biomass_oneind <- flora_biomass_raw %>% 
  filter(code %in% one_ind_species)

flora_biomass_oneind$biomass_s <- flora_biomass_oneind$biomass_i

flora_biomass_oneind <- flora_biomass_oneind %>% 
  group_by(sampling, plot, code) %>% 
  mutate(n_observations = n()) %>% 
  ungroup()

flora_biomass_raw <- flora_biomass_raw %>% 
  filter(!code %in% one_ind_species)
## Estimating number of individuals

# To be able to estimate abundance based on the number of individuals we need data were abundance and number of individuals per species,
# plot and sampling were available. Since we have been learning the application of this non-destructive methodology in the go,
# we have 2 different groups of data:

#1.  Since sampling 12, we have been estimating the number of individuals per species and plot based on direct field observations.


nind1 <- read.csv("data/n_individuals.csv")
#source("code/first_script_old.R")

nind1 <- nind1 %>% 
  rename(abundance_s = abundance)

nind1$code <- as.factor(nind1$code)

nind1 <- select(nind1, sampling, plot, code, nind_m2, abundance_s)
#flora_nind <-  flora %>% select(code, family) %>% distinct(code, .keep_all = TRUE)

nind1 <- nind1 %>%
  group_by(sampling, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance_s = sum(abundance_s))

nind1$sampling <- as.factor(nind1$sampling)
nind1$plot <- as.factor(nind1$plot)
nind1$code <- as.factor(nind1$code)
nind1$nind_m2 <- as.numeric(nind1$nind_m2)
nind1$abundance_s <- as.numeric(nind1$abundance_s)

## Las especies de asteraceae que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

#2.  Up to sampling 11, We only measured the morphological parameters of 5 individuals per specie as maximum. 
#    If there were less than 5 species, we know that number was the total amount of individuals in the plot.

nind2 <- flora_raw %>%
  filter(!sampling %in% c("12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, plot, code) %>%
  mutate(n_individuals = n())

nind2 <- nind2 %>%
  group_by(sampling, plot, code,abundance_s) %>%
  summarize(n_individuals_mean = mean(n_individuals, na.rm = T))  #corrección de asteraceae y poaceae #corrección de asteraceae y poaceae

nind2 <- nind2 %>%
  filter(n_individuals_mean < 5)
names(nind2)[names(nind2) == "n_individuals_mean"] <- "nind_m2"

nind <- bind_rows(nind1, nind2)

# ** We could discuss the question: if we have some species for which the number of individuals measured in the plot was the total amount
# of individuals in that plot, why we are not calculating the biomass of that species as biomass_s = biomass_1 + biomass_2 ...biomass_i ?
# I think this is worthy to be considered. At the begining we did this. But, I think we decided that, for the shake of consistency, 
# we do not want to mix biomass_s estimation approaches for the same species. For the case of one_ind_species, these species have only been
# spotted isolately. Therefore, biomass_i = biomass_s is valid for these species since it will be the only approach used for them. 

# Once that we have the dataset for stablishing the estimation, we have applied a linear regression model where we plot the number of individuals
# per m$^2$ as a function of abundance. There are some species for which just one individual was found in one plot at different samplings,
# meaning that these won't be considered in the regression and their biomass will be the biomass of the measured individual.
# These species are *Rumex crispus*, *Amaranthus sp.*, *Kickxia supuria* and an unidentified species of the family *brasicaceae*.
# For the lineal regression we applied the code: lm(nind_m2 \~ abundance). On example of the output is the following:

### Regresión lineal. 

nind$code <- as.character(nind$code)

#The lm model is actually no needed for species "rucr", "amsp", "kips" and "brasicaceae" since these are
# just one individual found in one plot at different times. So the estimated biomass will be for 1 square meter

nind <- nind %>% 
  filter(!code %in% one_ind_species)


code_levels <- unique(nind$code)
gglist <- list()

nind_lm_data <- matrix(nrow = length(code_levels), ncol = 6)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_observations")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

library(broom)

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ abundance_s, data = nind_i)
  
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
  
  gglist[[counter]] <- ggplot(nind_i, aes(x = abundance_s, y = nind_m2)) +
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

#print(gglist[[6]])

# As we can see, for this example the R$^2$ is quite low ( R$^2$ = 0.23). The fact is that, if we take a look to how well
# the model fit for all of our species, there are many considerations.



species_lm<- nind_lm_data %>% 
  filter(r_squared > 0.3) %>% 
  filter(p_value < 0.1)



species_lm_codes <- unique(species_lm$code)

nind_lm_species <- nind_lm_data %>% 
  filter(code %in% species_lm_codes )

excluded_species_lm <- {nind_lm_data %>% 
    filter(!code %in% species_lm_codes )}$code



flora_biomass_lm <- merge(flora_biomass_raw, nind_lm_species)


#calculation of number of indivuals per m2 with the regression data for each species
flora_biomass_lm$nind_m2 <- flora_biomass_lm$intercept + flora_biomass_lm$abundance_s * flora_biomass_lm$slope


# Since there are some species for which its intercept is negative, the lm consider that nind_m2 <0 when abundance = 0. And that
# cannot be the case. There are 3 species. We can either delete these species or make this "shitty" correction by sayin that if
# nind_m2 < 0 , then nind_m2 = 0
# Shitty correction ?????????????????????????????????????
flora_biomass_lm <- flora_biomass_lm %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))


# Calculation of species biomass per square meter by multiplying biomass at individual (biomass_i) level by the 
# number of individuals per square meter (nind_m2)
flora_biomass_lm$biomass_s <- flora_biomass_lm$biomass_i * flora_biomass_lm$nind_m2


#Putting together lm species and one ind species
flora_biomass <- bind_rows(flora_biomass_oneind, flora_biomass_lm)


# There are some NAs comming from sampling 3, where we did not measure morphological data from species with very low abundance (decisions of
# tired Javi and Laura :( ))

# Removing outliers from biomass_s
# At this point, it is worth considering the removal of outlier for biomass_s. To reach this information we have: 
# 1) Use abundance data gathered from the field by direct observations
# 2) Use morphological measurements taken in the field in the application of a biomass equation
# 3) Use the available data of number of individuals / m2 and species that was gathered in the field by visual estimation
# 4) use a linear model to estimate the number of individuals per m2 for all species, samplings and plots. 
# 5) Calculate the biomass at species level by biomass_i * nind_m2. 
# So, here there is an accumulation of calculations that has undoubtfully led to error accumulation. 



# Temporal way of removing outliers

#par(mfrow = c(1,2))
#boxplot(flora_biomass$biomass_s, main = "biomass_s")
#hist(flora_biomass$biomass_s, breaks = 50, main = "biomass_s")
#
##With log transformation
#boxplot(log(flora_biomass$biomass_s), main = "log(biomass_s)")
#hist(log(flora_biomass$biomass_s), breaks = 50, main = "log(biomass_s)")



# Step 1: Calculate IQR for log-transformed biomass_s
Q1 <- quantile(log(flora_biomass$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(flora_biomass$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Step 2: Remove outliers
flora_biomass_clean <- flora_biomass %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)

# View the cleaned data

# Without ouliers 
#boxplot(flora_biomass_clean$biomass_s, main = "Without ouliers")
#hist(flora_biomass_clean$biomass_s, breaks = 50, main = "Without outliers")
#
## Without ouliers + logtrasnform
#boxplot(log(flora_biomass_clean$biomass_s), main = "No outliers (log(biomass_s))")
#hist(log(flora_biomass_clean$biomass_s), breaks = 50, main = "Without outliers (log(biomass_S))")

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

species_biomass_lm <- unique(flora_biomass$code)




rm(list = setdiff(ls(), c("flora_abrich", "flora_biomass", "flora_biomass_clean", "species_biomass_lm")))



