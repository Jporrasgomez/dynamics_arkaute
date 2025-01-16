

# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, rpivotTable)



# Usamos amaranthus? Outliers? el que??


flora_raw <- read.csv("data/flora_db_raw.csv") # Opening and transforming data(opening_floradata.R) ####

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))

flora_raw$plot <- factor(flora_raw$plot)


flora_raw$sampling <- factor(flora_raw$sampling, levels = sort(unique(flora_raw$sampling)))
flora_raw$treatment <- factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp") )
flora_raw$plot <- factor(flora_raw$plot, levels = sort(unique(flora_raw$plot)))

flora_raw <- select(flora_raw, -date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)


# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$sampling <- factor(sampling_dates$sampling)

sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew, label = TRUE)
sampling_dates$day <- day(sampling_dates$datenew)
sampling_dates$year <- year(sampling_dates$datenew)
sampling_dates$date <- sampling_dates$datenew
sampling_dates$micro_sampling <- NULL
sampling_dates$label_micro <- NULL


sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))



flora_rare <- flora_raw %>% select(sampling, sampling_date, one_month_window, omw_date, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)
#flora_rare <- flora_rare[flora_rare$species != "am", ] # Taking out super extreme outlier
#flora_rare <- flora_rare[!apply(is.na(flora_rare), 1, all), ]# removing NA full lines 


flora_rare$sampling_date <- factor(flora_rare$sampling_date,
                                   levels = c("4-May-23", "16-May-23", "14-Jun-23", "28-Jun-23", "07-Jul-23",
                                              "26-Jul-23", "22-Aug-23", "4-Sep-23", "19-Sep-23", 
                                              "4-Oct-23", "16-Oct-23", "13-Nov-23", "12-Mar-24", 
                                              "3-Apr-24", "16-Apr-24", "7-May-24", "28-May-24", 
                                              "21-Jun-24", "15-Jul-24", "19-Aug-24", "21-Oct-24")
                                   , ordered = TRUE)



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


# Ecuación biomasa####                 
#Transforming diameters into circumferences
flora_rare$cm <- round(ifelse(!is.na(flora_rare$Dm), flora_rare$Dm * pi, flora_rare$Cm), 2)
flora_rare$cb <- round(ifelse(!is.na(flora_rare$Db), flora_rare$Db * pi, flora_rare$Cb), 2)

#Calculating area of circle with circumference values
flora_rare$Ah <- ((flora_rare$cm)^2)/(4*pi)
flora_rare$Ab <- ((flora_rare$cb)^2)/(4*pi)


## Simplificación de base de datos donde hacemos: 
### 1) Calculamos la media de los valores morfológicos por especie, plot y sampling

### 2) Corrección de los grupos de asteraceae, poaceae y otros que contienen varios valores morfológicos y abundancias 
###   sampling y plot ya que en su día las diferenciabamos en campo. Al calcular la media de la abundancia, corregimos este problema 

taxongroups <- flora_rare %>%
  filter(code %in% c("poaceae", "asteraceae", "tosp", "orchidaceae"))%>%
  group_by(code, sampling, sampling_date, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = sum(unique(abundance), na.rm = T), #here we sum abundances 
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))


species <- anti_join(flora_rare, taxongroups, by = "code") %>%
  group_by(code, sampling, sampling_date, one_month_window, omw_date, plot, treatment, date, month, species, family, genus_level, species_level) %>%
  summarize(abundance = mean(abundance, na.rm = T), #here we mean abundances (fake mean, species in the same plot and sampling have the same abundance info)
            height = mean(height, na.rm = T),
            Ah = mean(Ah, na.rm = T),
            Ab = mean(Ab, na.rm = T))

flora_medium <- bind_rows(taxongroups, species)


# RICHNESSS
flora_richness <- summarise(group_by(flora_medium, plot, sampling),
                            richness = n_distinct(code, na.rm = T)) ##adding number of species

flora_medium <- merge(flora_medium, flora_richness)

# ABUNDANCE

flora_abundance <- summarise(group_by(flora_medium, sampling, date, treatment, plot),
                             abundance_total = sum(abundance, na.rm = T)) ##adding number of species

  
  
  
  flora_medium <- merge(flora_medium, flora_abundance)


# BIOMASS 



#Application of equation proposed by paper Perrone R. et al. 2020

d <- 1.96
z <- 2/3
flora_medium$x <- (flora_medium$height/2)*(flora_medium$Ab + flora_medium$Ah)


#Removing outliers
flora_medium1 <- flora_medium[which(flora_medium$x < (as.numeric(quantile(flora_medium$x, na.rm = TRUE)[4] + (1.5 * IQR(flora_medium$x, na.rm = TRUE))))),] 
#Removing extreme outliers
flora_medium3 <- flora_medium[which(flora_medium$x < (as.numeric(quantile(flora_medium$x, na.rm = TRUE)[4] + (3 * IQR(flora_medium$x, na.rm = TRUE))))),] 



# Explorar esto
#outl_limit <- 3
#outliers_sd <- flora_medium %>%
#  filter(x < mean(flora_medium$x, na.rm = TRUE) - outl_limit  * sd(flora_medium$x, na.rm = TRUE) |
#           x > mean(flora_medium$x, na.rm = TRUE) + outl_limit  * sd(flora_medium$x, na.rm = TRUE)) 



#cook <- lm(x ~ treatmnt, flora_medium)
#plot(cook, which = 4)


flora_medium$biomass_i <- d*(flora_medium$x^z)
flora_medium1$biomass_i <- d*(flora_medium1$x^z)
flora_medium3$biomass_i <- d*(flora_medium3$x^z)



# Incluimos numero de individuos/m2 medidos posteriormente en el campo

nind_lm_data <- read.csv("data/nind_lm_data.csv")

#With outliers
flora_well <- merge(flora_medium, nind_lm_data, by = "code")

#calculation of number of indivuals per m2 with the regression data for each species
flora_well$nind_m2 <- flora_well$intercept + flora_well$abundance * flora_well$slope  

# Shitty correction
flora_well <- flora_well %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))

# Calculation of species biomass per square meter by multiplying biomass at individual level by the number of individuals per square meter.
flora_well$biomass <- flora_well$biomass_i *flora_well$nind_m2

#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
flora_well_biomass_total <- summarise(group_by(flora_well, plot, sampling),
                                      biomass_total =  sum(biomass, na.rm = TRUE))


flora_well <- merge(flora_well, flora_well_biomass_total)
flora_well_complete <- flora_well %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family, genus_level,
         species_level, height, Ah, Ab, biomass_i, nind_m2, intercept, slope, r_squared,
         p_value, biomass, richness, abundance_total, biomass_total)
flora_well <- flora_well %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family,
         genus_level, species_level, richness, abundance_total, biomass_total)




#Without outliers

flora_well1 <- merge(flora_medium1, nind_lm_data, by = "code")

flora_well1$nind_m2 <- flora_well1$intercept + flora_well1$abundance * flora_well1$slope
flora_well1 <- flora_well1 %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))
flora_well1$biomass <- flora_well1$biomass_i *flora_well1$nind_m2
flora_well1_biomass_total <- summarise(group_by(flora_well1, plot, sampling),
                                       biomass_total =  sum(biomass, na.rm = TRUE))
flora_well1 <- merge(flora_well1, flora_well1_biomass_total)
flora_well1_complete <- flora_well1 %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family, genus_level,
         species_level, height, Ah, Ab, biomass_i, nind_m2, intercept, slope, r_squared,
         p_value, biomass, richness, abundance_total, biomass_total)
flora_well1 <- flora_well1 %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family,
         genus_level, species_level, richness, abundance_total, biomass_total)


#hist(flora_well1$abundance_total, breaks = 30)
#boxplot(flora_well1$abundance_total)
#hist(flora_well1$richness, breaks = 20)
#boxplot(flora_well1$richness)
#hist(flora_well1$biomass_total, breaks = 30)
#hist(log(flora_well1$biomass_total), breaks = 30)
#boxplot(flora_well1$biomass_total)
#boxplot(log(flora_well1$biomass_total))

#Without extreme outliers

flora_well3 <- merge(flora_medium3, nind_lm_data, by = "code")

flora_well3$nind_m2 <- flora_well3$intercept + flora_well3$abundance * flora_well3$slope
flora_well3 <- flora_well3 %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 0.1, nind_m2))
flora_well3$biomass <- flora_well3$biomass_i *flora_well3$nind_m2
flora_well3_biomass_total <- summarise(group_by(flora_well3, plot, sampling),
                                       biomass_total =  sum(biomass, na.rm = TRUE))
flora_well3 <- merge(flora_well3, flora_well3_biomass_total)
flora_well3_complete <- flora_well3 %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family, genus_level,
         species_level, height, Ah, Ab, biomass_i, nind_m2, intercept, slope, r_squared,
         p_value, biomass, richness, abundance_total, biomass_total)
flora_well3 <- flora_well3 %>%
  select(sampling, sampling_date, one_month_window, omw_date, date, month, treatment, plot, abundance, code, species, family,
         genus_level, species_level, richness, abundance_total, biomass_total)


#hist(flora_well1$abundance_total, breaks = 30)
#boxplot(flora_well1$abundance_total)
#hist(flora_well1$richness, breaks = 20)
#boxplot(flora_well1$richness)
#hist(flora_well1$biomass_total, breaks = 30)
#hist(log(flora_well1$biomass_total), breaks = 30)
#boxplot(flora_well1$biomass_total)
#boxplot(log(flora_well1$biomass_total))

flora <- flora_well
flora_complete <- flora_well_complete
flora1 <- flora_well1
flora1_complete <- flora_well1_complete
flora3 <- flora_well3
flora3_complete <- flora_well3_complete

#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia


#Especies para las cuales no tenemos datos

#flora %>% write.csv("data/flora_db.csv", row.names  = F)
#flora_complete %>% write.csv("data/flora_complete_db.csv", row.names  = F)



rm(list = setdiff(ls(), c("flora", "flora1", "flora3", "flora_complete", "flora1_complete", "flora3_complete")))



