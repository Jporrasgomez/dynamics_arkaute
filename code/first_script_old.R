


pacman::p_load(dplyr,reshape2,tidyverse, lubridate)


# Usamos amaranthus? Outliers? el que??
# ni species?





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
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL
  

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))


flora <- flora_raw %>% select(sampling, plot, treatment, code, abundance, height, Cb, Db, Cm, Dm, date, month)
#flora <- flora[flora$species != "am", ] # Taking out super extreme outlier
#flora <- flora[!apply(is.na(flora), 1, all), ]# removing NA full lines 


anyNA(flora_raw)

#Checking if we have mistakes (missing more than 2 variables with NA's)
#View(flora %>%
#  filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2))
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Changes into flora

flora <- flora %>%
  group_by(plot, sampling, code) %>%
  mutate(n_individuals = n()) %>% #number of individuals per species
  ungroup()


#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos
flora$Dm <- flora$Dm + 0.01
flora$Db <- flora$Db + 0.01




# Ecuación biomasa####                 
#Transforming diameters into circumferences
flora$cm <- round(ifelse(!is.na(flora$Dm), flora$Dm * pi, flora$Cm), 2)
flora$cb <- round(ifelse(!is.na(flora$Db), flora$Db * pi, flora$Cb), 2)

flora$Ah <- ((flora$cm)^2)/4*pi
flora$Ab <- ((flora$cb)^2)/4*pi

#Application of equation proposed by paper Perrone R. et al. 2020

d <- 1.96
z <- 2/3
flora$x <- (flora$height/2)*(flora$Ab + flora$Ah)
flora$biomass <- d*(flora$x^z)

flora <- flora %>%
  group_by(sampling, date, month, treatment, plot, abundance, code) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * (abundance/100)) %>%  #Calculating biomass per species, plot and sampling
  distinct(sampling, date, month, plot, treatment, abundance, code, biomass)
#Mirar si la distribución de los datos de biomasa tienen sentido una vez aplicada la estimacion en base a la abundancia

#Transformo en NA los valores 0 de biomasa, que corresponden a los datos de los samplings 0, 1, 2 y los 26 datos del 3. 
#Esto es para el gráfico, pero hay que ver las correlaciones entre H, cb, cm...
#! Esdte paso no es necesario ahora con flora1, ya que al generar la df hicimos un na.rm = T
#flora1$biomass <- ifelse(flora1$biomass == 0, NA, flora1$biomass)



# RICHNESSS
flora_n_species <- summarise(group_by(flora, plot, sampling),
                            n_species = n_distinct(code, na.rm = T)) ##adding number of species

flora <- merge(flora, flora_n_species)
  
#Correction of abundance and biomass for poaceae and asteraceae.These families put together several species with different values of abundance. 
  
flora <- summarise(group_by(flora, sampling, plot, date, month, treatment, code, n_species), 
                   abundance = sum(abundance, na.rm = T), #La abundancia total de las especies que forman poaceae y asteraceae 
                   biomass = mean(biomass, na.rm = T))    #la biomasa media de las especies que conformaban poaceae y asteraceae. No se suma, se hace la media para ser consistente con la forma de calcular biomasa


#Adding species information
species_code <- read.csv("data/species_code.csv")
species_code <- select(species_code, species, code, family, genus_level, species_level)
species_code <- species_code %>%
  mutate(across(where(is.character), as.factor))




# WE WORK WITH IDENTIFIED SPECIES!!


flora <- merge(flora, species_code, by = "code") 



#dummy_rows <- anti_join(flora, flora0)
#dummy_rows$species <- NA
#dummy_rows$family <- NA
#dummy_rows$genus_level <- NA
#dummy_rows$species_level <- NA
#flora <- rbind(flora0, dummy_rows) #addind "dummy rows" which are 8 rows. For sampling 1 treatment wp and p, where we have no data but we need them to be present for some analysis


#flora %>% write.csv("data/flora_db.csv", row.names  = F)


rm(species_code)
rm(flora_raw)
rm(sampling_dates)
rm(desired_order)
rm(desired_order_treat)
rm(flora_n_species)
rm(dummy_rows)
rm(flora0)
rm(d)
rm(z)

