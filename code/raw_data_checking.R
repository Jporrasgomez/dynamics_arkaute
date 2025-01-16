


# Revisar samplin 15, plot 5, especie shar. Le faltan datos.
# Reisar sampling 17, plot 3, especie apar. Le faltan datos. 


rm(list = ls(all.names = TRUE))
pacman::p_load(dplyr,reshape2,tidyverse, lubridate, ggplot2, ggpubr, rpivotTable)

theme_set(theme_bw() +
            theme(
                  legend.position = "right",
                  panel.grid = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold"),
                  text = element_text(size = 11)))


# Usamos amaranthus? Outliers? el que??




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

library(ggrepel)


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
  



#REVISAR DATOS A ESTE NIVEL!!!!!

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

#Checking if we have mistakes (missing more than 2 variables with NA's)
#checkingNA <- flora_rare %>%
# filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2) %>%
#  filter(!sampling %in% c("0", "1", "2", "3", "12")) #quitamos estos muestreos porque ya sé que aqui hay muchos valores con NA
#
#View(checkingNA) #Hay 4 líneas solo. Las dejamos. 
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



ggplot(flora_rare, aes(x = date, y = Dm,
                         color = treatment, fill = treatment,
                         group = treatment, label = sampling))+
  facet_wrap( ~ plot) + 
  
  geom_point(alpha = 0.7, size = 1.2) +
  
  #geom_path(alpha = 0.7) +
  
  geom_text_repel(size = 3.5,                # Text size
                  min.segment.length = 0.1,  # Ensures lines are always drawn
                  segment.color = "gray50",  # Line color
                  segment.size = 0.5         # Line thickness
  ) +
  
  #geom_smooth(method = "loess", span = 0.3,alpha = 0.2, se = T) +
  
  scale_colour_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) +
  
  scale_fill_manual(values = c("c" = "#48A597", "w" = "#D94E47", "p" = "#3A7CA5", "wp" = "#6D4C7D")) 


  # BIOMASS 


# Ecuación biomasa####                 
#Transforming diameters into circumferences
flora_rare$cm <- round(ifelse(!is.na(flora_rare$Dm), flora_rare$Dm * pi, flora_rare$Cm), 2)
flora_rare$cb <- round(ifelse(!is.na(flora_rare$Db), flora_rare$Db * pi, flora_rare$Cb), 2)

#Calculating area of circle with circumference values
flora_rare$Ah <- ((flora_rare$cm)^2)/(4*pi)
flora_rare$Ab <- ((flora_rare$cb)^2)/(4*pi)

nrow({checkingNA <- flora_rare %>%
  filter(rowSums(is.na(select(., height, Ah, Ab))) > 1) %>%
  filter(!sampling %in% c("0", "1", "2", "3", "12"))})

 
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



#source("code/plots_functions_flora/create_reactable.R")
#create_reactable(flora_medium)
#
#rpivotTable(flora_rare)

## Simplificación de base de datos donde hacemos: 
### 1) Calculamos la media de los valores morfológicos por especie, plot y sampling

### 2) Corrección de los grupos de asteraceae, poaceae y otros que contienen varios valores morfológicos y abundancias 
###    Utilizo sum(unique(abundance)) para que se sumen los datos distintos de abundancia que un mismo grupo taxonómico contenta. 

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




hist(flora_abundance$abundance_total, breaks = 100)
boxplot(flora_abundance$abundance_total)

 
  

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
  



flora_medium <- merge(flora_medium, flora_abundance)


#Application of equation proposed by paper Perrone R. et al. 2020

d <- 1.96
z <- 2/3
flora_medium$x <- (flora_medium$height/2)*(flora_medium$Ab + flora_medium$Ah)


#hist(flora_medium$x, breaks = 100)
#boxplot <- boxplot(flora_medium$x)
#hist(log(flora_medium$x), breaks = 100)
#boxplot(log(flora_medium$x))
#boxplot(exp(log(flora_medium$x)))
#
#boxplot$out




#Removing outliers
flora_medium1 <- flora_medium[which(flora_medium$x < (as.numeric(quantile(flora_medium$x, na.rm = TRUE)[4] + (1.5 * IQR(flora_medium$x, na.rm = TRUE))))),] 
#Removing extreme outliers
flora_medium3 <- flora_medium[which(flora_medium$x < (as.numeric(quantile(flora_medium$x, na.rm = TRUE)[4] + (3 * IQR(flora_medium$x, na.rm = TRUE))))),] 





flora_medium$biomass_i <- d*(flora_medium$x^z)
flora_medium1$biomass_i <- d*(flora_medium1$x^z)
flora_medium3$biomass_i <- d*(flora_medium3$x^z)

hist((flora_medium$biomass), breaks = 100)
hist((flora_medium1$biomass), breaks = 100)
hist((flora_medium3$biomass), breaks = 100)

hist(log(flora_medium$biomass), breaks = 100)
hist(log(flora_medium1$biomass), breaks = 100)
hist(log(flora_medium3$biomass), breaks = 100)


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


hist(flora_well$abundance_total, breaks = 80)
boxplot(flora_well$abundance_total)
hist(flora_well$richness, breaks = 80)
boxplot(flora_well$richness)
hist(flora_well$biomass_total, breaks = 80)
hist(log(flora_well$biomass_total), breaks = 80)
boxplot(flora_well$biomass_total)
boxplot(log(flora_well$biomass_total))


ggplot(flora_well, aes(x = sampling, y = biomass_total, color = treatment, label = paste(sampling, plot, sep = ", "))) +
  geom_point(size = 0.02) +                # Add points with size 3
  geom_text(vjust = -1, hjust = 0.5, size = 3) +  # Add labels slightly above the points
  labs(
    x = "Sampling Date",                # Label for x-axis
    y = "Biomass Total",                # Label for y-axis
    color = "Treatment"                 # Legend title
  ) +
  theme_minimal() +                     # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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


