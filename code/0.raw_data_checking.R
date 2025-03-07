


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
{

flora_raw <- read.csv("data/flora_db_raw.csv")

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor),
         plot = factor(plot),
         sampling = factor(sampling, levels = sort(unique(flora_raw$sampling))),
         treatment =  factor(flora_raw$treatment, levels =  c("c", "w", "p", "wp")),
         plot = factor(plot, levels = sort(unique(flora_raw$plot)))) %>% 
  select(-date, -category, -OBS, -ni.agrupado, -familia, -species_old_1, -species_old_2)


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

{ggcbdm <- 
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
}

# Adding dates
sampling_dates <- read.csv("data/sampling_dates.csv") %>% 
  mutate(sampling = as.factor(sampling),
         date = ymd(date), 
         month = month(date, label = TRUE),
         day = day(date), 
         year = year(date)) %>% 
  select(sampling, date, day, month, year, one_month_window, omw_date) %>% 
  mutate(across(where(is.character), as.factor))


flora_rare <- flora_raw %>% 
  right_join(sampling_dates, by = join_by(sampling)) %>% 
  select(sampling, one_month_window, omw_date, plot, treatment,
         code, abundance, height, Cb, Db, Cm, Dm, date, month)



#Checking if we have mistakes (missing more than 2 variables with NA's)
nrow({checkingNA <- flora_rare %>%
 filter(rowSums(is.na(select(., Dm, Db, Cm, Cb))) > 2) %>%
  filter(!sampling %in% c("0", "1", "2", "3", "12"))}) #quitamos estos muestreos porque ya sé que aqui hay muchos valores con NA

#
##View(checkingNA) #Hay 4 líneas solo. Las dejamos. 
#Estan todos los datos de los muestreos 0, 1 y 2 y los 
#26 datos del muestreo 3 donde no cogíamos medidas si la abundancia era menor de 5.


#Adding species information
species_code <- read.csv("data/species_code.csv") %>% 
  select(species, code, family, genus_level, species_level, growing_type) %>%
  mutate(across(where(is.character), as.factor))


# WE WORK WITH IDENTIFIED SPECIES!!

flora_rare <- merge(flora_rare, species_code, by = "code") 

# Cambios de los datos morfológicos antes de agrupar la base de datos por especie, muestro y sampling 

#Sumar 0.01 cm a los diámetros por el error del calibre con el que medimos


#
#flora_rare$Dm <- flora_rare$Dm + 0.01
#flora_rare$Db <- flora_rare$Db + 0.01



flora_rare <- flora_rare %>%
  mutate(Dm = coalesce(ifelse(Dm < 0.1, 0.1, Dm), Dm))

flora_rare <- flora_rare %>%
  mutate(Db = coalesce(ifelse(Db < 0.1, 0.1, Db), Db))


## BIOMASS AT INDIVIDUAL LEVEL ####

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

}

## BIOMASS AT SPECIES LEVEL ###########

#For biomass, we do not have information for samplings 0, 1, 2 and 12. 

flora_biomass_raw <- flora_medium %>% 
  filter(!sampling %in% c("0", "1", "2", "12")) %>%  # Remove samplings for which we have no morphological measurements
  filter(!is.na(x)) %>%                              # Remove rows where x is NA since we do not have morph. meas. for those datapoints
  filter(height > 5 & height < 200)               # Remove individuals with height < 5 cm or > 200 cm, because the equation does not properly work with them 

flora_medium %>% 
  filter(!(height > 5 & height < 200)) %>% 
  nrow() # Only 187 datapoints are lost

length(unique(flora_biomass_raw$code))

length(unique(flora_medium$code))

#There is no loss of species by taking out individuals with height > 5 cm 

## NUMBER OF INDIVIDUALS ####

# To be able to estimate abundance based on the number of individuals we need data were abundance and number of individuals per species,
# plot and sampling were available. Since we have been learning the application of this non-destructive methodology in the go,
# we have 2 different groups of data:

#1.  Since sampling 12, we have been estimating the number of individuals per species and plot based on direct field observations.

plots <- read.csv("data/plots.csv") %>% 
  select(nplot, treatment_code) %>% 
  rename(treatment = treatment_code,
         plot = nplot)

nind1 <- read.csv("data/n_individuals.csv") %>% 
  merge(plots) %>% 
  select(sampling, treatment,  plot, code, abundance, nind_m2) %>%
  group_by(sampling, treatment, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance = sum(abundance)) %>% 
  mutate(sampling = as.factor(sampling),
         treatment = as.factor(treatment), 
         plot = as.factor(plot), 
         code = as.factor(code), 
         nind_m2 = as.numeric(nind_m2), 
         abundance = as.numeric(abundance))

## Las especies de asteraceae que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

#2.  Up to sampling 11, We only measured the morphological parameters of 5 individuals per specie as maximum. 
#    If there were less than 5 species, we know that number was the total amount of individuals in the plot.

nind2 <- flora_raw %>%
  filter(!sampling %in% c( "12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, treatment, plot, code) %>%
  mutate(n_individuals = n()) %>%
  group_by(sampling, treatment, plot, code, abundance) %>%
  summarize(nind_m2 = mean(n_individuals, na.rm = T)) %>%
  filter(nind_m2 < 5) 

# Final database for number of individuals

nind <- bind_rows(nind1, nind2)

##| Plus*
##| There are some species for which we only have measured 1 individual per plot and sampling every time we have spot it.
##|  Therefore, for these species we do not need to estimate the number of individuals.
##|  In think is useful to keep a vector

one_ind_species <- c("rucr", "amsp", "kisp")


## IMPUTATION OF is.na(nind_m2)  ####


source("code/0.2.MICE.R")


imput_stability
imput_reliability_test

# Calculation of biomass at species level for both scenarios: with imputation of data with MICE and without it

{biomass_imp <- biomass_imp %>% 
  mutate(biomass_s = nind_m2_imputed * biomass_i)

hist(biomass_imp$biomass_s, breaks = 100)
hist(log(biomass_imp$biomass_s), breaks = 100, main = "log biomass_imp")

# Step 1: Calculate IQR for log-transformed biomass_s
Q1 <- quantile(log(biomass_imp$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_imp$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Step 2: Remove outliers
biomass_imp_clean<- biomass_imp %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)

hist(biomass_imp_clean$biomass_s, breaks = 100)
hist(log(biomass_imp_clean$biomass_s), breaks = 100, main = "Log biomass_imp without outliers")


biomass_noimp <- biomass %>% 
  mutate(biomass_s = nind_m2* biomass_i, na.rm =T)  # We go ahead with the 18% of NA's of nind_m2

hist(biomass_noimp$biomass_s, breaks = 100, main = "Log biomass_s without imputation")
hist(log(biomass_noimp$biomass_s), breaks = 100, main = "Log biomass_s without imputation")

# Step 1: Calculate IQR for log-transformed biomass_s
Q1 <- quantile(log(biomass_noimp$biomass_s), 0.25, na.rm = TRUE)
Q3 <- quantile(log(biomass_noimp$biomass_s), 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Step 2: Remove outliers
biomass_noimp_clean<- biomass_noimp %>%
  filter(log(biomass_s) >= Q1 - 1.5 * IQR & log(biomass_s) <= Q3 + 1.5 * IQR)

hist(biomass_noimp_clean$biomass_s, breaks = 100)
hist(log(biomass_noimp_clean$biomass_s), breaks = 100, main = "log biomass_noimp without outliers")
}
### 1. Calculating biomass with linear regression model: 
##
##source("code/0.1linear_regression.R")
##
### 2. Calculating biomass with no linear models 
##
##source("code/0.4.biomass_no_lm.R")


source("code/0.3.lm_biomass012.R")

print(results)

flora_medium012 <- left_join(flora_medium, lm_data_filtered) %>%
  filter(sampling %in% c("0", "1", "2")) %>% 
  mutate(year = year(date)) %>% 
  mutate(abundance = ifelse(abundance < 1, 1, abundance)) %>% 
  mutate(biomass_s = exp(intercept + slope * log(abundance)) * smearing_factor)

biomass_imp_clean_012 <- bind_rows(biomass_imp_clean, flora_medium012) 

biomass_noimp_clean_012 <- bind_rows(biomass_noimp_clean, flora_medium012)


#Calculation of total biomasss (community biomass) by summing all biomass at species level per square meter
# I will keep the flora_biomass not cleaned in order to compare results with and without outliers

biomass_imp <- biomass_imp_clean_012 %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup() 

biomass_noimp <- biomass_noimp_clean_012 %>% 
  group_by(plot, sampling, treatment) %>% 
  mutate(biomass_community =  sum(biomass_s, na.rm = TRUE)) %>%
  ungroup()


#Adding dummy rows for p and wp in sampling 1

{dummy_rows_p <- matrix(nrow = 4, ncol = 8)
  colnames(dummy_rows_p) <- c("sampling", "plot", "treatment", "richness","abundance", "abundance_community", 
                              "biomass_s", "biomass_community")
  dummy_rows_p <- as.data.frame(dummy_rows_p)
  
  dummy_rows_p[] <- 0
  dummy_rows_p[, 1] <- 1
  dummy_rows_p[, 2] <- c(3, 6, 10, 15)
  dummy_rows_p[, 3] <- "p"
  
  
  dummy_rows_wp <- matrix(nrow = 4, ncol = 8)
  colnames(dummy_rows_wp) <- c("sampling", "plot", "treatment", "richness","abundance", "abundance_community", 
                               "biomass_s", "biomass_community")
  dummy_rows_wp <- as.data.frame(dummy_rows_wp)
  
  dummy_rows_wp[] <- 0
  dummy_rows_wp[, 1] <- 1
  dummy_rows_wp[, 2] <- c(4, 5, 12, 13)
  dummy_rows_wp[, 3] <- "wp"
  

dummy_rows <- bind_rows(dummy_rows_p, dummy_rows_wp) %>% 
    mutate(sampling = as.factor(sampling),
           plot = as.factor(plot), 
           treatment = as.factor(treatment))

dummy_rows <- right_join(dummy_rows, sampling_dates, by = join_by(sampling)) %>% 
  filter(sampling == "1") }

  


biomass_imp <- bind_rows(biomass_imp, dummy_rows) %>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, biomass_s, biomass_community, abundance_community)

biomass_noimp <- bind_rows(biomass_noimp, dummy_rows) %>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, biomass_s, biomass_community, abundance_community)

flora_abrich <- bind_rows(flora_abrich, dummy_rows)%>% 
  select(year, date, sampling, treatment, plot, code, abundance,
         richness, abundance_community)




#rm(list = setdiff(ls(), c("flora_abrich", "flora_biomass_imp", "flora_biomass_noimp")))



