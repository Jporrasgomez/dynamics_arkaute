


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
  


flora_medium$code_ordered <- reorder(flora_medium$code, -flora_medium$abundance, FUN = mean)

flora_nobs <- flora_medium %>% 
  group_by(code) %>% 
  summarize(n_observations= n(),
            mean_abundance = mean(abundance))

flora_nobs$abnobs <- flora_nobs$mean_abundance * flora_nobs$n_observations

flora_nobs$code_nobs <- factor(flora_nobs$code,
                               levels = flora_nobs$code[order(flora_nobs$n_observations, decreasing = TRUE)])

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
plots <- read.csv("data/plots.csv") %>% 
  select(nplot, treatment_code) %>% 
  rename(treatment = treatment_code,
         plot = nplot)

nind1 <- merge(nind1, plots)


#source("code/first_script_old.R")

nind1$code <- as.factor(nind1$code)

nind1 <- nind1 %>% 
  select(sampling, treatment,  plot, code, nind_m2, abundance)
  
#flora_nind <-  flora %>% select(code, family) %>% distinct(code, .keep_all = TRUE)

nind1 <- nind1 %>%
  group_by(sampling, treatment, plot, code) %>%
  summarize(nind_m2 = sum(nind_m2), abundance = sum(abundance))

{
nind1$sampling <- as.factor(nind1$sampling)
nind1$treatment <- as.factor(nind1$treatment)
nind1$plot <- as.factor(nind1$plot)
nind1$code <- as.factor(nind1$code)
nind1$nind_m2 <- as.numeric(nind1$nind_m2)
nind1$abundance <- as.numeric(nind1$abundance)}

## Las especies de asteraceae que hemos agrupado por familia se recalculan sus individuos y abundancias aquí

  #2.  Up to sampling 11, We only measured the morphological parameters of 5 individuals per specie as maximum. 
  #    If there were less than 5 species, we know that number was the total amount of individuals in the plot.

nind2 <- flora_raw %>%
  filter(!sampling %in% c("12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, treatment, plot, code) %>%
  mutate(n_individuals = n())

nind2 <- nind2 %>%
  group_by(sampling, plot, treatment, code, abundance) %>%
  summarize(n_individuals_mean = mean(n_individuals, na.rm = T))  #corrección de asteraceae y poaceae #corrección de asteraceae y poaceae

nind2 <- nind2 %>%
  filter(n_individuals_mean < 5)
names(nind2)[names(nind2) == "n_individuals_mean"] <- "nind_m2"

nind <- full_join(nind1, nind2)

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
    geom_point(aes(color = treatment), alpha = 0.5) +
    scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue3", "wp" = "purple")) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = paste("LM ", code_levels[i]),
         subtitle = paste("Equation: y =", round(intercept_i, 2), "+", round(slope_i, 2), "* x\n",
                          "R2:", round(r_squared_i, 2), ", p-value:", round(p_value_i, 4), "\n",
                          "n observations", n_observations_i),
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

ggarrange(gglist[[1]], gglist[[2]], gglist[[3]], 
          gglist[[4]], gglist[[5]], gglist[[6]], 
          ncol = 2, nrow = 3)

ggarrange(gglist[[7]], gglist[[8]], gglist[[9]], 
          gglist[[10]], gglist[[11]], gglist[[12]], 
          ncol = 2, nrow = 3)

ggarrange(gglist[[13]], gglist[[14]], gglist[[15]], 
          gglist[[16]], gglist[[17]], gglist[[18]], 
          ncol = 2, nrow = 3)

ggarrange(gglist[[19]], gglist[[20]], gglist[[21]], 
          gglist[[22]], gglist[[23]], gglist[[24]], 
          ncol = 2, nrow = 3)

ggarrange(gglist[[25]], gglist[[26]], gglist[[27]], 
          gglist[[28]], gglist[[29]], gglist[[30]], 
          ncol = 2, nrow = 3)

ggarrange(gglist[[31]], gglist[[32]], gglist[[33]], 
          gglist[[34]], gglist[[35]], gglist[[36]], 
          ncol = 2, nrow = 3)


gglist[[37]]


print(gglist[[12]])

ggplot(nind, aes(x = abundance, y = nind_m2)) + 
  facet_wrap(~ treatment) +
  geom_point(aes(color = treatment), shape = 15, alpha = 0.5) +
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
  scale_fill_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
  geom_smooth(method = "lm", se = FALSE, color = "black")


# As we can see, for this example the R$^2$ is quite low ( R$^2$ = 0.23). The fact is that, if we take a look to how well
# the model fit for all of our species, there are many considerations.

nind_lm_data$posneg_slope <- ifelse(nind_lm_data$slope < 0, paste("negative"), paste("positive"))
#hist(nind_lm_data$slope)

#Let's check results
results <- 
  ggplot(nind_lm_data, aes( x = r_squared, y = p_value, 
                            label = paste(code, n_observations, sep = ", "),
                            color = posneg_slope))+
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


#The lm model fits well (R^2 >= 0.3 and p-value < 0.1) for a few species 


# We can see all this process for the disaggregation of treatments and see if we
# could create a linear model per code and treatment, in order to be more accurate

source("code/0.1disaggregating_lm_bytreatment.R")
results_treatments
shared_codes 
#these are the shared species between all treatments that have presented a well fitted linear model
# Therefore, we stick to the general linear model for all species

# We take out species with negative slope first 
nind_lm_data <- nind_lm_data %>% 
  filter(posneg_slope %in% "positive")


# But,  !! Why do we choose R2 0.3?
# Lets do a sensitivity analysis

source("code/0.2sensitivity_analysis_lm.R")
#The plot A is nind_lm_species, B is nind_lm_species_1 and C is nind_lm_species_2
ggarrange(a,b,c,
          labels = c("A", "B", "C"),
          nrow = 1, 
          ncol = 3)


# Which set of species do we choose? 
# I say we choose p-value < 0.05. Is the least arbitrary and we do not lose a lot of information. 

flora_biomass_lm <- merge(flora_biomass_raw, nind_lm_species_2)


#calculation of number of indivuals per m2 with the regression data for each species
flora_biomass_lm$nind_m2 <- flora_biomass_lm$intercept + flora_biomass_lm$abundance * flora_biomass_lm$slope

# Since there are some species for which its intercept is negative, the lm consider that nind_m2 <0 when abundance = 0. And that
# cannot be the case. There are 3 species. We can either delete these species or make this "shitty" correction by sayin that if
# nind_m2 < 0 , then nind_m2 = 0
# Shitty correction ?????????????????????????????????????
flora_biomass_lm <- flora_biomass_lm %>%
  mutate(nind_m2 = ifelse(nind_m2 < 0, 1, nind_m2))


# Calculation of species biomass per square meter by multiplying biomass at individual (biomass_i) level by the 
# number of individuals per square meter (nind_m2)
flora_biomass_lm$biomass_s <- flora_biomass_lm$biomass_i * flora_biomass_lm$nind_m2


#Putting together lm species and one ind species
flora_biomass <- bind_rows(flora_biomass_oneind, flora_biomass_lm)

nabiomass <- which(is.na(flora_biomass$biomass_s))
print(flora_biomass[nabiomass, ]) 
# There are some NAs comming from sampling 3, where we did not measure morphological data from species with very low abundance (decisions of
# tired Javi and Laura :( ))

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



