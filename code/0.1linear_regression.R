
# to do's: 
##| 1) Hacer diferencia entre predictive values and real values. Cuando haya datos de números de individuos
##| tomamos ese valor y cuando no lo haya, se pone el valor extraído de la 





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

flora_biomass_oneind$nind_m2 <- 1


#flora_biomass_raw <- flora_biomass_raw %>% 
#  filter(!code %in% one_ind_species)
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
  select(sampling, treatment,  plot, code, abundance, nind_m2)

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
  filter(!sampling %in% c( "12", "13", "14", "15", "16", "17", "18", "19", "20"))  %>%
  group_by(sampling, treatment, plot, code) %>%
  mutate(n_individuals = n())

nind2 <- nind2 %>%
  group_by(sampling, treatment, plot, code, abundance) %>%
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
nind0 <- nind

nind <- nind %>% 
  filter(!code %in% one_ind_species)


code_levels <- unique(nind$code)
gglist <- list()

nind_lm_data <- matrix(nrow = length(code_levels), ncol = 7)
colnames(nind_lm_data) <- c("code", "intercept", "slope", "r_squared", "p_value", "n_points_lm", "shapiro_pvalue")
nind_lm_data <- as.data.frame(nind_lm_data)

counter <- 0

library(broom)

for (i in 1: length(code_levels)) {
  
  nind_i <- subset(nind, code == code_levels[i])
  lm_i <- lm(nind_m2 ~ sqrt(abundance), data = nind_i)
  
  shapiro_i <- shapiro.test(residuals(lm_i))
  
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
  nind_lm_data$n_points_lm[counter] <- n_observations_i
  nind_lm_data$shapiro_pvalue[counter] <- shapiro_i$p.value
  
  
}


#ggarrange(gglist[[1]], gglist[[2]], gglist[[3]], 
#          gglist[[4]], gglist[[5]], gglist[[6]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[7]], gglist[[8]], gglist[[9]], 
#          gglist[[10]], gglist[[11]], gglist[[12]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[13]], gglist[[14]], gglist[[15]], 
#          gglist[[16]], gglist[[17]], gglist[[18]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[19]], gglist[[20]], gglist[[21]], 
#          gglist[[22]], gglist[[23]], gglist[[24]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[25]], gglist[[26]], gglist[[27]], 
#          gglist[[28]], gglist[[29]], gglist[[30]], 
#          ncol = 2, nrow = 3)
#
#ggarrange(gglist[[31]], gglist[[32]], gglist[[33]], 
#          gglist[[34]], gglist[[35]], gglist[[36]], 
#          ncol = 2, nrow = 3)
#
#
#gglist[[37]]


#print(gglist[[12]])



# As we can see, for this example the R$^2$ is quite low ( R$^2$ = 0.23). The fact is that, if we take a look to how well
# the model fit for all of our species, there are many considerations.

#ggplot(nind, aes(x = abundance, y = nind_m2)) + 
#  facet_wrap(~ treatment) +
#  geom_point(aes(color = treatment), shape = 15, alpha = 0.5) +
#  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
#  scale_fill_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
#  geom_smooth(method = "lm", se = FALSE, color = "black")

hist(nind_lm_data$shapiro_pvalue, breaks = 30)

#Shapiro test indicates there is no normality of residuals in most part  of our regressions. So we cannot use
# a linar model. :) :') :'''''''''''''''''''(

nind_lm_data$posneg_slope <- ifelse(nind_lm_data$slope < 0, paste("negative"), paste("positive"))
#hist(nind_lm_data$slope)

#Let's check results
results <- 
  ggplot(nind_lm_data, aes( x = r_squared, y = p_value, 
                            label = paste(code, n_points_lm, sep = ", "),
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

#source("code/0.1disaggregating_lm_bytreatment.R")
#results_treatments
#shared_codes 
#these are the shared species between all treatments that have presented a well fitted linear model
# Therefore, we stick to the general linear model for all species

# We take out species with negative slope first 

neg_slope_species <- nind_lm_data$code[which(nind_lm_data$posneg_slope == "negative")]

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


source("code/0.3species_presence.R")
ggpresence
ggarrange(
  ggpresence0, ggpresence1, ggpresence2, 
  labels = c("A", "B", "C"), 
  nrow = 1, 
  ncol = 3
)


# Plus exploration by Dani

nind_2 <- nind %>% 
  filter(!code %in% excluded_species_lm_2)

species_0_obs <- flora_nobs_presence %>% 
  filter(n_obs_xtreat == 0)

presence_2 <- flora_nobs_presence %>% 
  # Filtro para las especies con p-value < 0.05
  filter(!code %in% excluded_species_lm_2) %>% 
  # Aseguro quedarme con las especies presentes en todos los tratamientos
  filter(!code %in% unique(flora_nobs_presence$code[which(flora_nobs_presence$n_obs_xtreat == 0)])) 
nind_2 <- merge(nind_2, presence_2)

code_levels_2 <- unique(nind_2$code)

source("code/plots_functions_flora/lm_function.R")

#lm_function(nind_2, code_levels_2[1])
#lm_function(nind_2, code_levels_2[2])
#lm_function(nind_2, code_levels_2[3])
#lm_function(nind_2, code_levels_2[4])
#lm_function(nind_2, code_levels_2[5])
#lm_function(nind_2, code_levels_2[6])
#lm_function(nind_2, code_levels_2[7])
#lm_function(nind_2, code_levels_2[8])
#lm_function(nind_2, code_levels_2[9])
#lm_function(nind_2, code_levels_2[10])
#lm_function(nind_2, code_levels_2[11])
#lm_function(nind_2, code_levels_2[12])
#lm_function(nind_2, code_levels_2[13])
#lm_function(nind_2, code_levels_2[14])
#lm_function(nind_2, code_levels_2[15])
#lm_function(nind_2, code_levels_2[16])
#lm_function(nind_2, code_levels_2[17])
##lm_function(nind_2, code_levels_2[18])
#lm_function(nind_2, code_levels_2[19])
#lm_function(nind_2, code_levels_2[20])
#lm_function(nind_2, code_levels_2[21])
##lm_function(nind_2, code_levels_2[22])



#las especies para las que no funciona la función son especies sin representación en algo


# Which set of species do we choose? 

# I say we choose p-value < 0.05. Is the least arbitrary and we do not lose a lot of information. 


flora_biomass_nind <- full_join(flora_biomass_raw, nind0) %>% 
  filter(!sampling %in% c("0", "1", "2"))

#Only 489 points have to be filled
length(flora_biomass_nind$nind_m2[which(is.na(flora_biomass_nind$nind_m2))])

# This is a 18% of the biomass data
(length(flora_biomass_nind$nind_m2[which(is.na(flora_biomass_nind$nind_m2))])/length(flora_biomass_nind$nind_m2))*100


flora_biomass_lm <- merge(flora_biomass_nind, nind_lm_species_2)

# Now I will fill the gaps of nind_m2 and let the ones that we already have to be present
#calculation of number of indivuals per m2 with the regression data for each species


flora_biomass_lm <- flora_biomass_lm %>%
  mutate(nind_m2 = coalesce(nind_m2, intercept + abundance * slope))




# Calculation of species biomass per square meter by multiplying biomass at individual (biomass_i) level by the 
# number of individuals per square meter (nind_m2)
flora_biomass_lm$biomass_s <- flora_biomass_lm$biomass_i * flora_biomass_lm$nind_m2


#Putting together lm species and one ind species
flora_biomass_lm <- bind_rows(flora_biomass_oneind, flora_biomass_lm)
