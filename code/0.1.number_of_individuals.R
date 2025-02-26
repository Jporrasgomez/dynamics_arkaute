



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
