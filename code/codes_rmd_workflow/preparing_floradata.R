

#Adding number of measured individuals for biomass of each species per plot and sampling
flora <- flora %>%
  group_by(plot, sampling, species) %>%
  mutate(n_individuals = n()) %>%
  ungroup()

#Biomass equation                 
#Transforming diameters into circumferences
flora$cm <- round(ifelse(!is.na(flora$Dm), flora$Dm * pi, flora$Cm), 2)
flora$cb <- round(ifelse(!is.na(flora$Db), flora$Db * pi, flora$Cb), 2)
flora$Ah <- ((flora$cm)^2)/4*pi
flora$Ab <- ((flora$cb)^2)/4*pi

#Application of equation proposed by Perronne, Jabot and Pottier 2020 with the d and z values
d <- 1.96
z <- 2/3
flora$biomass <- d*(((flora$height/2)*(flora$Ab + flora$Ah))^z)


#Grouping by smapling, plot and treatment. Estimating biomass by multplying mean biomass per plot and sampling by their abundance
flora <- flora %>%
  group_by(sampling, datenew, month, treatment, plot, abundance, species) %>%
  reframe(biomass = mean(biomass, na.rm = TRUE) * abundance) %>%
  distinct(sampling, datenew, month, plot, treatment, abundance, species, biomass)

#Transforming NA into 0s for the graphs
flora$biomass <- ifelse(flora$biomass == 0, NA, flora$biomass)

#New databases

flora <- flora %>%
  group_by(plot, sampling) %>%
  mutate(n_species = n()) %>%
  ungroup()

flora_samplings <-  flora %>%
  group_by(sampling, datenew, month, treatment, plot) %>%
  reframe(biomass = sum(biomass, na.rm = T), 
          n_species = n_species, 
          abundance = sum(abundance, na.rm = T)) %>%
  distinct(sampling, datenew, month, plot, treatment, biomass, n_species, abundance)

flora_treatments <-  flora_samplings %>%
  group_by(treatment) %>%
  reframe(biomass = mean(biomass, na.rm = T), 
          n_species = mean(n_species, na.rm = T), 
          abundance = mean(abundance, na.rm = T)) %>%
  distinct(treatment, biomass, n_species, abundance) 

