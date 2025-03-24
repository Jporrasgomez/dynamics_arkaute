


source("code/1.first_script.R")
source("code/4.species_composition_NMDSbray.R")
rm(list = setdiff(ls(), c("ab_rich_treatmeans", "biomass_treatmeans", "biomass_treatmeans012", "nmds_df_treatmeans")))
                          
source("code/palettes_labels.R")                          
palette <- palette5
labels <- labels_RR



source("code/meta_function/effect_size_treatment_c.R")
source("code/meta_function/effect_size_treatment_wp.R")



list_c <- list()
gglist_c <- list()
list_wp <- list()
gglist_wp <- list()

variables <- c("richness", "abundance", "Y_zipf", "mu_log", "sigma_log")
for (i in 1:5){
  effect_size_treatment_c(ab_rich_treatmeans, variables[i])
  list_c[[i]] <- RR_treatment %>%
    select(treatment, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_c[[i]] <- gg_RR
  
  effect_size_treatment_wp(ab_rich_treatmeans, variables[i])
  list_wp[[i]] <- RR_treatment_wp %>%
    select(treatment, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_wp[[i]] <- gg_RR_wp
  
}


RR_c <- reduce(list_c, bind_rows)
RR_wp <- reduce(list_wp, bind_rows)



{effect_size_treatment_c(biomass_treatmeans012, "biomass")
  RR_biomass_c <- RR_treatment %>%
    select(treatment, n,  variable, starts_with("RR"), starts_with("se_RR"))
  print(gg_RR)
}

{effect_size_treatment_wp(biomass_treatmeans012, "biomass")
  RR_biomass_wp <- RR_treatment_wp %>%
    select(treatment, n,  variable, starts_with("RR"), starts_with("se_RR"))
  print(gg_RR_wp)
}


RR_c <- bind_rows(RR_c, RR_biomass_c) 
#  RR_c <- RR_c %>% 
# filter(! variable %in% c("mu_log", "sigma_log"))


RR_wp <- bind_rows(RR_wp, RR_biomass_wp) 
#  RR_wp <- RR_wp %>% 
# filter(! variable %in% c("mu_log", "sigma_log"))


list_NMDS_c <- list()
list_NMDS_wp <- list()
gglist_NMDS_c <- list()
gglist_NMDS_wp <- list()
NMDS <- c("NMDS1", "NMDS2", "NMDS3")
for (i in 1:3){
  effect_size_treatment_c(nmds_df_treatmeans, NMDS[i])
  list_NMDS_c[[i]] <- RR_treatment %>%
    select(treatment, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_NMDS_c[[i]] <- gg_RR
  
  effect_size_treatment_wp(nmds_df_treatmeans, NMDS[i])
  list_NMDS_wp[[i]] <- RR_treatment_wp %>%
    select(treatment, n, variable, starts_with("RR"), starts_with("se_RR"))
  gglist_NMDS_wp[[i]] <- gg_RR_wp
  
}

gglist_NMDS_c[[1]]
gglist_NMDS_c[[2]]
gglist_NMDS_c[[3]]

gglist_NMDS_wp[[1]]
gglist_NMDS_wp[[2]]
gglist_NMDS_wp[[3]]

RR_NMDS_c <- reduce(list_NMDS_c, bind_rows)
RR_NMDS_wp <- reduce(list_NMDS_wp, bind_rows)

RR_c <- bind_rows(RR_c, RR_NMDS_c)
RR_wp <- bind_rows(RR_wp, RR_NMDS_wp)



ggarrange(

ggplot(RR_c, aes(x = variable, y = RR, color = treatment)) + 
  geom_errorbar(aes(ymin = RR - se_RR,
                    ymax = RR + se_RR,
                    color =  treatment), 
                linewidth = 0.5,
                position = position_dodge(width = 0.2),
                width = 0.1) +  
  geom_point(aes(color = treatment), position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette) +
  scale_x_discrete(
    limits = c("richness",
               "abundance",
               "Y_zipf",
               "mu_log",
               "sigma_log",
               "biomass",
               "NMDS1",
               "NMDS2",
               "NMDS3"),  
    labels = function(x) str_to_title(x)  
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR") +
  theme(legend.position = "none"),


ggplot(RR_wp, aes(x = variable, y = RR, color = treatment)) + 
  geom_errorbar(aes(ymin = RR - se_RR,
                    ymax = RR + se_RR,
                    color =  treatment), 
                linewidth = 0.5,
                position = position_dodge(width = 0.2),
                width = 0.1) +  
  geom_point(aes(color = treatment), position = position_dodge(width = 0.2)) + 
  scale_color_manual(values = palette_wp) +
  scale_x_discrete(
    limits = c("richness",
               "abundance",
               "Y_zipf",
               "mu_log",
               "sigma_log",
               "biomass",
               "NMDS1",
               "NMDS2",
               "NMDS3"),  
    labels = function(x) str_to_title(x)  
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = NULL, y = "RR") +
  theme(legend.position = "none"),

ncol = 1, nrow = 2)



