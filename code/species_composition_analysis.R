# to do's

# Elegimos Bray-curtis porque funciona mejor con los "dobles 0s". Esto es, cuando una especie no está presente 
# entre tratamientos
# Al gráfico de turnover, añadire un ratio encima appearance/disappearance


library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggforce)

#theme_set(theme_bw()+ theme(legend.position = "NULL"))


source("code/first_script.R")



#Calcular, para cada especie, su abundancia media por tratamiento y muestreo 
species_ab <-  summarise(group_by(flora, date, month, code, sampling, treatment, family,  genus_level, species_level),
                              abundance = (sum(abundance, na.rm = T)/4)) #mean abundance by treatment and sampling in a square meter (mean abundance of 4 plots) (?)
totals_df <- summarise(group_by(species_ab, sampling, treatment), #adding number of species per treatment and sampling to species_ab
                          n_species = n_distinct(code),
                          total_abundance = sum(abundance))
species_ab <- merge(species_ab, totals_df)
  


# Hacer otro día los RADs con ID debajo #####

# TURNOVER ##### package "codyn"

# T = (E + C)/R, where T is the percentage turnover rate, E is the number of taxa that went extinct between two time points,
# C is the number of taxa that colonised the community between the same two time points, and R is the total number of species
# (i.e. the richness of the pool of species conformed by the two samples). 

species_ab$sampling <- as.numeric(species_ab$sampling) # fuction codyn::turnover needs time variable to be numeric
species_ab$sampling <- species_ab$sampling - 1 # when transforming to numeric, it transform factor into their position. And sampling 0 goes into position 1

sp_total_turnover <- species_ab %>% 
  codyn::turnover(time.var = "sampling",
                  species.var = "code",
                  abundance.var = "abundance",
                  metric = "total",# calculates summed appearances and disappearances relative to total species richness across both time periods.
                  replicate.var = "treatment")

sp_appear <- species_ab %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "appearance", # Calculates the number of species that appeared in the second 
           # time period relative to total species richness across both time periods
           replicate.var = "treatment")

sp_disappear <- species_ab %>% 
  turnover(time.var = "sampling",
           species.var = "code",
           abundance.var = "abundance",
           metric = "disappearance", # Calculates the number of species that disappeared in the second
           # time period relative to total species richness across both time periods.
           replicate.var = "treatment")


sp_turnover <- left_join(sp_appear, sp_disappear)
sp_turnover <- pivot_longer(sp_turnover, cols = -c(sampling, treatment) , names_to = "metric", values_to = "rate")

ggturnover <- 
  ggplot(sp_turnover, aes(x = sampling, y = rate)) +
  facet_grid(~ treatment) +
  geom_col(aes(fill = metric)) +
  geom_point(data = sp_total_turnover, aes(y = total)) +
  geom_line(data = sp_total_turnover, aes(y = total)) +
  scale_fill_viridis_d(begin = 0.5) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) + 
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sampling",
       x = "Sampling",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+
  theme_bw()




#ABUNDANCE-BASED ANALYSIS. Package "vegan" #####

treats <- unique(flora$treatment)
list1 <- list()
gglist1 <- list()
count = 0


for(i in 1: length(treats)){
  
  count = count + 1
  
  list1[[count]] <- subset(species_ab, treatment == treats[i])
  
  
  sp_wide <- list1[[count]] %>%
    pivot_wider(id_cols = sampling,
                names_from = code,
                values_from = abundance,
                values_fill = list(abundance = 0)) %>% 
    column_to_rownames(var = "sampling") %>% 
    arrange(as.numeric(rownames(.)))
  
  pcoa_hell <- sp_wide %>% 
    na.omit() %>% 
    vegan::vegdist(method = "hellinger") %>%  # we use Hellinger because it works better with double 0's
    cmdscale(eig = T) 
  var_exp_hell <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])
  
  pcoa_samplings_hell<- pcoa_hell$points %>% 
    as.data.frame() 
  pcoa_species_hell<- cor(sp_wide, pcoa_samplings_hell) %>% 
    as.data.frame()
  
  
  gglist1[[count]] <-
    ggplot() +
    #geom_segment(data = pcoa_species_hell_c %>% 
    # rownames_to_column(var = "sp"),
    #aes(x = 0, y = 0, xend = V1, yend = V2),
    #color = "grey",
    #arrow = arrow()) +
    geom_text_repel(data = pcoa_species_hell %>% 
                      rownames_to_column(var = "sp"),
                    aes(x = V1, y = V2, label = sp),
                    color = "grey",
                    max.overlaps = 30) +
    geom_point(data = pcoa_samplings_hell %>% 
                 rownames_to_column(var = "sampling"),
               aes(x = V1, y = V2),
               size = 2) +
    geom_text_repel(data = pcoa_samplings_hell %>% 
                      rownames_to_column(var = "sampling"),
                    aes(x = V1, y = V2, label = sampling),
                    max.overlaps = 13) +
    geom_path(data = pcoa_samplings_hell %>% 
                rownames_to_column(var = "sampling"),
              aes(x = V1, y = V2)) +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed") +
    geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
    labs(title = paste("PCoA using Hellinger distance:", treats[i], sep = " "),
         subtitle = paste0("Variance explained = ", round(sum(var_exp_hell)*100), "%"),
         x = paste0(round(var_exp_hell[1]*100), "% var"),
         y = paste0(round(var_exp_hell[2]*100), "% var"))
  
}


ggpcoa_hell <-
ggarrange(
  gglist1[[2]], gglist1[[3]], gglist1[[1]], gglist1[[4]],
  ncol = 2, nrow = 2)






# ALL TREATMENTS IN 1 PLOT ##########

sp_wide <- species_ab %>%
  pivot_wider(id_cols = c(sampling, treatment),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

# create a distance matrix using Hellinger distances
abundance_data <- sp_wide %>% select(-treatment, -sampling)
distance_matrix <- abundance_data %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") 
pcoa_result <- cmdscale(distance_matrix)

pcoa_hell <- cmdscale(distance_matrix, eig = T)
var_exp_hell <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])

# plot the PCoA using ggplot


pcoa_df <- data.frame(
  PC1 = pcoa_result[, 1],
  PC2 = pcoa_result[, 2],
  treatment = sp_wide$treatment, #Cómo sabe R donde meter los niveles?
  sampling = sp_wide$sampling #Cómo sabe R donde meter los niveles?
)
pcoa_df <- pcoa_df %>% arrange(sampling)


ggpcoa_hell_alltreatments<- 
  
ggplot(pcoa_df, aes(x = PC1, y = PC2, color = treatment)) +
  geom_point(size = 2) +
  geom_text_repel(aes(x = PC1, y = PC2, label = sampling), max.overlaps = 100) +
  geom_path()+ #no  funciona
  geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
  labs(title = "PCoA using Hellinger distance",
       subtitle = paste0("Variance explained = ", round(sum(var_exp_hell)*100), "%"),
       x = paste0(round(var_exp_hell[1]*100), "% var"),
       y = paste0(round(var_exp_hell[2]*100), "% var"))+
  theme(legend.position = "bottom")







#4 treatments but without sampling differentiation. CLoud of dots. 4 dots per sampling x 12 samplings = 48 dots per cloud. 

sp_wide_treat <- flora %>%
  pivot_wider(id_cols = c(plot, treatment, sampling),
              names_from = code,
              values_from = abundance,
              values_fill = list(abundance = 0))

# create a distance matrix using Hellinger distances
abundance_data_treat <- sp_wide_treat %>% select(-treatment, -sampling, -plot)
distance_matrix_treat <- abundance_data_treat %>% 
  na.omit() %>% 
  vegan::vegdist(method = "hellinger") 
pcoa_result_treat <- cmdscale(distance_matrix_treat)

pcoa_hell_treat <- cmdscale(distance_matrix_treat, eig = T)
var_exp_hell_treat <- pcoa_hell_treat$eig[1:2]/sum(pcoa_hell_treat$eig[pcoa_hell_treat$eig > 0])


pcoa_df_treat <- data.frame(
  V1 = pcoa_result_treat[, 1],
  V2 = pcoa_result_treat[, 2],
  treatment = sp_wide_treat$treatment #Cómo sabe R donde meter los niveles?
)


ggpcoa_clouds <- 
    ggplot(pcoa_df_treat, aes(x = V1, y = V2, color = treatment, fill = treatment)) +
    geom_point(size = 2) +
    stat_ellipse(geom = "polygon", aes(fill = treatment),
                 alpha = 0.2,
                 show.legend = FALSE,
                 level = 0.6) + # % de datos que abarcan las elipses
    geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
    scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
    scale_fill_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
    labs(title = "PCoA, Hellinger",
         subtitle = paste0("Variance explained = ", round(sum(var_exp_hell_treat)*100), "%"),
         x = paste0(round(var_exp_hell_treat[1]*100), "% var"),
         y = paste0(round(var_exp_hell_treat[2]*100), "% var")) 







#A PCoA per SAMPLING

samps <- sort(unique(flora$sampling))
list2 <- list()
gglist2 <- list()
count = 0

for (i in 1:length(samps)){
  
  count = count + 1
  
  list2[[count]] <-  subset(flora, sampling == samps[i]) %>%
    pivot_wider(id_cols = c(plot, treatment, sampling),
                names_from = code,
                values_from = abundance,
                values_fill = list(abundance = 0))
  
  
  abundance_data <- list2[[count]] %>% select(-treatment, -plot, -sampling)
  distance_matrix <- abundance_data %>% 
    na.omit() %>% 
    vegan::vegdist(method = "hellinger") 
  pcoa_result <- cmdscale(distance_matrix)
  
  pcoa_hell <- cmdscale(distance_matrix, eig = T)
  pcoa_plots_hell <- pcoa_hell$points %>% 
    as.data.frame() 
  pcoa_species_hell<- cor(abundance_data, pcoa_plots_hell) %>% 
    as.data.frame()
  
  var_exp_hell <- pcoa_hell$eig[1:2]/sum(pcoa_hell$eig[pcoa_hell$eig > 0])
  
  pcoa_df <- data.frame(
    V1 = pcoa_result[, 1],
    V2 = pcoa_result[, 2],
    treatment = list2[[count]]$treatment #Cómo sabe R donde meter los niveles?
  )
  
  
  gglist2[[count]] <- 
  ggplot(pcoa_df, aes(x = V1, y = V2, color = treatment, fill = treatment)) +
    geom_point(size = 2) +
    #geom_polygon(alpha = 0.3) + #crossing edges!
    stat_ellipse(geom = "polygon", aes(fill = treatment),
                 alpha = 0.2,
                 show.legend = FALSE,
                 level = 0.95) +
    geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
    scale_color_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
    scale_fill_manual(values = c("c" = "darkolivegreen4", "p" = "#1C86EE", "w" = "red3", "wp" = "purple"))+
    labs(title = paste("Sampling", list2[[count]]$sampling, sep = " "),
         x = paste0(round(var_exp_hell[1]*100), "% var"),
         y = paste0(round(var_exp_hell[2]*100), "% var")) 
  
  
  
}




cloudspersampling<- ggarrange(
  gglist2[[1]], gglist2[[2]], gglist2[[3]], gglist2[[4]], 
  gglist2[[5]], gglist2[[6]], gglist2[[7]], gglist2[[8]], 
  gglist2[[9]], gglist2[[10]], gglist2[[11]], gglist2[[12]],
  gglist2[[13]], gglist2[[14]], gglist2[[15]], gglist2[[16]],
  gglist2[[17]], gglist2[[18]], gglist2[[19]], gglist2[[20]],
  gglist2[[21]],
  nrow = 6, ncol = 4)


## trying with one element of the list: 





#Plots : 

#ggturnover
#ggpcoa_hell
#ggpcoa_hell_alltreatments
#ggpcoa_clouds 
#ggpcoa_cloudspersampling


#Removing all elements from the environment but the ggplots

rm(list = setdiff(ls(), grep("gg", ls(), value = TRUE)))

















