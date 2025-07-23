

# To do's



## PROBAR CON BIOMASS 012!!

# multigroups function 

##  data(meadows)
##  
##  jutila <- psem(
##    lm(rich ~ elev + mass, data = meadows),
##    lm(mass ~ elev, data = meadows)
##  )
##  
##  jutila.multigroup <- multigroup(jutila, group = "grazed")
##  
##  jutila.multigroup
##  

## Probar varias cosas: 
##| biomass vs biomass012
##| Utilizar solo functional traits (PCA) y no composición ( - NMDS)
##| Abundancia directa sobre PCA
##| 


##Criterios de seleccion de variables en los modelos: 
##| Relación conceptual
##| Diferencias significativas en los LRR con el control
##| Diferencias en LRR

## MIrar en el summary del modelo: 
##| Individual R-squared
##| Modelos individuales: te salen las filas por cada variable respuesta
##| Diferencia entre (Conditional value - marginal value) =
##| variación en la variable respuesta debida a los factores random (en nuestro caso: plot)
##| Si la diferencia es muy distinta a 0, más diferencias se les atribuye al factor random
##| Conditional: 

rm(list = ls(all.names = TRUE))




library(piecewiseSEM)
library(nlme)
library(lme4)
library(tidyverse)
library(corrplot)


arkaute_norm <- read.csv("data/arkaute_norm.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))


arkaute <- read.csv("data/arkaute.csv") %>% 
  mutate(
    year = as.factor(year),
    date = ymd(date),
    omw_date = as.factor(omw_date),
    one_month_window = as.factor(one_month_window),
    sampling = as.factor(sampling),
    plot = as.factor(plot),
    treatment = as.factor(treatment))


arkaute_long <- arkaute_norm %>% 
  pivot_longer(
    cols = richness:PC2,          
    names_to = "variable",      
    values_to = "value"          
  )


variables <- unique(arkaute_long$variable)
explanatory_var <- c("year", "date", "omw_date", "one_month_window", "sampling", "plot", "treatment")


data <- arkaute_norm


data_c <- data %>% 
  filter(treatment == "c") %>% 
  as.data.frame() %>% 
  mutate(treat_label = "Control") %>% 
  na.omit()

data_p <- data %>% 
  filter(treatment == "p")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Perturbed") %>% 
  na.omit()

data_w <- data %>% 
  filter(treatment == "w")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Warming") %>% 
  na.omit()

data_wp <- data %>% 
  filter(treatment == "wp")%>% 
  as.data.frame() %>% 
  mutate(treat_label = "Global Change") %>% 
  na.omit()

data_list <- list(data_c, data_w, data_p, data_wp)
                    # 1     # 2     # 3      # 4 



# First data exploration: correlations, multi-collinearities



{i = 1
  ##| 1: Control
  ##| 2: Warming
  ##| 3: Perturbation
  ##| 4: Warming + perturbation

pairs(data_list[[i]][,variables])

cor(data_list[[i]][,variables], use="pairwise.complete.obs")

corrplot(cor(data_list[[i]][,variables], use="pairwise.complete.obs"), method = "number")}



model7_plot_list <- list()
model7_summ_list <- list()
  
for (i in 1:4){
  
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + Y_zipf,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
  model7_summ_list[[i]] <- print(summary(global_model))
  
  model7_plot_list[[i]] <- plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
  svg_code <- export_svg(model7_list[[i]])
  rsvg_png(charToRaw(svg_code), file = paste0("results/Plots/SEM/model7_plot", i, ".png"), 
           width = 1200,
           height = 1200)
  
  summary_text <- capture.output(summary(global_model))
  writeLines(summary_text, con = paste0("results/Plots/SEM/model7_summary", i, ".txt"))

}

{i = 1
print(model7_summ_list[[i]])
model7_plot_list[[i]]}



#Regla de la *D* =  número de interacciones(hipótesis) * 5 < número de observaciones. 
# EFECTOS INDIRECTOS: MULTIPICAR COEFICIENTES DE INTERACCIONES. 

# Model1: NMDS1, PC1 and abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1 + abundance, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


# Model2: NMDS2, PC2 and abundance

{
mod1 = lme(biomass ~ richness + Y_zipf + PC2 + abundance, random = ~ 1 | plot,  data_list[[i]])
mod2 = lme(PC2 ~ richness + NMDS2, random = ~ 1 | plot, data_list[[i]])
mod3 = lme(NMDS2 ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
mod4 = lme(Y_zipf ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])

global_model <- psem(
  mod1,
  mod2,
  mod3,
  mod4
)

summary(global_model)

plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model3: NMDS1, PC2 and  abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model4: NMDS1, PC1 and no abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness + NMDS1, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS1 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


#Model5: NMDS2, PC2 and no abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + NMDS2, random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(NMDS2 ~ richness, random = ~ 1 | plot, data_list[[i]])
  mod4 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3,
    mod4
  )
  
  summary(global_model)
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
}


# Model6: NO NMDS1 and abundance
{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
 
  print(summary(global_model))
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
}





# Model7: NO NMDS1 and YES abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2 + abundance, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~ richness + abundance,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness + abundance, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
  
  print(summary(global_model))
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
}





# Model8: No abundance and YES PC2

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC2, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC2 ~  Y_zipf ,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
  
  print(summary(global_model))
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
}


# Model9: No abundance and YES PC1

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness + Y_zipf,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
  
  print(summary(global_model))
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
}










# Probar quitar NMDS1 y añadir abundance

{
  mod1 = lme(biomass ~ richness + Y_zipf + PC1, random = ~ 1 | plot,  data_list[[i]])
  mod2 = lme(PC1 ~ richness,  random = ~ 1 | plot, data_list[[i]])
  mod3 = lme(Y_zipf ~ richness, random = ~ 1 | plot, data_list[[i]])
  
  global_model <- psem(
    mod1,
    mod2,
    mod3
  )
  
  
  plot(global_model, title = paste0(unique(data_list[[i]]$treat_label)))
  
  print(summary(global_model))
}




# Probar quitar NMDS + abundance


