



source("code/first_script.R")

library(dplyr)


library(dplyr)


#loop: 

samps <- unique(flora$sampling)
plots <- unique(flora$plot)

list <- list()
count = 0
for (i in 1:length(samps)){
  for(j in 1: length(plots)){
    
    count = count + 1
    plot_sampling_data <- flora %>%
      filter(plot == plots[j] & sampling == samps[i])
    list[[count]] <- summarise(group_by(plot_sampling_data, code),
                                    unique_abundance = n_distinct(abundance))
    list[[count]]$sampling <- samps[i]
    list[[count]]$plot <- plots[j]
  
  }
}


corrections <- do.call(rbind, list)

corrections <- filter(corrections, unique_abundance >1)




corrections %>% write.csv("data/corrections.csv")










