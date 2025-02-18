



#We calculate the mean area of the individual by taking Ah and Ab both and transform it into m2 (cm2/10000)
biomass_nolm <- flora_biomass_raw

biomass_nolm$mean_area_i <- ((biomass_nolm$Ah + biomass_nolm$Ab)/2)/10000

# We then calculate the number of individuals that are within the plot by dividing the total abundance of the species
# by the mean area occupied by an individual

# There is a problem here. 
biomass_nolm$nind_s <- (biomass_nolm$abundance/100) / biomass_nolm$mean_area_i

par(mfrow = c(1,1))

hist(biomass_nolm$nind_s, breaks = 100)
View(subset(biomass_nolm, nind_s >1000000))

#The number of individuals is STUPIDLY HIGH
ggplot(biomass_nolm, 
       aes(y = nind_s, x = abundance, 
           color = treatment, 
           label = paste(code, sampling, plot, sep = ", "))) + 
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
  geom_point()+
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) 

ggplot(biomass_nolm, 
       aes(y = nind_s, x = abundance, 
           color = treatment, 
           label = paste(code, sampling, plot, sep = ", "))) + 
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
  geom_point()+
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) 

View(subset(biomass_nolm, plot == 16))
View(subset(flora_raw, plot == 16 & code == "poaceae"))


ggplot(biomass_nolm, aes(x = mean_area_i, y = nind_s)) +
  geom_point()

ggplot(biomass_nolm, aes(x = abundance, y = nind_s)) +
  geom_point()





biomass_nolm$biomass_s <- biomass_nolm$nind_s * biomass_nolm$biomass_i


ggplot(biomass_nolm, 
       aes(y = biomass_s, x = date, 
           color = treatment, 
           label = paste(code, sampling, plot, sep = ", "))) + 
  scale_colour_manual(values = c("c" = "green2", "w" = "red", "p" = "blue", "wp" = "purple")) +
  geom_point()+
  geom_text_repel(
    size = 3,                # Text size
    min.segment.length = 0.1,  # Ensures lines are always drawn
    segment.color = "gray50",  # Line color
    segment.size = 0.5         # Line thickness
  ) 
