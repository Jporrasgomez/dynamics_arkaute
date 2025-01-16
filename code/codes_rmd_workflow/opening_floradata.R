# Opening and transforming data ####
flora_raw <- read.csv("data/flora_db.csv")

flora_raw <- flora_raw %>%
  mutate(across(where(is.character), as.factor))


# Histograms with ggplot
print(ggarrange(
  
  ggplot(flora_raw, aes(x = abundance)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Abundance") + 
    theme_minimal(),
  
  ggplot(flora_raw, aes(x = height)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Height") + 
    theme_minimal(),
  
  ggplot(flora_raw, aes(x = Cb)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Cb") + 
    coord_cartesian(xlim = c(0, 20)) +
    theme_minimal(),
  
  ggplot(flora_raw, aes(x = Cm)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Cm") + 
    coord_cartesian(xlim = c(0, 10)) +
    theme_minimal(),
  
  ggplot(flora_raw, aes(x = Db)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Db") + 
    theme_minimal(),
  
  ggplot(flora_raw, aes(x = Dm)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(x = "", y = "Frequency") +
    ggtitle("Dm") + 
    theme_minimal(),
  
  labels = c("A", "B", "C", "D", "E", "F"),
  ncol = 3, nrow = 2))



#Organizing samplings by chronological order (more levels will be added )
desired_order <- c("sampling 0", "sampling 1", "sampling 2", "sampling 3", "sampling 4", "sampling 5", "sampling 6", 
                   "sampling 7", "sampling 8", "sampling 9", "sampling 10")

flora_raw$sampling <- factor(flora_raw$sampling, levels = desired_order)

# Modifying database
#Adding 0.01 cm to diametres for the calibre mistake
flora_raw$Dm <- flora_raw$Dm + 0.01
flora_raw$Db <- flora_raw$Db + 0.01

#Divide abundance by 100 to get relative values
flora_raw$abundance <- round(flora_raw$abundance/100, 3)


#Adding date, might be useful afterwards
sampling_dates <- read.csv("data/sampling_dates.csv")
sampling_dates$datenew <-  ymd(sampling_dates$date)
sampling_dates$month <- month(sampling_dates$datenew)
sampling_dates$date <- NULL
sampling_dates$micro.sampling <- NULL
sampling_dates$N.micro <- NULL

sampling_dates <- sampling_dates %>%
  mutate(across(where(is.character), as.factor))

flora_raw <- right_join(flora_raw, sampling_dates, by = join_by(sampling))

flora <- flora_raw %>% select(sampling, plot, treatment, species, abundance, height, Cb, Db, Cm, Dm, datenew, month)


