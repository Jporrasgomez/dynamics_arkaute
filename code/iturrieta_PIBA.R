


library(XML)l
library(dplyr)

# Load the XML file
xml_file.1 <- "data/c024/C024_2020_1.xml"
xml_file.2 <- "data/c024/C024_2020_2.xml"
xml_file.3 <- "data/c024/C024_2020_3.xml"
xml_file.4 <- "data/c024/C024_2020_4.xml"
xml_file.5 <- "data/c024/C024_2020_5.xml"
xml_file.6 <- "data/c024/C024_2020_6.xml"
xml_file.7 <- "data/c024/C024_2020_7.xml"
xml_file.8 <- "data/c024/C024_2020_8.xml"
xml_file.9 <- "data/c024/C024_2020_9.xml"
xml_file.10 <- "data/c024/C024_2020_10.xml"
xml_file.11 <- "data/c024/C024_2020_11.xml"
xml_file.12 <- "data/c024/C024_2020_12.xml"

# xml.1
xml_data.1 <- xmlParse(xml_file.1)
dia_nodes.1 <- getNodeSet(xml_data.1, "//dia")
records.1 <- data.frame()

for (dia in dia_nodes.1) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.1 <- rbind(records.1, record)
  }
}



# xml.2
xml_data.2 <- xmlParse(xml_file.2)
dia_nodes.2 <- getNodeSet(xml_data.2, "//dia")
records.2 <- data.frame()

for (dia in dia_nodes.2) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.2 <- rbind(records.2, record)
  }
}

# xml.3
xml_data.3 <- xmlParse(xml_file.3)
dia_nodes.3 <- getNodeSet(xml_data.3, "//dia")
records.3 <- data.frame()

for (dia in dia_nodes.3) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.3 <- rbind(records.3, record)
  }
}

# xml.4
xml_data.4 <- xmlParse(xml_file.4)
dia_nodes.4 <- getNodeSet(xml_data.4, "//dia")
records.4 <- data.frame()

for (dia in dia_nodes.4) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.4 <- rbind(records.4, record)
  }
}

# xml.5
xml_data.5 <- xmlParse(xml_file.5)
dia_nodes.5 <- getNodeSet(xml_data.5, "//dia")
records.5 <- data.frame()

for (dia in dia_nodes.5) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.5 <- rbind(records.5, record)
  }
}

# xml.6
xml_data.6 <- xmlParse(xml_file.6)
dia_nodes.6 <- getNodeSet(xml_data.6, "//dia")
records.6 <- data.frame()

for (dia in dia_nodes.6) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.6 <- rbind(records.6, record)
  }
}

# xml.7
xml_data.7 <- xmlParse(xml_file.7)
dia_nodes.7 <- getNodeSet(xml_data.7, "//dia")
records.7 <- data.frame()

for (dia in dia_nodes.7) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.7 <- rbind(records.7, record)
  }
}

# xml.8
xml_data.8 <- xmlParse(xml_file.8)
dia_nodes.8 <- getNodeSet(xml_data.8, "//dia")
records.8 <- data.frame()

for (dia in dia_nodes.8) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.8 <- rbind(records.8, record)
  }
}

# xml.9
xml_data.9 <- xmlParse(xml_file.9)
dia_nodes.9 <- getNodeSet(xml_data.9, "//dia")
records.9 <- data.frame()

for (dia in dia_nodes.9) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.9 <- rbind(records.9, record)
  }
}

# xml.10
xml_data.10 <- xmlParse(xml_file.10)
dia_nodes.10 <- getNodeSet(xml_data.10, "//dia")
records.10 <- data.frame()

for (dia in dia_nodes.10) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.10 <- rbind(records.10, record)
  }
}

# xml.11
xml_data.11 <- xmlParse(xml_file.11)
dia_nodes.11 <- getNodeSet(xml_data.11, "//dia")
records.11 <- data.frame()

for (dia in dia_nodes.11) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.11 <- rbind(records.11, record)
  }
}

# xml.12
xml_data.12 <- xmlParse(xml_file.12)
dia_nodes.12 <- getNodeSet(xml_data.12, "//dia")
records.12 <- data.frame()

for (dia in dia_nodes.12) {
  date <- xmlGetAttr(dia, "Dia")
  hora_nodes <- getNodeSet(dia, ".//hora")
  
  for (hora in hora_nodes) {
    time <- xmlGetAttr(hora, "Hora")
    meteo <- xmlToList(hora[["Meteoros"]])
    record <- c(Date = date, Time = time, unlist(meteo))
    records.12 <- rbind(records.12, record)
  }
}


list <- list(records.1, records.2, records.3, records.4, records.4, records.5,
             records.6, records.7, records.8, records.9, records.10, records.11, 
             records.12)
sapply(list, )

# Clean column names: Ensure valid R column names and replace placeholders



names(records.1) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.2) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.3) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.4) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.5) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")
records.5$Pressure_90cm <- NA

names(records.6) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")


names(records.7) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.8) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")


names(records.9) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.10) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                      "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                      "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                      "VelMax_950cm", "VelMed_950cm")

names(records.11) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                       "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                       "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                       "VelMax_950cm", "VelMed_950cm")

names(records.12) <- c("Date", "Time", "DirMed_950cm", "Humidity_170cm", 
                       "Precip_140cm", "Pressure_90cm", "RadUV_226cm", 
                       "SigDir_950cm", "SigVel_950cm", "TempAir_170cm", 
                       "VelMax_950cm", "VelMed_950cm")

records <- rbind(records.1, records.2, records.3, records.3, records.4, records.5,
                 records.6, records.7, records.8, records.9, records.10, records.11, 
                 records.12)


records$Humidity_170cm <- as.numeric(records$Humidity_170cm)
records$TempAir_170cm <- as.numeric(records$TempAir_170cm)
records$Precip_140cm <- as.numeric(records$Precip_140cm)
records$Pressure_90cm <- as.numeric(records$Pressure_90cm)
records$RadUV_226cm <- as.numeric(records$RadUV_226cm)
records$SigDir_950cm <- as.numeric(records$SigDir_950cm)
records$SigVel_950cm <- as.numeric(records$SigVel_950cm)
records$VelMax_950cm <- as.numeric(records$VelMax_950cm)
records$VelMed_950cm <- as.numeric(records$VelMed_950cm)

# Perform the grouping and calculate the statistics using `summarise()`
records_day <- records %>% 
  group_by(Date) %>% 
  summarise(
    humidity_mean = mean(Humidity_170cm, na.rm = TRUE),
    humidity_sd = sd(Humidity_170cm, na.rm = TRUE),
    temp_mean = mean(TempAir_170cm, na.rm = TRUE),
    temp_sd = sd(TempAir_170cm, na.rm = TRUE),
    temp_max = max(TempAir_170cm, na.rm = TRUE),
    temp_min = min(TempAir_170cm, na.rm = TRUE),
    precip_mean = mean(Precip_140cm, na.rm = TRUE),
    precip_sd = sd(Precip_140cm, na.rm = TRUE),
    precip_max = max(Precip_140cm, na.rm = TRUE),
    precip_min = min(Precip_140cm, na.rm = TRUE)
  )

records %>% write.csv("results/iturrieta_10min.csv")
records_day %>% write.csv("results/iturrieta_day.csv")

# Check the structure of the resulting data
str(records_day)
  
# Guardar el archivo combinado como CSV
write.csv(all_data, "C024_combined.csv", row.names = FALSE)