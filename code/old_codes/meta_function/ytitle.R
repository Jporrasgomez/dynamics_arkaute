


# Definir un diccionario de variables y títulos
ytitle_dict <- list(
  
  "richness" = "Richness",
  "abundance" = "Community cover",
  "sigma_log" = "Sigma (Coefficient 2 in Log model for RADs)",
  "mu_log" = "Mu (Coefficient 1 in Log model for RADs)",
  "Y_zipf" = "Gamma (Coefficient in Zipf model for RADs)",
  "biomass" = "Community biomass",
  "NMDS1" = "NMDS1",
  "NMDS2" = "NMDS2",
  "NMDS3" = "NMDS3",
  "total_turnover" = "Total turnover", 
  "appearance" = "Turnover: appearance", 
  "disappearance" = "Turnover: disappearance",
  "SLA" = "SLA (?)", 
  "LA" = "LA(?)", 
  "LDMC" = "LDMC(?)", 
  "leafN"= "Leaf nitrogen(?)",
  "seed.mass" = "Seed mass",
  "vegetation.height" = "Vegetation height"
)

ytitle <- ytitle_dict[[variable]]

if (is.null(ytitle)) {
  stop("Variable must be one of the following: ", paste(names(ytitle_dict), collapse = ", "))
}


ytitle <<- ytitle