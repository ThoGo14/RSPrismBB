# ----------------------------------------------------------------------------------------------------- #
# Definiere eine Liste der benötigten Pakete
pakete <- c("shiny",
            "shinydashboard",
            "shinyjqui",
            "colourpicker",
            "tidyverse",
            "DT",
            "data.table",
            "openxlsx",
            "scales",
            "ggrepel",
            "ggbeeswarm",
            "RColorBrewer")

# Überprüfe, ob Pakete installiert sind, und installiere sie, falls nicht
installiere_fehlende_pakete <- function(pakete) {
  fehlende_pakete <- pakete[!(pakete %in% installed.packages()[, "Package"])]
  
  if (length(fehlende_pakete) > 0) {
    install.packages(fehlende_pakete)
  }
}

# Rufe die Funktion auf
installiere_fehlende_pakete(pakete)

# load all packages
lapply(pakete, require, character.only = TRUE)

# ----------------------------------------------------------------------------------------------------- #

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# For Colors
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Function for the standard deviation of sample (n) not of population (n-1)
sd_n <- function(Vector) {
  sd(Vector, na.rm = T) * sqrt((length(na.exclude(Vector)) - 1) / length(na.exclude(Vector)))
}