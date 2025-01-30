# ----------------------------------------------------------------------------------------------------- #
# Definiere eine Liste der benötigten Pakete
pakete <- c(
  "shiny",
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
  "RColorBrewer"
)

# Überprüfe, ob Pakete installiert sind, und installiere sie, falls nicht
installiere_fehlende_pakete <- function(pakete) {
  fehlende_pakete <- pakete[!(pakete %in% installed.packages()[, "Package"])]
  
  if (length(fehlende_pakete) > 0) {
    install.packages(fehlende_pakete)
  }
}

# Max Dateilimit ist nun 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

# Rufe die Funktion auf
installiere_fehlende_pakete(pakete)

# load all packages
lapply(pakete, require, character.only = TRUE)

# ----------------------------------------------------------------------------------------------------- #

# setwd("Q:/IM/AGPathobiochemie/Z_RScripts_for_use/Barplot und Boxplot/Shiny")

# For Colors
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Function for the standard deviation of sample (n) not of population (n-1)
sd_n <- function(Vector) {
  sd(Vector, na.rm = T) * sqrt((length(na.exclude(Vector)) - 1) / length(na.exclude(Vector)))
}

print(sessionInfo())

# Lese die changelog.txt Datei ein
changelog_path <- "changelog.txt"  # Falls die Datei auf einem Shared Drive liegt, Pfad anpassen

if (file.exists(changelog_path)) {
  changelog_content <- readLines(changelog_path, warn = FALSE)
  
  # Nehme die erste Zeile (erste Version)
  aktuelle_version <- changelog_content[1]
  
  # Entferne das Datum für eine saubere Anzeige (optional)
  aktuelle_version <- sub(" - .*", "", aktuelle_version)  # Entfernt alles nach " - "
  aktuelle_version <- sub("<b>", "", aktuelle_version)
} else {
  aktuelle_version <- "Version unbekannt"
}
