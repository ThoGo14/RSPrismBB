# ----------------------------------------------------------------------------------------------------- #
# Definiere eine Liste der benötigten Pakete
pakete <- c(
  "shiny",
  "shinydashboard",
  "shinyjqui",
  "shinyWidgets",
  "shinyjs",
  "colourpicker",
  "tidyverse",
  "DT",
  "data.table",
  "openxlsx",
  "scales",
  "ggrepel",
  "ggbeeswarm",
  "RColorBrewer",
  "sortable",
  "svglite"
  )

# Überprüfe, ob Pakete installiert sind, und installiere sie, falls nicht
installiere_fehlende_pakete <- function(pakete) {
  fehlende_pakete <- pakete[!(pakete %in% installed.packages()[, "Package"])]
  
  if (length(fehlende_pakete) > 0) {
    install.packages(fehlende_pakete)
  }
}

# Max Dateilimit ist nun 100MB
MAX_UPLOAD_SIZE_MB <- 100
options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)

# Rufe die Funktion auf
if (interactive()) {
  installiere_fehlende_pakete(pakete)
}

# load all packages
paket_laden <- function(pakete) {
  for (pkg in pakete) {
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("Paket konnte nicht geladen werden:", pkg))
    }
  }
}

paket_laden(pakete)

# ----------------------------------------------------------------------------------------------------- #

# setwd("Q:/IM/AGPathobiochemie/Z_RScripts_for_use/Barplot und Boxplot/Shiny")

# For Colors
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Function for the standard deviation of sample (n) not of population (n-1)
sd_n <- function(x) {
  x <- na.exclude(x)
  if (length(x) < 2) return(NA)
  sd(x) * sqrt((length(x) - 1) / length(x))
}

print(sessionInfo())

# Lese die changelog.txt Datei ein
changelog_path <- "changelog.txt"  # Falls die Datei auf einem Shared Drive liegt, Pfad anpassen

# Lese die changelog.txt Datei ein
changelog_path <- "changelog.txt"  # Falls die Datei auf einem Shared Drive liegt, Pfad anpassen
aktuelle_version <- "Version unbekannt"

if (file.exists(changelog_path)) {
  changelog_content <- readLines(changelog_path, warn = FALSE)
  
  # Nehme die erste Zeile (erste Version)
  aktuelle_version <- changelog_content[1]
  
  # Entferne das Datum für eine saubere Anzeige (optional)
  aktuelle_version <- sub(" - .*", "", aktuelle_version)  # Entfernt alles nach " - "
  aktuelle_version <- sub("<b>", "", aktuelle_version)
}
