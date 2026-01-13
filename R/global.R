# ----------------------------------------------------------------------------------------------------- #
# Definiere eine Liste der benötigten Pakete
pakete <- c(
  "jsonlite",
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
  "sortable"
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

try(require(svglite), silent = TRUE)

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

# ----------------------------------------------------------------------------------------------------- #
# Initialize translator for multilingual support
# ----------------------------------------------------------------------------------------------------- #
# Load translations from JSON file
translations_json <- fromJSON("../inst/translations.json")

# Create a simple translator object
translator <- list(
  translations = translations_json,
  current_language = "de",
  set_translation_language = function(lang) {
    if (lang %in% names(translations_json)) {
      translator$current_language <<- lang
    }
  },
  tl = function(key) {
    translations_json[[translator$current_language]][[key]] %||% key
  }
)

# ----------------------------------------------------------------------------------------------------- #
