#' Utility functions for RSPrismBB app
#'
#' These are utility functions used across the application.
#'
#' @importFrom stats na.exclude sd
#' @importFrom utils installed.packages
#' @name utils_helpers
NULL

#' Standard Deviation of Sample (n) not Population (n-1)
#'
#' @param x Numeric vector
#'
#' @return Standard deviation of the sample
#' @noRd
sd_n <- function(x) {
    x <- na.exclude(x)
    if (length(x) < 2) {
        return(NA)
    }
    sd(x) * sqrt((length(x) - 1) / length(x))
}

#' Get Color Palette
#'
#' @return A color palette function
#' @noRd
get_color_palette <- function() {
    RColorBrewer::brewer.pal(9, "Set1") |>
        grDevices::colorRampPalette()
}

#' Initialize Translator for Multilingual Support
#'
#' @return Translator environment object
#' @noRd
initialize_translator <- function() {
    # Load translations from JSON file
    trans_file <- system.file("translations.json", package = "RSPrismBB")

    # Fallback if package not installed yet 
    if (trans_file == "" || !file.exists(trans_file)) {
        trans_file <- "inst/translations.json"
    }

    if (!file.exists(trans_file)) {
        stop("translations.json not found!")
    }

    translations_json <- jsonlite::fromJSON(trans_file)

    # Create an environment
    translator <- new.env(parent = emptyenv())
    translator$translations <- translations_json
    translator$current_language <- "de"
    
    translator$set_translation_language <- function(lang) {
        if (lang %in% names(translations_json)) {
            translator$current_language <<- lang
        }
    }
    
    translator$tl <- function(key) {
        translations_json[[translator$current_language]][[key]] %||% key
    }

    return(translator)
}

#' Set Shiny Options
#'
#' Sets global options for the Shiny app
#'
#' @noRd
set_app_options <- function() {
    # Get max upload size from config or use default
    max_size_mb <- tryCatch(
        get_golem_config("max_upload_size_mb", default = 100),
        error = function(e) 100
    )
    options(shiny.maxRequestSize = max_size_mb * 1024^2)
}

#' Check and Install Missing Packages
#'
#' @param pakete Character vector of package names
#'
#' @noRd
installiere_fehlende_pakete <- function(pakete) {
    fehlende_pakete <- pakete[!(pakete %in% installed.packages()[, "Package"])]

    if (length(fehlende_pakete) > 0) {
        utils::install.packages(fehlende_pakete)
    }
}

#' Load Required Packages
#'
#' @param pakete Character vector of package names
#'
#' @noRd
paket_laden <- function(pakete) {
    for (pkg in pakete) {
        if (!require(pkg, character.only = TRUE)) {
            stop(paste("Paket konnte nicht geladen werden:", pkg))
        }
    }
}
