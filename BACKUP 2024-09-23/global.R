library(shiny)
library(shinydashboard)
library(shinyjqui)
library(tidyverse)
library(DT)
library(data.table)
library(openxlsx)
library(scales)
library(ggrepel)
library(ggbeeswarm)
library(RColorBrewer)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# For Colors
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

## Function for the standard deviation of sample (n) not of population (n-1)
sd_n <- function(Vector) {
  sd(Vector, na.rm = T) * sqrt((length(na.exclude(Vector)) - 1) / length(na.exclude(Vector)))
}