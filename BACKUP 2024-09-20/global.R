library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(data.table)
library(openxlsx)
library(scales)
library(ggrepel)
library(ggbeeswarm)
library(RColorBrewer)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# function for reversing and Log Transform x/y-axis
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x)
    - log(x, base)
  inv <- function(x)
    base ^ (-x)
  trans_new(
    paste0("reverselog-", format(base)),
    trans,
    inv,
    log_breaks(base = base),
    domain = c(1e-100, Inf)
  )
}
# function for filtering data inside ggplot
pick <- function(condition) {
  function(d)
    d %>% filter_(condition)
}

# nicer scientific notation
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text = l)
}

# For Colors
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

