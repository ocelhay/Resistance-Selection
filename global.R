
# load packages
library(deSolve)
library(ggforce)
library(ggplot2)
library(patchwork)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(shiny)

# enable bookmarking
enableBookmarking(store = "url")

# load the models
source("www/models.R", local = TRUE)

# list of initial parameters
source("www/init_parameters.R", local = TRUE)



# Colors
cols <- c('Sensitive Parasites' = '#377eb8', 
          'Resistant Parasites' = '#e41a1c')