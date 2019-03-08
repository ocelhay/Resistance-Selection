# Shiny web application to showcase the mechanism of resistance selection =====

library(deSolve)
library(ggforce)
library(ggplot2)
library(patchwork)
library(scales)
library(shinyWidgets)
library(tidyverse)
library(shiny)

# enable bookmarking
enableBookmarking(store = "url")

# load the models
source("www/models.R", local = TRUE)

# Parameters for the model
init_param <- list(time_resistant = 0,
                   nb_resistant = 100000,
                   growth_s = 8,
                   growth_r = 6,
                   dose = 500,
                   ka = 100,
                   Fa = 0.8,
                   V = 6,
                   CL = 0.4,
                   k1 = -1,
                   n = 1,
                   EC50_s = 10,
                   EC50_r = 50,
                   second_inf = FALSE,
                   t_secondary = 120)

# Times
# times <- c(seq(0, 1.99, by = 0.01), 2:240)

# Colors
cols <- c('Sensitive Parasites' = '#377eb8', 
          'Resistant Parasites' = '#e41a1c')