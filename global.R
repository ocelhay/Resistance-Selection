# Shiny web application to showcase the mechanism of resistance selection =====

library(deSolve)
library(ggplot2)
library(patchwork)
library(shinyWidgets)
library(tidyverse)

library(shiny)

# load the models
source("www/models.R", local = TRUE)

# Parameters for the model
init_param <- list(growth_s = 8,
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
                   t_secondary = 100)

# Initial conditions of the ODE
state <- c(S = 10^12, R = 10^5)

# Times
times <- c(seq(0, 1.99, by = 0.01), 2:180)