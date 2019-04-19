
# load packages
library(deSolve)
library(ggforce)
library(ggplot2)
library(pracma)
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
# source("www/init_parameters.R", local = TRUE)



# Colors
cols <- c('Sensitive Parasites' = '#377eb8', 
          'Resistant Parasites' = '#e41a1c')


# Modifictaion of inputs to have the labels on the left
sliderInputSplit <- function (label, l = 5, class = 'sl',...) {
  fluidRow(
    column(l, div(class = class, label)),
    column((12 - l), sliderInput(label = NULL,...))
  )
}

numericInputSplit <- function (label, l = 5, class = 'sl', ...) {
  fluidRow(
    column(l, div(class = class, label)),
    column((12 - l), numericInput(label = NULL,...))
  )
}

selectInputSplit <- function (label, l = 5, class = 'sl', ...) {
  fluidRow(
    column(l, div(class = class, label)),
    column((12 - l), selectInput(label = NULL,...))
  )
}