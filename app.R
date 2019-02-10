#
# Shiny web application to showcase the mechanism of resistance selection
# =============================================================================

library(deSolve)
library(ggplot2)
library(patchwork)
library(tidyverse)

library(shiny)

# load the models
source("www/models.R", local = TRUE)

# Parameters for the model
init_param <- list(growth = 8/48,
                   dose = 500,
                   ka = 100,
                   Fa = 0.8,
                   V = 6,
                   CL = 0.4,
                   k1 = -1,
                   n = 1,
                   EC50s = 10,
                   EC50r = 50,
                   second_inf = FALSE,
                   t_secondary = 100)

# Initial conditions of the ODE
state <- c(S = 10^12, R = 10^5)

# time
times = c(seq(0, 1.99, by = 0.01), 2:150)



# Define UI for the application
# =============================================================================
ui <- fluidPage(
  includeCSS("./www/styles.css"),
  
  # Application title
  titlePanel("Resistance Selection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4(icon("arrow-alt-circle-right"), "Antimalarial drug Pharmacokinetic characteristics"),
      
      fluidRow(
        column(width = 6,
               span("Absorption parameter (per hour):", sliderInput("ka", NULL, min = 0, max = 10, value = init_param$ka))
        ),
        column(width = 6,
               span("Fraction absorbed — bioavailability:", sliderInput("Fa", NULL, min = 0, max = 1, value = init_param$Fa))
        )
      ),
      fluidRow(
        column(width = 6,
               span("Volume of drug in blood (liters):", sliderInput("V", NULL, min = 4, max = 20, value = init_param$V))
        ),
        column(width = 6,
               span("Clearance rate (liters  per hour):", sliderInput("CL", NULL, min = 0.1, max = 1, value = init_param$Cl))
        )
      ),
      p("Half-life of the drug (hours):"),
      fluidRow(
        column(width = 6,
               withMathJax(em("$$t_{1/2} = \\frac{0.693.V}{CL}$$"))
        ),
        column(width = 6,
               htmlOutput("half_life")
        )
      ),
      
      
      h4(icon("arrow-alt-circle-right"), "Parasite multiplication rates"),
      sliderInput("growth", NULL, min = 0, max = 1, value = init_param$growth),
      
      h4(icon("arrow-alt-circle-right"), "Resistance attributes"),
      fluidRow(
        column(width = 6,
               span("first order rate constant for maximum parasite killing: "), numericInput("k1", NULL, min = -10, max = 10, value = init_param$k1, width = "80px")
        ),
        column(width = 6,
               span("slope of the linear portion of the concentration-effect curve: "), numericInput("n", NULL, min = -10, max = 10, value = init_param$n, width = "80px")
        )
      ),
      fluidRow(
        column(width = 6,
               span("EC50 for ", div(class = "sensitive", "sensitive parasites"), sliderInput("EC50s", NULL, min = 0, max = 100, value = init_param$EC50s))
        ),
        column(width = 6,
               span("EC50 for ", div(class = "resistant", "resistant parasites"), sliderInput("EC50r", NULL, min = 0, max = 100, value = init_param$EC50r))
        )
      ),
      h4(icon("arrow-alt-circle-right"), "Secondary infection"),
      checkboxInput(inputId = "second_inf",label = "Secondary infection (newly acquired infections exposed to residual antimalarial drug concentrations)", value = FALSE),
      conditionalPanel("input.second_inf", 
                       sliderInput("t_secondary", NULL, min = 10, max = 100, value = init_param$t_secondary)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4(icon("tablets"), "Amount of antimalarial drug administered. (mg)"),
      sliderInput("dose", label = NULL, min = 100, max = 1000, value = init_param$dose),
      
      p("selection"),
      
      plotOutput("combined_graphs")
    )
  )
)







# Define server logic required to draw a histogram
# =============================================================================
server <- function(input, output) {
  
  # update default values based on inputs
  parameters = reactiveValues(growth = init_param$growth,
                              dose = init_param$dose,
                              ka = init_param$ka,
                              Fa = init_param$Fa,
                              V = init_param$V,
                              CL = init_param$CL,
                              k1 = init_param$k1,
                              n = init_param$n,
                              EC50s = init_param$EC50s,
                              EC50r = init_param$EC50r,
                              second_inf = init_param$second_inf,
                              t_secondary = init_param$t_secondary)
  
  observe({
    parameters$growth = input$growth
    parameters$dose = input$dose
    parameters$ka = input$ka
    parameters$Fa = input$Fa
    parameters$V = input$V
    parameters$CL = input$CL
    parameters$k1 = input$k1
    parameters$n = input$n
    parameters$EC50s = input$EC50s
    parameters$EC50r = input$EC50r
    parameters$second_inf = input$second_inf
    parameters$t_secondary = input$t_secondary
  })
  
  # Output 1: half-life
  output$half_life <- renderText(paste0("<br>", " = ", round(0.693 * parameters$V / parameters$CL, 2)))
  
  # Output 2: combined graphs
  output$combined_graphs <- renderPlot(height = 650, {
    
    # Drug concentration
    # ========================================================================
    
    Ct <- drug_concentration(dose = parameters$dose, ka = parameters$ka, 
                             Fa = parameters$Fa, V = parameters$V, CL = parameters$CL, 
                             t = times)
    
    print(times)
    dim(Ct)
    
    # we assume that (less than 10,000 the max concentration <=> 0)
    # Ct[which(Ct < max(Ct)/10000)] <- 0
    # print(Ct)
    
    
    graph_concentration <- data.frame(t = times, Ct = Ct) %>%
      ggplot(aes(x = t, y = Ct)) +
      geom_line(size = 1, colour = "grey") +
      # geom_vline(xintercept = log(parameters$ka * parameters$V /parameters$CL)/(parameters$ka - parameters$CL/parameters$V), 
      #            col = 'grey', lty = 2) + # max concentration
      labs(x = "Time (hours)", y = "Drug concentration") +
      theme_classic(base_size = 13)
    
    
    
    # Dose response
    # ========================================================================
    
    graph_dose_response <- 
      data.frame(t = times, Ct = Ct,
                 fC_S = dose_response(k1 = parameters$k1, Ct = Ct, 
                                      EC50 = parameters$EC50s, n = parameters$n),
                 fC_R = dose_response(k1 = parameters$k1, Ct = Ct, 
                                      EC50 = parameters$EC50r, n = parameters$n)) %>%
      ggplot(aes(x = times)) +
      geom_line(aes(y = fC_S), col = "blue") +
      geom_line(aes(y = fC_R), col = "red") +
      labs(x = "Time (hours)", y = "Parasite killing rate") +
      theme_classic(base_size = 13)
    
    
    # Run the model and normalize the output
    # ========================================================================
    toto <- list(growth = parameters$growth,
                 dose = parameters$dose,
                 ka = parameters$ka,
                 Fa = parameters$Fa,
                 V = parameters$V,
                 CL = parameters$CL,
                 k1 = parameters$k1,
                 n = parameters$n,
                 EC50s = parameters$EC50s,
                 EC50r = parameters$EC50r,
                 second_inf = parameters$second_inf,
                 t_secondary = parameters$t_secondary)
    
    out <- ode(y = state, times = times, func = resistance_window, parms = toto, method = "adams")
    out <- as.data.frame(out)
    
    # Limit the range of the y-axis (and make the growth somehow limited)
    # out$S <- ifelse(out$S <= 1, 1, out$S) # (log10(1) = 0)
    # out$R <- ifelse(out$R <= 1, 1, out$R)
    
    print(out$time)
    print(out$S)
    
    graph_parasites <- ggplot(data = out, aes(x = time)) +
      geom_line(aes(y = log10(S)), col = 'blue') +
      geom_line(aes(y = log10(R)), col = 'red') +
      # geom_vline(xintercept = parameters$t_secondary, col = 'orange', lty = 4) + 
      # geom_text(x = parameters$t_secondary, y = 15, label = "Second. Infection") + 
      scale_y_continuous(limits = c(0, 13)) +
      labs(x = "Time (hours)", y = "Parasites (Log scale)") +
      theme_minimal(base_size = 13)
    
    
    
    # Output
    # ========================================================================
    graph_concentration + graph_dose_response + graph_parasites + 
      plot_layout(ncol = 1, height = c(2, 2, 4))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)