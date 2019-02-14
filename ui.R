fluidPage(
  includeCSS("./www/styles.css"),
  
  # Add window title
  titlePanel(title = NULL, windowTitle = "Resistance Selection"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4(icon("arrow-right"), "Antimalarial Drug Pharmacokinetic Characteristics"),
      
      fluidRow(
        column(width = 6,
               span("Absorption parameter (per hour):", sliderInput("ka", NULL, min = 0, max = 10, value = init_param$ka))
        ),
        column(width = 6,
               span("Bioavailability", br(), "(fraction):", sliderInput("Fa", NULL, min = 0, max = 1, value = init_param$Fa))
        )
      ),
      fluidRow(
        column(width = 6,
               span("Volume of drug in blood (liters):", sliderInput("V", NULL, min = 4, max = 20, value = init_param$V))
        ),
        column(width = 6,
               span("Clearance rate", br(), "(liters  per hour):", sliderInput("CL", NULL, min = 0.1, max = 1, value = init_param$Cl))
        )
      ),
      
      htmlOutput("half_life"),
      # withMathJax(em("$$t_{1/2} = \\frac{0.693.V}{CL}$$"))
      
      hr(),
      h4(icon("arrow-right"), "Parasites Attributes"),
      fluidRow(
        column(width = 12,
               span('Parasites multiplication rate:',
                    sliderInput("growth", NULL, min = 0, max = 1, value = init_param$growth, width = "50%")
               )
        )
      ),
      fluidRow(
        column(width = 12,
               span("First order rate constant for maximum parasite killing: "), numericInput("k1", NULL, min = -10, max = 10, value = init_param$k1, width = "80px"),
               span("Slope of the linear portion of the concentration-effect curve: "), numericInput("n", NULL, min = -10, max = 10, value = init_param$n, width = "80px")
        )
      ),
      fluidRow(
        column(width = 6,
               span(div(class = "sensitive", "EC50 for sensitive parasites:"), sliderInput("EC50s", NULL, min = 0, max = 100, value = init_param$EC50s))
        ),
        column(width = 6,
               span(div(class = "resistant", "EC50 for resistant parasites:"), sliderInput("EC50r", NULL, min = 0, max = 100, value = init_param$EC50r))
        )
      ),
      hr(),
      h4(icon("arrow-right"), "Secondary Infection"),
      checkboxInput(inputId = "second_inf",label = "Simulate secondary infection", value = FALSE),
      conditionalPanel("input.second_inf", 
                       span("Time of secondary infection:",
                            sliderInput("t_secondary", NULL, min = 10, max = 100, round = TRUE, value = init_param$t_secondary, width = '50%')
                       )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4(icon("tablets"), "Amount of Antimalarial Drug Administered (mg)"),
      fluidRow(column(width = 6,
                      sliderInput("dose", label = NULL, min = 100, max = 1000, value = init_param$dose)
      ),
      column(width = 6,
             br(),
             htmlOutput('text_selection')
      )
      ),
      
      plotOutput("window_selection", height = '100px'),
      plotOutput("combined_graphs")
      
    )
  )
)