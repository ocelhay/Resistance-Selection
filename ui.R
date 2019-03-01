fluidPage(
  includeCSS("./www/styles.css"),
  
  # KaTeX
  tags$head(
    tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
         <script>
         document.addEventListener("DOMContentLoaded", function(){
         renderMathInElement(document.body, {
         delimiters: [{left: "$", right: "$", display: false}]
         });
         })
         </script>')
    ),
  
  # Add window title
  titlePanel(title = NULL, windowTitle = "Resistance Selection"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4(icon("arrow-right"), "Antimalarial Drug Pharmacokinetic Characteristics"),

      fluidRow(
        column(width = 6,
               span("Absorption parameter", sliderInput("ka", NULL, min = 0, max = 10, value = init_param$ka))
        ),
        column(width = 6,
               span("Bioavailability", sliderInput("Fa", NULL, min = 0, max = 1, value = init_param$Fa))
        )
      ),
      fluidRow(
        column(width = 6,
               span("Volume of drug in blood", sliderInput("V", NULL, min = 4, max = 20, value = init_param$V))
        ),
        column(width = 6,
               span("Clearance rate", sliderInput("CL", NULL, min = 0.1, max = 1, value = init_param$Cl))
        )
      ),
      
      
      
      
      
      dropdownButton(label = "About these parameters", circle = FALSE, status = "primary", icon = icon("info"), width = "500px", tooltip = FALSE, right = FALSE,
                     
                     fluidRow(
                       column(width = 12,

                              HTML("The concentration of the drug is: <br><br>
$C(t) = \\frac{dose.k_{a}.F_{a}}{V.k_{a} - CL} \\Big[ \\exp{(-\\frac{CL}{V}.t)}-\\exp{(-k_{a}.t)} \\Big]$"),
                              HTML('with:'),
                              HTML("<ul><li>$k_{a}$: Absortion parameter (per hour)</li>
<li>$F_{a}$: Bioavailability (fraction)</li>
                                   <li>$V$: Volume of drug in blood (liters)</li>
                                   <li>$CL$: Clearance rate (liters per hour)</li></ul>"),
                              br(),
                              
                              span("The drug half life is given by the formula:
                                $t_{1/2} = \\frac{0.693.V}{CL}$"),
                              
                              htmlOutput("half_life")
                       ))
                     ),

      
      hr(),
      h4(icon("arrow-right"), "Parasites Attributes"),
      fluidRow(
        column(width = 6,
               span(div(class = "sensitive", 'Multiplication rate for sensitive parasites: (per 48 hours cycle)'), sliderInput("growth_s", NULL, min = 0, max = 10, value = init_param$growth_s))
        ),
        column(width = 6,
               span(div(class = "resistant", 'Multiplication rate for resistant parasites: (per 48 hours cycle)'), sliderInput("growth_r", NULL, min = 0, max = 10, value = init_param$growth_r))
        )
      ),
      fluidRow(
        column(width = 12,
               htmlOutput('message_rate')
        )
      ),
      
      fluidRow(
        column(width = 6,
               span(div(class = "sensitive", "EC50 for sensitive parasites:"), sliderInput("EC50_s", NULL, min = 0, max = 100, value = init_param$EC50_s))
        ),
        column(width = 6,
               span(div(class = "resistant", "EC50 for resistant parasites:"), sliderInput("EC50_r", NULL, min = 0, max = 100, value = init_param$EC50_r))
        )
      ),
      fluidRow(
        column(width = 12,
               htmlOutput('message_EC50')
        )
      ),
      fluidRow(
        column(width = 12,
               span("First order rate constant for maximum parasite killing: "), numericInput("k1", NULL, min = -10, max = 10, value = init_param$k1, width = "80px"),
               span("Slope of the linear portion of the concentration-effect curve: "), numericInput("n", NULL, min = -10, max = 10, value = init_param$n, width = "80px")
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
                      sliderInput("dose", label = NULL, min = 0, max = 1000, value = init_param$dose)
      ),
      column(width = 6,
             br(),
             htmlOutput('text_selection')
      )
      ),
      fluidRow(column(width = 5,
                      plotOutput("window_selection", height = '120px')
      )),
      br(), hr(),
      plotOutput("combined_graphs")
      
    )
  )
)