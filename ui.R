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
      
      h4(icon("tablets"), "Amount of Drug Administered (mg)"),
      fluidRow(column(width = 12,
                      sliderInput("dose", label = NULL, min = 0, max = 1000, value = init_param$dose)
      )),
      
      h4(icon("arrow-right"), "Drug Pharmacokinetic Characteristics"),
      
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
               span("Clearance rate", sliderInput("CL", NULL, min = 0.1, max = 1, value = init_param$CL))
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
               span(div(class = "resistant", 'Time of introduction of resistants (hours since start treatment)'), sliderInput("time_resistant", NULL, min = 0, max = 192, step = 12, value = init_param$t_resistant))
        ),
        column(width = 6,
               span(div(class = "resistant", 'Number of resistants parasites'), selectInput('nb_resistant', NULL, choices = c(1, 10, 100, 1000, 10000, 100000), selected = 100000, width = '50%'))
        )
      ),
      fluidRow(
        column(width = 6,
               span(div(class = "resistant", "EC50 for resistant"), sliderInput("EC50_r", NULL, min = 0, max = 100, value = init_param$EC50_r))
               
        ),
        column(width = 6,
               span(div(class = "resistant", 'Multiplication rate for resistant (per 48h cycle)'), sliderInput("growth_r", NULL, min = 0, max = 10, value = init_param$growth_r))
        )
      ),
      fluidRow(
        column(width = 6,
               span(div(class = "sensitive", "EC50 for sensitive"), sliderInput("EC50_s", NULL, min = 0, max = 100, value = init_param$EC50_s))
        ),
        column(width = 6,
               span(div(class = "sensitive", 'Multiplication rate for sensitive (per 48h cycle)'), sliderInput("growth_s", NULL, min = 0, max = 10, value = init_param$growth_s))
        )
      ),
      fluidRow(
        column(width = 12,
               htmlOutput('message_EC50'),
               htmlOutput('message_rate')
        )
      ),
      
      dropdownButton(label = "Additional parameters", circle = FALSE, status = "success", icon = icon("cog"), width = "250px", tooltip = FALSE, right = FALSE,
                     
                     fluidRow(
                       column(width = 12,
                              span("First order rate constant for maximum parasite killing: "), numericInput("k1", NULL, min = -10, max = -1, value = init_param$k1, width = "80px"),
                              span("Slope of the concentration-effect curve: "), numericInput("n", NULL, min = -10, max = 10, value = init_param$n, width = "80px")
                       )
                     )
      ),
      hr(),
      h4(icon("arrow-right"), "Secondary Infection"),
      checkboxInput(inputId = "second_inf",label = "Simulate secondary infection", value = FALSE),
      conditionalPanel("input.second_inf", 
                       span("Time of secondary infection (hours after drug administration):",
                            sliderInput("t_secondary", NULL, min = 24, max = 192, step = 12, value = init_param$t_secondary, width = '50%')
                       )
      ),
      bookmarkButton()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # column(width = 6,
      #        br(),
      #        p('The window opens when the usually single parasite resistant de novo, which emerges from the liver, and its progeny can survive and grow; i.e., the blood concentrations have fallen to or below the MIC for this level of resistance.'),
      #        p('The window of selection closes when antimalarial blood concentrations have fallen to levels such that the survival probabilities of resistant and sensitive parasites are equal.'),
      #        htmlOutput('text_selection')
      # )
      # ),
      # fluidRow(column(width = 5,
      #                 plotOutput("window_selection", height = '120px')
      # )),
      htmlOutput('conc_growth_r'),
      htmlOutput('conc_growth_s'),
      checkboxInput('zoom_y', 'Zoom into y-axis', value = FALSE),
      plotOutput("combined_graphs", height = "800px")
      
    )
  )
)