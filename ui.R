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
  
  # Add window title ----
  titlePanel(title = NULL, windowTitle = "Resistance Selection"),
  
  
  # Sidebar with inputs ----
  sidebarLayout(
    sidebarPanel(
      
      h4(icon("tablets"), "Amount of Drug Administered"),
      fluidRow(column(width = 12,
                      noUiSliderInput("dose", label = NULL, min = 0, max = 1000, step = 50, update_on = 'end',
                                      value = init_param$dose, format = wNumbFormat(decimals = 0, suffix = 'mg'), height = '15px')
      )),
      hr(),
      
      h4(icon("flask"), "Pharmacokinetic Characteristics"),
      
      fluidRow(
        column(width = 6,
               span("Absorption Parameter", sliderInput("ka", NULL, min = 0, max = 100, step = 5, ticks = FALSE,
                                                        post = ' %', value = init_param$ka))
        ),
        column(width = 6,
               span("Bioavailability", sliderInput("Fa", NULL, min = 0, max = 1, step = 0.05, ticks = FALSE, 
                                                   value = init_param$Fa))
        )
      ),
      fluidRow(
        column(width = 6,
               span("Volume of Distribution", sliderInput("V", NULL, min = 4, max = 20, step = 1, ticks = FALSE, 
                                                          post = ' l.', value = init_param$V))
        ),
        column(width = 6,
               span("Clearance Rate", sliderInput("CL", NULL, min = 0.1, max = 1, ticks = FALSE,
                                                  post = ' l/h', value = init_param$CL))
        )
      ),
      
      htmlOutput("half_life"),
      
      
      hr(),
      h4(icon('caret-right'), "Parasites Load"),
      fluidRow(
        column(width = 6,
               div(class = "resistant", "Resistant Parasites")
        ),
        column(width = 6,
               div(class = "sensitive", "Sensitive Parasites")
        )
      ),
      p("At Drug Admininstration (t = 0)"),
      fluidRow(
        column(6, selectInput('nb_resistant_t0', NULL, choices = c(0, 100000), selected = 100000, width = '50%')),
        column(6, selectInput('nb_sensitive_t0', NULL, choices = c(0, 1000000000000), selected = 1000000000000, width = '50%'))
      ),
      checkboxInput(inputId = "second_inf",label = "Add a Secondary Infection", value = FALSE),
      
      conditionalPanel("input.second_inf",
                       p('Time of introduction of resistants (hours since start of treatment)'),
                       noUiSliderInput("t_secondary", label = NULL, min = 12, max = 192, step = 12, update_on = 'end',
                                       value = init_param$t_secondary, format = wNumbFormat(decimals = 0, suffix = ' h'), height = '15px')
      ),
      
      fluidRow(
        column(6, conditionalPanel("input.second_inf", selectInput('nb_resistant_sec', NULL, choices = c(1, 100000), selected = 1, width = '50%'))),
        column(6, conditionalPanel("input.second_inf", selectInput('nb_sensitive_sec', NULL, choices = c(1, 1000000000), selected = 1, width = '50%')))
      ),
      
      hr(),
      h4(icon('caret-right'), "Parasites Attributes"),
      fluidRow(
        column(width = 6,
               div(class = "resistant", "Resistant Parasites")
        ),
        column(width = 6,
               div(class = "sensitive", "Sensitive Parasites")
        )
      ),
      fluidRow(
        column(width = 6,
               span("EC50", sliderInput("EC50_r", NULL, min = 0, max = 100, value = init_param$EC50_r))
               
        ),
        column(width = 6,
               span("EC50", sliderInput("EC50_s", NULL, min = 0, max = 100, value = init_param$EC50_s))
        )
      ),
      fluidRow(
        column(width = 6,
               span('Multiplication Rate (per 48h cycle)', 
                    sliderInput("growth_r", NULL, min = 0, max = 10, value = init_param$growth_r))
               
        ),
        column(width = 6,
               span('Multiplication Rate (per 48h cycle)', 
                    sliderInput("growth_s", NULL, min = 0, max = 10, value = init_param$growth_s))
               
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
      bookmarkButton()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
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
                                   $t_{1/2} = \\frac{0.693.V}{CL}$")
                       ))
      ),
      
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
      htmlOutput('msw'),
      
      hr(),
      checkboxInput('zoom_y', 'Focus on Mutant Selection Window (between MPC and MIC)', value = FALSE, width = '100%'),
      plotOutput("plot_1", height = "300px") %>% 
        withSpinner(),
      plotOutput("plot_2", height = "300px") %>% 
        withSpinner(),
      plotOutput("plot_3", height = "200px") %>% 
        withSpinner(),
      htmlOutput("danger_time")
    )
  )
)