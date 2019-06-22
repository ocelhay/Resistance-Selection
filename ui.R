ui <- function(request) { fluidPage(
  theme = "bootstrap.css",
  includeCSS("./www/styles.css"),
  
  title = "Drug Resistance Selection App",
  
  # Change default slider
  chooseSliderSkin('HTML5'),
  
  # Add Google Analytics
  tags$head(includeHTML("./www/gtag.js")),
  
  # Add window title ----
  fluidRow(
    column(5,
           em(h2("Drug Resistance Selection App")),
           span('Modify the parameters related to the drug, dose-response and parasites and observe the potential for resistance selection.', 
                actionLink('about_model', span('About Drug Resistance and The App', icon('external-link-alt'))))
    ),
    column(7,
           div(class = 'logo',
               tags$a(href= 'http://www.tropmedres.ac/home', img(src = 'MORU_FS_Partners.png', height = '60px'))
           )
    )
  ),
  
  # Sidebar with inputs ----
  
  fluidRow(
    column(width = 5,
           h3("Parameters"),
           tabsetPanel(
             # ----
             tabPanel("Drug Regimen",
                      div(class = 'well2',
                          br(),
                          fluidRow(
                            column(7, 
                          numericInput("dose_1", label = "Dose of Drug Apsorbed (mg)", min = 1, max = 1000, value = 80, width = '200px')
                            ),
                          column(5, 
                                 strong("Taken at ", em("t = 0"))
                          )
                          ),
                          hr(),
                          fluidRow(
                            column(7, 
                                   numericInput("dose_2", label = "Second Dose (mg)", min = 0, max = 1000, value = 80, width = '200px')
                            ),
                            column(5, 
                                   conditionalPanel(
                                     condition = "input.dose_2 > 0",
                                     sliderInput(inputId = "t_dose_2", label = 'Time of Intake', min = 12, max = 192, step = 6, value = 24, post = ' h', ticks = FALSE)
                                   )
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(7, 
                                   numericInput("dose_3", label = "Third Dose (mg)", min = 0, max = 1000, value = 80, width = '200px')
                            ),
                            column(5, 
                                   conditionalPanel(
                                     condition = "input.dose_3 > 0",
                                     sliderInput(inputId = "t_dose_3", label = 'Time of Intake', min = 12, max = 192, step = 6, value = 48, post = ' h', ticks = FALSE)
                                   )
                            )
                          )
                      )
             ),
             tabPanel("Drug PK",
                      div(class = 'well2',
                          br(),
                          h4(icon('caret-right'), "Drug Pharmacokinetics"),
                          numericInputSplit("ka", label = "Absorption rate constant (24h)", l = 5, class = 'sl', min = 0, max = 48, value = 12, width = '100px'),
                          numericInputSplit("ke", label = "Elimination rate constant (24h)", l = 5, class = 'sl', min = 0, max = 48, value = 10, width = '100px'),
                          sliderInputSplit("Fa", label = "Bioavailability (F)", l = 5, class = 'sl', min = 0, max = 1, step = 0.05, value = 0.8, ticks = FALSE, 
                                           animate = animationOptions(interval = 3000, loop = TRUE)),
                          sliderInputSplit("V", label = "Volume of Distribution (V)", l = 5, class = 'dl', min = 4, max = 20, step = 0.5, ticks = FALSE, post = ' l.', value = 6)
                          # sliderInputSplit("CL", label = "Clearance Rate", l = 5, class = 'dl', min = 0.1, max = 1, step = 0.05, ticks = FALSE, post = ' l/h', value = 0.4)
                      )
                      
             ),
             tabPanel("Dose-Response",
                      div(class = 'well2',
                          br(),
                          h4(icon('caret-right'), "EC50"),
                          numericInputSplit(inputId = "EC50_s", label = "Sensitive Parasites", min = 0, max = 500, value = 0.1),
                          numericInputSplit(inputId = "EC50_r", label = "Resistant Parasites", min = 0, max = 500, value = 0.2),
                          htmlOutput('message_EC50'),
                          
                          h4(icon('caret-right'), "Other Drug-Effect Parameters"),
                          numericInputSplit(inputId = "k1", label = "First order rate constant for maximum parasite killing: ", min = -10, max = -1, value = -1, width = "80px", l = 9),
                          numericInputSplit(inputId = "n", label = "Slope of the concentration-effect curve: ", min = -10, max = 10, value = 1, width = "80px", l = 9)
                      )
             ),
             
             # ----
             tabPanel("Parasites",
                      div(class = 'well2',
                          br(),
                          h4(icon('caret-right'), "Multiplication Rate (per 48h cycle)"),
                          sliderInputSplit(inputId = "growth_s", label = "Sensitive", min = 0, max = 15, value = 10, ticks = FALSE),
                          sliderInputSplit(inputId = "growth_r", label = "Resistant", min = 0, max = 15, value = 9, ticks = FALSE),
                          htmlOutput('message_rate'),
                          
                          h4(icon('caret-right'), "Primary Infection — t = 0"),
                          selectInputSplit(inputId = 'nb_sensitive_t0', label = "Quantity of Sensitive", choices = c(10^9, 10^12), selected = 10^12, l = 7, class = 'tl'),
                          selectInputSplit(inputId = 'nb_resistant_t0', label = "Quantity of Resistant", choices = c(0, 10^5), selected = 0, l = 7, class = 'tl'),
                          
                          h4(icon('caret-right'), "Secondary Infection"),
                          selectInputSplit('nb_sensitive_sec', label = "Quantity of Sensitive", choices = c(0, 1, 10^9), selected = 0, l = 7, class = 'tl'),
                          selectInputSplit('nb_resistant_sec', label = "Quantity of Resistant", choices = c(0, 1, 10^5, 10^9), selected = 10^5, l = 7, class = 'tl'),
                          
                          conditionalPanel("input.nb_sensitive_sec != 0 | input.nb_resistant_sec != 0",
                                           sliderInputSplit(inputId = "t_secondary", label = 'Time of Infection', l = 5, class = 'dl',
                                                            min = 6, max = 192, step = 3, value = 36, post = ' h', ticks = FALSE),
                                           htmlOutput('time_hour')
                          )
                      )
             )
           ),
           fluidRow(column(12, 
                           htmlOutput("half_life"),
                           htmlOutput("auc"),
                           htmlOutput("cmax"),
                           htmlOutput("tmax"),
                           htmlOutput("clf"),
                           htmlOutput("vf"),
                           htmlOutput("ka"),
                           htmlOutput("ke")
           )
           )
    ),
    
    # Show a plot of the generated distribution
    column(width = 7,
           fluidRow(
             column(width = 7, 
                    h3("Drug Concentration"),
                    plotOutput("plot_1", height = "300px") %>% 
                      withSpinner(type = 7, size = 0.7),
                    br()),
             column(width = 5, 
                    h3("Mutant Selection Window"),
                    div(class = 'info-bottom',
                        fluidRow(column(6, htmlOutput('mpc')), column(6, htmlOutput('mic'))),
                        br(),
                        fluidRow(column(6, htmlOutput('mpc_time')), column(6, htmlOutput('mic_time'))),
                        fluidRow(column(12, htmlOutput('window')))
                    ))
           ),
           
           fluidRow(
             column(width = 7, 
                    h3("Parasites"),
                    plotOutput("plot_2", height = "300px") %>% 
                      withSpinner(type = 7, size = 0.7),
                    br()),
             column(width = 5, 
                    h3("Proportion Resistant"),
                    htmlOutput("danger_time"),
                    br(),
                    plotOutput("plot_3", height = "150px") %>% 
                      withSpinner(type = 7, size = 0.7)
             )
           )
    )
  ),
  br(), br(),
  source('./www/footer.R', local = TRUE)$value
  
)
}