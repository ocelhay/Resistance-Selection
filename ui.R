ui <- function(request) { fluidPage(
  # theme = shinytheme("flatly"),
  theme = "bootstrap.css",
  includeCSS("./www/styles.css"),
  title = "Resistance Selection",
  
  # Change default slider
  chooseSliderSkin('HTML5'),
  
  # KaTeX
  # tags$head(
  #   tags$link(rel="stylesheet",
  #             href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
  #             integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
  #             crossorigin="anonymous"),
  #   HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
  #   HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
  #   HTML('
  #        <script>
  #        document.addEventListener("DOMContentLoaded", function(){
  #        renderMathInElement(document.body, {
  #        delimiters: [{left: "$", right: "$", display: false}]
  #        });
  #        })
  #        </script>')
  # ),
  
  # Add window title ----
  # titlePanel(title = "Resistance Selection Showcase App", windowTitle = "Resistance Selection"),
  fluidRow(column(6,
                  h3("Resistance Selection Showcase App"),
                  span(actionLink('about_model', span('Our model', icon('external-link-alt'))),
                       "simulate the evolution of", em("sensitive"), " and ", em("resistant"), 
                       "parasites. This App allow you to modify several parameters related to the ",
                       actionLink('about_drug', span('drug concentration and half life', icon('external-link-alt'))),
                       "as well as the",
                       actionLink('about_dose_response', span('dose-response relationship', icon('external-link-alt'))),
                       "You can readily observe the impact of these parameters on the ",
                       actionLink('about_msw', span('Mutant Selection Window (MSW)', icon('external-link-alt'))),
                       " and the potential for resistance to the drug in different scenario of infections."
                  )
  ),
  column(6,
         div( class = 'logo',
              img(src = 'MORU_FS_Partners.png', align = "right", height = '75px')
         )
  )
  ),
  br(),
  
  
  # Sidebar with inputs ----
  
  fluidRow(
    column(width = 5,
           tabsetPanel(
             # ----
             tabPanel("Drug Dose & Pharmacokinetics",
                      div(class = 'well2',
                          br(),
                          sliderInputSplit("dose", label = span("Dose of Drug Absorbed", br(), "(t = 0)"), l = 5, class = 'dl', min = 0, max = 1000, step = 50, value = 500, post = ' mg', ticks = FALSE),
                          br(),
                          h4(icon('caret-right'), "Drug Pharmacokinetics"),
                          sliderInputSplit("ka", label = "Absortion", l = 5, class = 'sl', min = 0, max = 100, step = 5, value = 100, post = '%', ticks = FALSE),
                          sliderInputSplit("Fa", label = "Bioavailability", l = 5, class = 'sl', min = 0, max = 1, step = 0.05, value = 0.8, ticks = FALSE),
                          sliderInputSplit("V", label = "Volume of Distribution", l = 5, class = 'dl', min = 4, max = 20, step = 1, ticks = FALSE, post = ' l.', value = 6),
                          sliderInputSplit("CL", label = "Clearance Rate", l = 5, class = 'dl', min = 0.1, max = 1, ticks = FALSE, post = ' l/h', value = 0.4)
                      )
                      
             ),
             tabPanel("Dose-Response",
                      div(class = 'well2',
                          br(),
                          h4(icon('caret-right'), "EC50"),
                          sliderInputSplit(inputId = "EC50_s", label = "Sensitive Parasites", min = 0, max = 100, value = 10, ticks = FALSE),
                          sliderInputSplit(inputId = "EC50_r", label = "Resistant Parasites", min = 0, max = 100, value = 50, ticks = FALSE),
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
                          sliderInputSplit(inputId = "growth_s", label = "Sensitive", min = 0, max = 10, value = 8, ticks = FALSE),
                          sliderInputSplit(inputId = "growth_r", label = "Resistant", min = 0, max = 10, value = 6, ticks = FALSE),
                          htmlOutput('message_rate'),
                          
                          h4(icon('caret-right'), "Primary Infection — t = 0"),
                          selectInputSplit(inputId = 'nb_sensitive_t0', label = "Quantity of Sensitive", choices = c(10^9, 10^12), selected = 10^12, l = 7, class = 'tl'),
                          selectInputSplit(inputId = 'nb_resistant_t0', label = "Quantity of Resistant", choices = c(0, 100000), selected = 100000, l = 7, class = 'tl'),
                          
                          h4(icon('caret-right'), "Secondary Infection"),
                          selectInputSplit('nb_sensitive_sec', label = "Quantity of Sensitive", choices = c(0, 1, 1000000000), selected = 0, l = 7, class = 'tl'),
                          selectInputSplit('nb_resistant_sec', label = "Quantity of Resistant", choices = c(0, 1, 100000), selected = 0, l = 7, class = 'tl'),
                          
                          conditionalPanel("input.nb_sensitive_sec != 0 | input.nb_resistant_sec != 0",
                                           sliderInputSplit(inputId = "t_secondary", label = 'Time of Infection', l = 5, class = 'dl',
                                                            min = 12, max = 192, step = 6, value = 120, post = ' h', ticks = FALSE),
                                           htmlOutput('time_hour')
                          )
                      )
             )
           ),
           
           div(class = 'info-bottom',
               fluidRow(column(12, htmlOutput("half_life"))),
               fluidRow(column(12, htmlOutput('window')),
                        fluidRow(column(4, offset = 1, htmlOutput('mpc')), column(4, offset = 1, htmlOutput('mic'))),
                        br(),
                        fluidRow(column(4, offset = 1, htmlOutput('mpc_time')), column(4, offset = 1, htmlOutput('mic_time')))
               )
           )
    ),
    
    
    
    
    
    # column(width = 1, br()),
    
    # Show a plot of the generated distribution
    column(width = 7,
           plotOutput("plot_1", height = "200px") %>% 
             withSpinner(type = 7, size = 0.7),
           br(),
           
           plotOutput("plot_2", height = "300px") %>% 
             withSpinner(type = 7, size = 0.7),
           br(),
           
           plotOutput("plot_3", height = "150px") %>% 
             withSpinner(type = 7, size = 0.7),
           htmlOutput("danger_time")
    )
  ),
  br(), br(),
  actionLink(label = "Take a Snapshot of the App", inputId = "._bookmark_"),
  br()
)
}