################################################################.
#    Modal ----
################################################################.

# Welcome Modal ----

welcome_modal <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE, 
  
  
  p('Welcome to the Resistance Selection Showcase App'),
  p("Learn more about the tool or get started"),
  
  footer = fluidRow(
    column(3, HTML('')),
    column(3, actionButton("tour","Take a tour", icon("play-circle"))),
    column(3, actionLink('close_welcome', 'Close and Go to the Tool', icon("times-circle"))),
    column(3, HTML(''))
  )
)

showModal(welcome_modal)

observeEvent(input$close_welcome, { removeModal() })



# "Take a Tour" Modal - first window ----

first_modal <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE, footer = NULL,
  
  p('You will want to change parameters'),
  img(src = './welcome_modal/parameters.png', height = '200px'),
  
  fluidRow(
    column(3, HTML('')),
    column(3, actionButton("next1","Next", icon("play-circle"))),
    column(3, modalButton("Exit", icon("times-circle"))),
    column(3, HTML(''))
  )
)

# call from either tour button or back button from modal 2
observeEvent(input$tour,  showModal(first_modal))
observeEvent(input$back1, showModal(first_modal))

# "Take a Tour" Modal - second window
second_modal <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE, footer = NULL,
  
  p('Observe the impact on half life and MSW'),
  img(src = './welcome_modal/output_1.png', height = '200px'),
  
  p('Observe the impact on drug concentration'),
  img(src = './welcome_modal/output_2.png', height = '200px'),
  
  p('Observe the impact on the time during which parameters'),
  img(src = './welcome_modal/output_2.png', height = '200px'),
  
  fluidRow(
    column(3, actionButton("back1","Back", icon("backward"))),
    column(3, actionButton("next2","Next", icon("play-circle"))),
    column(3, modalButton("Exit", icon("times-circle"))),
    column(3, HTML(''))
    )
)

#call from either next1 button or back button from modal 3
observeEvent(input$next1,  showModal(second_modal))
observeEvent(input$back2,  showModal(second_modal))


#"Take a Tour" Modal - third window
third_modal <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE, footer = NULL,
  
  p('Final Statement'),
  
  fluidRow(
    column(3, actionButton("back2","Back", icon("backward"))),
    column(3, HTML('')),
    column(3, modalButton("Exit", icon("times-circle"))),
    column(3, HTML(''))
  )
)

# call from next2 button
observeEvent(input$next2, showModal(third_modal))