################################################################.
#    Modal ----
################################################################.

# "Take a Tour" Modal - first window ----

observeEvent(input$about_model,  showModal(modal_about_1))

modal_about_1 <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE,
  
  
  p('Drug resistance is the reduction in effectiveness of a medication such as an antimicrobial or an antineoplastic in treating a disease or condition. Our simple mathematica model simulates the co-evolution of sensitive and resistant parasites under a drug regimen.'),
  img(src = "./model.png", width = '800px'),
  
  footer = fluidRow(
      column(3, HTML('')),
      column(6, align = 'center', 
             HTML('&nbsp; &nbsp;'),
             HTML('&nbsp; &nbsp;'),
             actionButton("next_ma1","Next", icon("chevron-right"))),
      column(3, align = 'right', actionButton('close_ma1', 'Close', icon("times-circle")))
  )
)

observeEvent(input$next_ma1,  showModal(modal_about_2))
observeEvent(input$close_ma1, removeModal())



# "Take a Tour" Modal - second window
modal_about_2 <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE,
  
  img(src = "./drug_concentration.png", width = '800px'),
  
  footer = fluidRow(
    column(3, HTML('')),
    column(6, align = 'center',
           actionButton("back_ma2","Back", icon("chevron-left")), 
           HTML('&nbsp; &nbsp;'),
           actionButton("next_ma2","Next", icon("chevron-right"))),
    column(3, align = 'right', actionButton('close_ma2', 'Close', icon("times-circle")))
  )
)

#call from either next1 button or back button from modal 3
observeEvent(input$back_ma2,  showModal(modal_about_1))
observeEvent(input$next_ma2,  showModal(modal_about_3))
observeEvent(input$close_ma2, removeModal())


# "Take a Tour" Modal - third window
modal_about_3 <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE,
  
  img(src = "./msw.png", width = '800px'),
  
  footer = fluidRow(
    column(3, HTML('')),
    column(6, align = 'center',
           actionButton("back_ma3","Back", icon("chevron-left")), 
           HTML('&nbsp; &nbsp;'),
           actionButton("next_ma3","Next", icon("chevron-right"))),
    column(3, align = 'right', actionButton('close_ma3', 'Close', icon("times-circle")))
  )
)

#call from either next1 button or back button from modal 3
observeEvent(input$back_ma3,  showModal(modal_about_2))
observeEvent(input$next_ma3,  showModal(modal_about_4))
observeEvent(input$close_ma3, removeModal())


#"Take a Tour" Modal - third window
modal_about_4 <- modalDialog(
  size = "l", align= "center", easyClose = TRUE, fade = FALSE,
  
  HTML('We will provice more information on the model here.'),
  
    footer = fluidRow(
      column(3, HTML('')),
      column(6, align = 'center',
             actionButton("back_ma4","Back", icon("chevron-left")), 
             HTML('&nbsp; &nbsp;'),
             HTML('&nbsp; &nbsp;')
      ),
      column(3, align = 'right', actionButton('close_ma4', 'Close', icon("times-circle")))
    )
)

#call from either next1 button or back button from modal 3
observeEvent(input$back_ma4,  showModal(modal_about_3))
observeEvent(input$close_ma4, removeModal())