#Copyright warning
div(class = 'footer',
    fluidRow(
      column(1, HTML('&nbsp;')),
      column(3, tags$a(href= 'http://www.tropmedres.ac/home', strong("Mahidol Oxford Research Unit"))), 
      column(4, actionLink(label = "Take a Snapshot of the App", inputId = "._bookmark_")),
      column(3, align = 'right', tags$a(href="mailto:olivier.celhay@gmail.com", strong("Contact Us"))),
      column(1, HTML('&nbsp;'))
    )
)