#Copyright warning
div(class = 'footer',
    fluidRow(
      column(1, HTML('&nbsp;')),
      column(3, tags$a(href= 'http://www.tropmedres.ac/home', span("Mahidol Oxford Research Unit", icon('external-link-alt')))), 
      column(4, actionLink(label = "Bookmark the App with Parameters", inputId = "._bookmark_")),
      column(3, align = 'right', tags$a(href = "mailto:olivier.celhay@gmail.com", span(icon('envelope'), "Contact Us"))),
      column(1, HTML('&nbsp;'))
    )
)