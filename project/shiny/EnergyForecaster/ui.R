library(shiny)
library(lubridate)

shinyUI(fluidPage(
  titlePanel("Energy Forecaster"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter your home city and province or state. 
        Then upload your usage data in csv format.
        A forecast model will genererated for you"),
    
      textInput("address", "Enter Weather Station:", "Beverly Hills, 90210, CA"),
    
      dateRangeInput("dates", 
        "Date range",
        start = ymd(Sys.Date()) - years(2), 
        end = Sys.Date(),
    ),
    
    submitButton(text = "Submit")),
    
    mainPanel(
              textOutput('outaddress'),
              textOutput('outgeo'),
              tableOutput('table'),
              tableOutput('result'),
              # plotOutput('w'),
              textOutput('start'),
              textOutput('end')
              )
  )
))