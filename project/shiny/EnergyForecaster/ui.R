library(shiny)
library(lubridate)
library(rCharts)

shinyUI(fluidPage(
  titlePanel("Energy Forecaster"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter your home city and province or state. 
        Then upload your usage data in csv format.
        A forecast model will genererated for you"),
    
      textInput("address", "Enter Weather Station:", "Mississauga, ON"),

      dateRangeInput("dates", 
                     "Date range",
                     start = ymd(Sys.Date()) - years(2), 
                     end = Sys.Date()
                     ),
                     
      fileInput('usageHistory', 'Choose CSV file', 
                accept = c('text/csv', 'text/comma-separated-values,text/plain', 
                           '.csv')
      ),
      
#       tags$hr(),
#       
#       checkboxInput('header', 'Header', TRUE),
#       radioButtons('sep', 'Separator',
#                    c(Comma=',',
#                      Semicolon=';',
#                      Tab='\t'),
#                    ','),
#       radioButtons('quote', 'Quote',
#                    c(None='',
#                      'Double Quote'='"',
#                      'Single Quote'="'"),
#                    '"'),
      
       submitButton(text = "Submit")
),
    
    mainPanel(
              textOutput('outaddress'),
              textOutput('outgeo'),
              tableOutput('table'),
              tableOutput('result'),
              tableOutput('outUsageHistory'),
              plotOutput('w'),
              plotOutput('u')
              )
  )
))