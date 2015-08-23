library(shiny)
library(lubridate)
library(rCharts)
library(markdown)

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
      
      selectInput("panelType", "Panel Type",
                  c("Tables", "Plots", "Debug")),
      
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
      
       actionButton("goButton","Go!")
),
    
    mainPanel(
            conditionalPanel("input.panelType == 'Tables'", tableOutput('table')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('result')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('weatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('dWeatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outUsageHistory')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outdModel')),
            
            conditionalPanel("input.panelType == 'Plots'", plotOutput('w')),
            
            conditionalPanel("input.panelType == 'Debug'", textOutput('outaddress'))
#                 tableOutput('table')
#                 tableOutput('result'),
#                 tableOutput('weatherSample'),
#                 tableOutput('dWeatherSample'),
#                 tableOutput('outUsageHistory'),
#                 tableOutput('outdModel')
#                     tabPanel("Tables",{
#                             
#                     }),
#                     tabPanel("Plots",{
#                             plotOutput('w')
#                             # plotOutput('u')
#                     }),
#                     tabPanel("Debug",{
#                             textOutput('outaddress')
#                             # textOutput('outgeo')
#                     })
            )
  )
))