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
                  c("Tables", "Plots", "Regression")),
      
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
            conditionalPanel("input.PanelType == 'Tables'", h2("Tables Used in Processing")),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('table')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('result')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('weatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('dWeatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outUsageHistory')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outdModel')),
            
            conditionalPanel("input.PanelType == 'Plots'", h2("Plots for Data Exploration")),
            conditionalPanel("input.panelType == 'Plots'", showOutput('mdPlot','nvd3')),
            conditionalPanel("input.panelType == 'Plots'", showOutput('mdScatter','nvd3')),
            
            conditionalPanel("input.PanelType == 'Regression'", h2("Regression Results")),
            conditionalPanel("input.panelType == 'Regression'", textOutput('r_squared')),
            conditionalPanel("input.panelType == 'Regression'", tableOutput('coef')),
            conditionalPanel("input.panelType == 'Regression'", plotOutput('regressionPlot'))
            )
  )
))