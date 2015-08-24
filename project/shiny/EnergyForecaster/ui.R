library(shiny)
library(lubridate)
library(rCharts)
library(markdown)

shinyUI(fluidPage(
  titlePanel("Energy Consumption Regression Modeller"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"), 
      helpText("1. Enter your address in any format."),
      helpText("2. Upload csv (see example links below which are default city)."),
      helpText("3. Hit Go!"),
      helpText("4. Explore the model using 'Panel Types' available below."),
    
      textInput("address", "Enter Address:", "Mississauga, ON"),

      dateRangeInput("dates", 
                     "Date range (maximum):",
                     start = ymd(Sys.Date()) - years(2), 
                     end = Sys.Date()
                     ),
                     
      fileInput('usageHistory', 'Choose CSV file:', 
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
            conditionalPanel("input.panelType == 'Tables'", textOutput('tablesTitle')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('table')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('result')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('weatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('dWeatherSample')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outUsageHistory')),
            conditionalPanel("input.panelType == 'Tables'", tableOutput('outdModel')),
            
            conditionalPanel("input.panelType == 'Plots'", textOutput('plotsTitle')),
            conditionalPanel("input.panelType == 'Plots'", showOutput('mdPlot','nvd3')),
            conditionalPanel("input.panelType == 'Plots'", showOutput('mdScatter','nvd3')),
            
            conditionalPanel("input.panelType == 'Regression'", textOutput('regressionTitle')),
            conditionalPanel("input.panelType == 'Regression'", textOutput('regressionSummary')),                  
            conditionalPanel("input.panelType == 'Regression'", textOutput('r_squared')),
            conditionalPanel("input.panelType == 'Regression'", tableOutput('coef')),
            conditionalPanel("input.panelType == 'Regression'", plotOutput('regressionPlot'))
            )
  )
))