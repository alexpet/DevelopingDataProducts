library(shiny)
library(lubridate)
library(rCharts)
library(markdown)

shinyUI(fluidPage(
        titlePanel("Energy Consumption Regression Modeller"),
        
        sidebarLayout(
                sidebarPanel(
                        h4("Quick Instructions:"), 
                        helpText("Click on the Manual tab for more detail"),
                        helpText("1. Enter your address information."),
                        textInput("address", "Enter Address:", "Mississauga, ON"),
                        helpText("2. Set a time range for weather history."),
                        dateRangeInput("dates", 
                                       "Date range (maximum):",
                                       start = ymd(Sys.Date()) - years(2), 
                                       end = Sys.Date()
                        ),
                        helpText("3. Upload csv without headers (samples in Manual tab)."),
                        fileInput('usageHistory', 'Choose CSV file:', 
                                  accept = c('text/csv', 'text/comma-separated-values,text/plain', 
                                             '.csv')
                        ),
                        helpText("4. Hit Go!"),
                        actionButton("goButton","Go!"),
                        helpText("5. Explore the panel tabs to review the model.")
                ),
                
                mainPanel(
                        tabsetPanel(type="tabs",
                                    tabPanel("Processing",
                                             tabsetPanel(type = "tabs",
                                                         tabPanel("Training Model",
                                                                  dataTableOutput('outdModel')
                                                         ),
                                                         tabPanel("Normal Temperatures",
                                                                  dataTableOutput('outndModel')
                                                         ),
                                                         tabPanel("Source Tables",
                                                                  h3("Weather Station"),
                                                                  tableOutput('table'),
                                                                  br(),
                                                                  h3("Weather Files"),
                                                                  tableOutput("result"),
                                                                  br(),
                                                                  h2("Raw Weather Data Sample"),
                                                                  tableOutput('weatherSample'),
                                                                  br(),
                                                                  h2("Processed Consumption"),
                                                                  tableOutput('outUsageHistory')
                                                         )
                                             )
                                    ),
                                    tabPanel("Exploration",
                                             tabsetPanel(type="tabs",
                                                         tabPanel("Location",
                                                                  tags$style('.leaflet {height: 400px;}'),
                                                                  showOutput('leafletMap',
                                                                             'leaflet')
                                                         ),
                                                         tabPanel("Consumption vs Temperature",
                                                                  showOutput('mdPlot','nvd3')
                                                         ),
                                                         tabPanel("Parameters Over Time",
                                                                  htmlOutput('motionChart')
                                                         )
                                             )
                                    ),
                                    tabPanel("Prediction",
                                             tabsetPanel(type="tabs",
                                                         tabPanel("Summary",
                                                                  br(),
                                                                  textOutput('r_squared'),
                                                                  br(),
                                                                  tableOutput('coef')
                                                         ),
                                                         tabPanel("Residuals",
                                                                  plotOutput('residualPlots')
                                                         ),
                                                         tabPanel("Backcast",
                                                                  plotOutput('regressionPlot')
                                                         ),
                                                         tabPanel("Normal Forecast",
                                                                  sliderInput("temps",
                                                                              "Deviation from Normal Degrees Farenheit",
                                                                              value = 0,
                                                                              min = -10,
                                                                              max = 10
                                                                  ),
                                                                  plotOutput('predictionPlot')
                                                         ),
                                                         tabPanel("R Output",
                                                                  h2('Model Summary'),
                                                                  textOutput('regressionSummary'),
                                                                  h2('Residuals'),
                                                                  tableOutput('regressionResiduals')
                                                         )
                                             )
                                    ),
                                    tabPanel("Manual",
                                             tabsetPanel(type="tabs",
                                                         tabPanel("Instructions",
                                                                  h4("Step 1: Entering your address information"),
                                                                  helpText(paste("You can put in your address in any",
                                                                                 "recognizable format, e.g. '123 First St,",
                                                                                 "City, State, ZIP/Postal, Country.'",
                                                                                 "You can also just put in a city and",
                                                                                 "province as shown in the default.",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Step 2: Setting time range for weather history"),
                                                                  helpText(paste("This will allow you to select how many",
                                                                                 "years of weather history to include.",
                                                                                 "You will want to include as much weather",
                                                                                 "as you have consumption history to start.",
                                                                                 "Later, you can increase the time range",
                                                                                 "at the start to generate a more normalized",
                                                                                 "temperature forecast. At least 10 years is",
                                                                                 "sufficient.",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Step 3: Upload your consumption history in CSV"),
                                                                  helpText(paste("The CSV has three columns:\n",
                                                                                 "1. Start Date \n",
                                                                                 "2. End Date \n",
                                                                                 "3. Energy consumption (gas or power) \n",
                                                                                 "The file should be comma separate, without",
                                                                                 "headers and no quoted identifiers.",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Step 4: Hit the Go! button"),
                                                                  helpText(paste("This will collect data you have provided",
                                                                                 "and fetch appropriate weather data from",
                                                                                 "the National Oceanic and Atmospheric",
                                                                                 "Adminstration. It takes a few minutes",
                                                                                 "to process, so please wait until information",
                                                                                 "shows on the 'Training Model' tab.",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Step 5: Explore the panel tabs to review the model"),
                                                                  helpText(paste("Click on various tabs to explore the model.",
                                                                                 sep = " "))
                                                         ),
                                                         tabPanel("Tab Descriptions",
                                                                  h4("Process"),
                                                                  helpText(paste("This tab shows the 'Training Model'",
                                                                                 "used to train the linear regression model.",
                                                                                 "A second tab shows 'Normal Temperatures'",
                                                                                 "that will be used in the general prediction",
                                                                                 "model. 'Source Tables' shows the raw data. \n",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Exploration"),
                                                                  helpText(paste("This tab shows some useful",
                                                                                 "information about the underlying data.",
                                                                                 "'Location' is a map showing the address you",
                                                                                 "and the closest weather station found.",
                                                                                 "and the closest weather station found.",
                                                                                 "'Consumption vs Temperature' plots average",
                                                                                 "daily consumption against average daily",
                                                                                 "temperature by month in celsius. Finally",
                                                                                 "'Parameters Over Time' is a small motion",
                                                                                 "chart which allows you to explore the",
                                                                                 "relationship of temperatire to consumption. \n",
                                                                                 sep = " ")
                                                                  ),
                                                                  h4("Prediction"),
                                                                  
                                                                  helpText(paste("This shows the underlying linear",
                                                                                 "model that was generated. The 'Summary' tab",
                                                                                 "shows the model that was selected. The",
                                                                                 "application will try 5 general models and",
                                                                                 "select the one with highest R-squared.",
                                                                                 "'Residuals' shows standard R residual plots",
                                                                                 "useful for diagnosis of the model. 'Backcast'",
                                                                                 "show how well the model fits the training data",
                                                                                 "'Normal Forecast' shows the model under normal",
                                                                                 "temperatures and has a slider to decrease/increase",
                                                                                 "the temperature to see the effect. Finally",
                                                                                 "'R Output' shows the raw model output from R.",
                                                                                 sep = " ")
                                                                  )
                                                         )
                                             )
                                    )
                        )
                )
        )
)
)