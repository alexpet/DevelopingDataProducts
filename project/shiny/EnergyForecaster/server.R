# server.R

library(lubridate)
library(quantmod)
library(RCurl)
library(RJSONIO)
library(rCharts)
library(zoo)
library(ggplot2)
library(sqldf)
library(reshape)
library(Rcpp)
library(googleVis)
source("helpers.R")

shinyServer(function(input, output) {
    #Get the closest weather station
    dataInput <- eventReactive(input$goButton,{
            loc <- gGeoCode(input$address)
            findClosestStation(loc, st, input$dates[1], input$dates[2])
    })
    #Get info on available weather data
    result <- eventReactive(input$goButton,{
            getWeather(dataInput(), input$dates[1], input$dates[2])
    })
    #Get the actual weather
    weather <- eventReactive(input$goButton, {
            r <- result()
            w <- loadWeather(r)
            w <- processWeather(w)
            w
    })
    #Create daily weather
    dWeather <- eventReactive(input$goButton, {
            dw <- dailyWeather(weather())
            dw[dw$DATE >= input$dates[1] & dw$DATE <= input$dates[2],]
    })
    #Process the usages
    usages <- eventReactive(input$goButton,{
            usagesData <- input$usageHistory
            if (is.null(usagesData))
                    return(NULL)
            usg <- readUsageHistory(
                    uFilePath = usagesData$datapath,
                    uHeader = FALSE,
                    uSep = ',',
                    uQuote = ''
            )
            usg <- usg[usg$end >= input$dates[1] & usg$start <= input$dates[2],]
            usg
    })
    #Generate model data
    dModel <- eventReactive(input$goButton,{
            prepareModelData(usages(), dWeather())
    })
    #Generation normal weather
    ndWeather <- eventReactive(input$goButton,{
            normalWeather(dWeather(), input$dates[2])
    })
    #Generate a regression model
    regressionModel <- eventReactive(input$goButton, {
            findBestRegression(dModel())
    })
    #Generate prediction data
    predModel <- reactive({
            dm <- dModel()
            bf <- regressionModel()
            nor <- prepareNormalModelData(ndWeather())
            buildNormalModel(nor, bf, input$temps)
    })
    
    rdModel <- eventReactive(input$goButton,{
            rd <- dModel()
            names(rd) <- c("Date",
                           "Days",
                           "Celsius",
                           "Usage",
                           "log(Usage)",
                           "Farenheit",
                           "CDD",
                           "HDD",
                           "Day Count",
                           "Season")
            rd$Celsius < round(rd$Celsius, 2)
            rd$Farenheit < round(rd$Farenheit, 2)
            rd$CDD < round(rd$CDD, 2)
            rd$HDD < round(rd$HDD, 2)
            rd
    })
    
    rndWeather <- eventReactive(input$goButton,{
            rd <- ndWeather()
            names(rd) <- c("Month",
                           "Celsius")
            rd$Farenheit <- rd$Celsius * 9.0/5.0 + 32.0
            rd$CDD <- rd$Farenheit - 65.0
            rd$HDD <- 65.0 - rd$Farenheit
            rd$CDD[rd$CDD<0] <- 0.0
            rd$HDD[rd$HDD<0] <- 0.0
            rd$Celsius < round(rd$Celsius, 2)
            rd$Farenheit < round(rd$Farenheit, 2)
            rd$CDD < round(rd$CDD, 2)
            rd$HDD < round(rd$HDD, 2)
            rd
    })
    

    #Exploration tables
    output$outdModel <- renderDataTable(rdModel(),
                                        options = list(
                                                pageLength = 5,
                                                initComplete = I("function(settings, json) {alert('Done.');}")
                                        )
    )
    output$outndModel <- renderDataTable(rndWeather(),
                                        options = list(
                                                pageLength = 5,
                                                initComplete = I("function(settings, json) {alert('Done.');}")
                                        )
    )
    
    #Output data tables
    output$table <- renderTable({dataInput()})
    output$result <- renderTable({result()})
    output$weatherSample <- renderTable({head(weather(), 10)})
    output$dWeatherSample <- renderTable({head(dWeather(), 10)})
    output$outUsageHistory <- renderTable({usages()})

    
    #Leaflet plot
    output$leafletMap <- renderMap({
            #Get the coordinates
            closest <- dataInput()
            #Generate the map
            map3 <- Leaflet$new()
            map3$setView(c(closest$LOC.LAT,closest$LOC.LON))
            map3$marker(c(closest$LOC.LAT,closest$LOC.LON), 
                       bindPopup = "<p> Your location </p>"
                       )
            map3$marker(c(closest$LAT,closest$LON), 
                       bindPopup = paste("<p>",
                                         closest$NAME,
                                         "</p>")
            )
            map3$set(dom = 'myChart2')
            #show the map
            map3
    }
    )
    
    #Model plot
    output$mdPlot <- renderChart({
            #Prepare the data
            md <- subset(dModel(), select = c(chartDate, avgTemp, avgUsage))
            names(md) <- c("Date", "Temperature", "Consumption")
            md <- melt(subset(md, select = c(Date,Temperature,Consumption)),
                       id.vars = "Date",
                       measure.vars = c("Temperature","Consumption")
                       )
            md$value <- round(md$value, 2)
            #Plot the result
            p1 <- nPlot(value ~ Date,
                        group = 'variable',
                        data = md,
                        type='multiChart'
                        )
            p1$set(multi = list(
                    Consumption = list(type="area", yAxis=1),
                    Temperature = list(type="line", yAxis=2
                                   )
            ))
            p1$chart(color = c('brown', 'blue'))
            p1$xAxis(
                    tickFormat =   "#! 
                        function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));}
                    !#",
                    rotateLabels = -35
            )
            p1$setTemplate(script = system.file(
                    "/libraries/nvd3/layouts/multiChart.html",
                    package = "rCharts"))
            p1$set(title = "Average Daily Usage vs Temperature")
            p1$set(dom = 'mdPlot', width = 800, height = 400)
            #Show the result
            return(p1)
    })
    
    #Motion plot
    output$motionChart <- renderGvis({
            #Prepare model data
            motionModel <- subset(dModel(),
                                  select = c("chartDate",
                                             "usageDays",
                                             "avgUsage",
                                             "avgTemp",
                                             "CDD",
                                             "HDD",
                                             "season"
                                             ))
            motionModel$year <- format(motionModel$chartDate, "%Y")
            names(motionModel) <- c("Date",
                                    "Days",
                                    "Consumption",
                                    "Temperature",
                                    "CDD",
                                    "HDD",
                                    "Season",
                                    "Year"
                                    )
            #Return the model
            return(gvisMotionChart(motionModel,
                                   idvar = "Year",
                                   timevar = "Date",
                                   colorvar = "Season",
                                   sizevar = "Days",
                                   xvar = "Temperature",
                                   yvar = "Consumption"
                                   )
                   )
    })
    
#     #Scatter plot
#     output$mdScatter <- renderChart({
#             p1 <- nPlot(x = "avgTemp", y = "avgUsage",
#                         facet = "season",
#                         data = dModel(),
#                         type="scatterChart")
#             # p1$set(pointSize = 0, lineWidth = 1)
#             p1$set(dom = 'mdScatter')
#             return(p1)
#     })
    
    #Output regression info
    #Regression coefficients
    output$coef <- renderTable({
            fit <- regressionModel()
            summary(fit)$coef
            }
            )
    #R-squared and ajusted R-squared
    output$r_squared <- renderText({
            fit <- regressionModel()
            rsq <- summary(fit)$r.squared
            arsq <- summary(fit)$adj.r.squared
            paste('The R-squared for the best model is ', round(rsq,4),
                  ' and the adjusted R-squared for this model is ', round(arsq,4),
                  '\n',
                  sep = ""
            )
    }
    )
    #Regression model plot
    output$regressionPlot <- renderPlot({
            dm <- dModel()
            bf <- regressionModel()
            predModel <- buildPredictionModel(dm, bf)
            ggplot(predModel, aes(x=chartDate,y=fit)) + 
                    geom_line(color = "blue") +
                    geom_line(aes(x = chartDate, y = avgUsage), color = "red") +
                    scale_colour_manual("", breaks = c("Backcast", "Actual"),
                                        values = c("blue", "red")) +
                    geom_errorbar(aes(ymax = upr, ymin = lwr)) +
                    labs(title = "Backcast Model",
                         x = "Date",
                         y = "Average Daily Consumption",
                         size = "Nitrogen") +
                    theme_minimal(base_size = 12, base_family = "")
    })
    #Prediction model plot
    output$predictionPlot <- renderPlot({
            ggplot(predModel(), aes(x=chartDate,y=fit)) + 
                    geom_line(color = "blue") +
                    geom_errorbar(aes(ymax = upr, ymin = lwr)) +
                    scale_colour_manual("", breaks = c("Prediction", "Actual"),
                                        values = c("black", "red")) +
                    labs(title = "Prediction Model",
                         x = "Date",
                         y = "Average Daily Consumption",
                         size = "Nitrogen") +
                    theme_minimal(base_size = 12, base_family = "") 
    })
    #Residual plot
    output$residualPlots <- renderPlot({
            fit <- regressionModel()
            plotResiduals(fit)
    })
    #Render print the model
    output$regressionSummary <- renderPrint({
            fit <- regressionModel()
            summary(fit)
    })
    #Render print the residuals
    output$regressionResiduals <- renderTable({
            fit <- regressionModel()
            as.data.frame(residuals(fit))
    })
    
#     #Text outputs for conditional panel
#     output$tablesTitle <- renderText({"Data Tables"})
#     output$plotsTitle <- renderText({"Data Exploration"})
#     output$regressionTitle <- renderText({"Regression Results"})
})