# server.R

library(lubridate)
library(quantmod)
library(RCurl)
library(RJSONIO)
library(rCharts)
library(zoo)
library(ggplot2)
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
    #Generate a regression model
    regressionModel <- eventReactive(input$goButton, {
            findBestRegression(dModel())
    })
    
    #Output data tables
    output$table <- renderTable({dataInput()})
    output$result <- renderTable({result()})
    output$weatherSample <- renderTable({head(weather(), 10)})
    output$dWeatherSample <- renderTable({head(dWeather(), 10)})
    output$outUsageHistory <- renderTable({usages()})
    output$outdModel <- renderTable({dModel()})
    
    #Output plots
    #Weather plot
    output$w <- renderPlot({
            with(dWeather(),
                 plot(TEMP ~ DATE)
            )
    })
    #Usage plot
    output$u <- renderPlot({
            u <- usages()
            plot(u$usage ~ u$chartDate)
    })
    #Model plot
    output$mdPlot <- renderChart({
            md <- dModel()
            md <- melt(subset(md, select = c(chartDate,avgTemp,avgUsage)),
                       id.vars = "chartDate",
                       measure.vars = c("avgTemp","avgUsage")
                       )
            md$chartDate <- as.Date(md$chartDate, format = "%Y-%m-%d")
            p1 <- nPlot(value ~ chartDate,
                        group = "variable",
                        data = md,
                        type="lineChart")
            p1$chart(color = c('brown', 'blue'))
            p1$xAxis(
                    tickValues = "#! [ 15995, 16622 ] !#"
            )  
            p1$set(dom = 'mdPlot')
            
            return(p1)
    })
    #Scatter plot
    output$mdScatter <- renderChart({
            p1 <- nPlot(x = "avgTemp", y = "avgUsage",
                        facet = "season",
                        data = dModel(),
                        type="scatterChart")
            # p1$set(pointSize = 0, lineWidth = 1)
            p1$set(dom = 'mdScatter')
            return(p1)
    })
    
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
            paste('The R-squared for this best model is ', round(rsq,4),
                  ' and the adjusted R-squared for this model is ', round(arsq,4),
                  '\n',
                  sep = ""
            )
    }
    )
    #Regression prediction model plot
    output$regressionPlot <- renderPlot({
            dm <- dModel()
            bf <- regressionModel()
            predModel <- buildPredictionModel(dm, bf)
            ggplot(predModel, aes(x=chartDate,y=fit)) + 
                    geom_point() +
                    geom_point(aes(x = chartDate,
                                   y = avgUsage), color = "red") +
                    geom_errorbar(aes(ymax = upr, ymin = lwr)) +
                    scale_colour_manual("", breaks = c("Prediction", "Actual"),
                                        values = c("black", "red")) +
                    labs(title = "Prediction Model")
    })
    #Render print the model
    output$regressionSummary <- renderPrint({
            fit <- regressionModel()
            summary(fit)
    })
    
    #Text outputs for conditional panel
    output$tablesTitle <- renderText({"Data Tables"})
    output$plotsTitle <- renderText({"Data Exploration"})
    output$regressionTitle <- renderText({"Regression Results"})
})