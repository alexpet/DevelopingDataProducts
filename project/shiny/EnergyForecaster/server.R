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
    # here, we just print a message to the console
    observeEvent(input$goButton, {
            cat("Showing", input$dates, "rows\n")
    })
    
    dataInput <- eventReactive(input$goButton,{
            loc <- gGeoCode(input$address)
            findClosestStation(loc, st, input$dates[1], input$dates[2])
    })
    
    result <- eventReactive(input$goButton,{
            getWeather(dataInput(), input$dates[1], input$dates[2])
    })
    
    weather <- eventReactive(input$goButton, {
            r <- result()
            w <- loadWeather(r)
            w <- processWeather(w)
            w
    })
    
    dWeather <- eventReactive(input$goButton, {
            dw <- dailyWeather(weather())
            dw[dw$DATE >= input$dates[1] & dw$DATE <= input$dates[2],]
    })
    
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
    
    dModel <- eventReactive(input$goButton,{
            prepareModelData(usages(), dWeather())
    })
    
    regressionModel <- eventReactive(input$goButton, {
            findBestRegression(dModel())
    })
    
    output$outddress <- renderText(input$address)
    output$outgeo <- renderText(input$geo)
    
    output$table <- renderTable({dataInput()})
    
    output$result <- renderTable({result()})
    
    output$weatherSample <- renderTable({head(weather(), 10)})
    output$dWeatherSample <- renderTable({head(dWeather(), 10)})
    
    
    output$outUsageHistory <- renderTable({usages()})
    
    output$outdModel <- renderTable({dModel()})
    
    
    output$w <- renderPlot({
            with(dWeather(),
                 plot(TEMP ~ DATE)
            )
    })
    
    output$u <- renderPlot({
            u <- usages()
            plot(u$usage ~ u$chartDate)
    })
    
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
    
    output$mdScatter <- renderChart({
            p1 <- nPlot(x = "avgTemp", y = "avgUsage",
                        facet = "season",
                        data = dModel(),
                        type="scatterChart")
            # p1$set(pointSize = 0, lineWidth = 1)
            p1$set(dom = 'mdScatter')
            return(p1)
    })
    
    output$coef <- renderTable({
            fit <- regressionModel()
            summary(fit)$coef
            }
            )
    
    output$r_squared <- renderText({
            fit <- regressionModel()
            rsq <- summary(fit)$r.squared
            arsq <- summary(fit)$adj.r.squared
            paste('The R-squared for this best model is ', round(rsq,4),
                  ' and the adjusted R-squared for this model is ', round(arsq,4),
                  sep = ""
            )
    }
    )
    
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
    
})