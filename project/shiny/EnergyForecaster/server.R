# server.R

library(lubridate)
library(quantmod)
library(RCurl)
library(RJSONIO)
library(rCharts)
library(zoo)
source("helpers.R")

shinyServer(function(input, output) {
    
    dataInput <- reactive({
            loc <- gGeoCode(input$address)
            findClosestStation(loc, st, input$dates[1], input$dates[2])
    })
    
    result <- reactive({
            getWeather(dataInput(), input$dates[1], input$dates[2])
    })
    
    usages <- reactive({
            usagesData <- input$usageHistory
            if (is.null(usagesData))
                    return(NULL)
            readUsageHistory(
                    uFilePath = usagesData$datapath,
                    uHeader = FALSE,
                    uSep = ',',
                    uQuote = ''
            )
    })
    
    output$outddress <- renderText(input$address)
    output$outgeo <- renderText(input$geo)
    
    output$table <- renderTable({dataInput()})
    
    output$result <- renderTable({result()})
    
    output$outUsageHistory <- renderTable({usages()})
    
    
    output$w <- renderPlot({
            weather <- loadWeather(result())
            weather <- processWeather(weather)
            dw <- dailyWeather(weather)
            dw <- dw[dw$DATE >= input$dates[1] &
                                  dw$DATE <= input$dates[2],]
            if(dim(dw)[1] == 0)
                    return(NULL)
            
            with(dw,
                 plot(TEMP ~ DATE)
                 )
            })
    
    output$u <- renderPlot({
            u <- usages()
#             u <- u[u$end >= input$dates[1] &
#                                  u$start <= input$dates[2],]
            plot(u$usage ~ u$chartDate)
    })
    
})