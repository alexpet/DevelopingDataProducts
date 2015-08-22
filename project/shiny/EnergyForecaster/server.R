# server.R

library(lubridate)
library(quantmod)
library(RCurl)
library(RJSONIO)
source("helpers.R")

shinyServer(function(input, output) {
    
    dataInput <- reactive({
            loc <- gGeoCode(input$address)
            findClosestStation(loc, st, input$dates[1], input$dates[2])
    })
    
    result <- reactive({
            getWeather(dataInput(), input$dates[1], input$dates[2])
    })
    
#     weather <- loadWeather()
#     weather <- processWeather(weather)
    
    output$outddress <- renderText(input$address)
    output$outgeo <- renderText(input$geo)
    
    output$table <- renderTable({dataInput()})
    
    output$result <- renderTable({result()})
    
#     output$w <- renderPlot({
#             plot(weather$DATE, weather$TEMP, main = "Temperature readings",
#                  ylab = "Temperature (Degrees C)",
#                  xlab = "Month",
#                  col = "grey")
#     })
    
    output$start <- renderText(as.numeric(format(input$dates[1], "%Y")))
    output$end <- renderText(as.numeric(format(input$dates[2], "%Y")))
})