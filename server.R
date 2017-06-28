library(shiny)
library(dplyr)
library(xlsx)
library(plotly)

source("file-setup.R")
source("graph-alteration.R")
source("graph-analysis.R")

shinyServer(function(input, output) {
   output$averageScans <- reactive({
     filename = function() {
       paste("average-scans", '.csv', sep='')
     }
     content = function(file) {
       if (averageScans == currentAverageScansNameThing) {
         return(averageScans)
       }
       averageScans(dataset)
       write.csv(dataSetInput(), file)
     }
   })
   
   output$scanPlot <- renderPlot({
    if (!is.null(input$scans)) {
      scan.data <- scanGraphData(input$scans)
      if (!is.null(input$sparklink)) {
        # graph labels are set, else, default values of sample 1, sample 2, sample 3, etc
      }
    }
   })
   
   output$ampPlot <- renderPlot({
     if (!is.null(input$amplog)) {
        amp.data <- ampGraphData(input$amplog)
     }
   })
})