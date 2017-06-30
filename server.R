library(shiny)
library(dplyr)
library(xlsx)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
source("graph-analysis.R")

shinyServer(function(input, output) {
  
  # Retrieves and reads in the given data files 
   sparklink.data <- reactive({
     infile <- input$sparklinkData
     if (is.null(infile)) {
       return(NULL)
     }
     read.csv(infile$datapath, stringsAsFactors = FALSE)
   })
   
   scans.data <- reactive({
     infile <- input$scansData
     if (is.null(infile)) {
       return(NULL)
     }
     read.csv(infile$datapath, stringsAsFactors = FALSE)
   })
   
   amplog.data <- reactive({
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     read.xlsx(infile$datapath, sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE)
   })
  # ------------------------------------------------------------------------------------------------------ #
   # Generates the graph that visualizes the scan data 
   output$scanPlot <- renderPlot({
       if (!is.null(input$scans)) { # If the scans file was provided, then plot will be generated
         if (!is.null(input$sparklinkData)) {
           scan.graph.data <- scanGraphData(scans.data, sparklink.data) # if the sparklink file was provided, 
                                                                        # then labels will be set to sparklink labels
         } else {
           scan.graph.data <- scanGraphData(scans.data) # else, set to default of "sample 1, sample 2, ..., etc"
         }
       } else {
         return(NULL)
       }
   })
   
   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     print("entered renderPlot for ampPlot")
     if (!is.null(input$amplog)) {
       amp.data <- ampGraphData(amplog.data)
       amp.plot <- p 
     }
   })
   
   # Generates average scan file to be returned through download button
   output$averageScans <- reactive({
     filename = function() {
       paste("average-scans", '.csv', sep='')
     }
     content = function(file) {
       if (averageScans == currentAverageScansNameThing) { # if the current average scans dataset is what we want, we shouldn't process it again
         return(averageScans)
       }
       averageScans(dataset)
       write.csv(dataSetInput(), file)
     }
   })
   
  
})