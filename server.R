library(shiny)
library(dplyr)
library(xlsx)
library(ggplot2)

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
     output$scanPlotDone <- renderUI({
       tags$input(type="hidden", value = "TRUE")
       if (!is.null(input$scans)) {
         scan.graph.data <- scanGraphData(scans.data)
         if (!is.null(input$sparklink)) {
           graph.labels <- getSampleLabels(sparklink.data)
         } else {
           graph.labels <- c(paste0("sample ", 1:length(scans.data)))
         }
       }
     })
     
   })
   
   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     if (!is.null(input$amplog)) {
       amp.data <- ampGraphData(amplog.data)
       amp.plot <- p 
     }
   })
   
   output$scanLabels <- renderUI({
   })
   
   output$addScans <- renderUI({ # these should really only appear when the graph is already rendered
     if (is.null()) return(NULL)
     selectInput("addScans", label = h4("Add a Scan"), choices = scansDropped)
   })
   
   output$removeScans <- renderUI({
     if (is.null(scans.data())) return(NULL)
     selectInput("removeScans", label= h4("Remove a Scan"), choices = scanChoices) # get scan choices from graph name
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