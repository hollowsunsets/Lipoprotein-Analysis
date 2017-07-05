library(shiny)
library(shinyjs)
library(dplyr)
library(xlsx)
library(plotly)


options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
# source("graph-alteration.R")
# source("graph-analysis.R")

shinyServer(function(input, output) {
  
  # -------------------------------------- General Data Processing and Retrieval ----------------------------------- #

    # Retrieves and reads in the given data files 
   sparklink.data <- reactive({
     infile <- input$sparklinkData
     if (is.null(infile)) {
       return(NULL)
     }
     read.csv(infile$datapath, stringsAsFactors = FALSE)
   })
   
   # Retrieves and reads in and processes scans dataset.
   # To-Do: Add check to ensure it is an accepted format.
   scans.data <- reactive({
     infile <- input$scansData
     if (is.null(infile)) {
       return(NULL)
     }
     if (!is.null(input$sparklinkData)) {
       print("sparklink dataset was provided, labels should be replaced")
       print(sparklink.data)
       scanGraphData(read.csv(infile$datapath, stringsAsFactors = FALSE), sparklink.data()) # if the sparklink file was provided, 
     } else {
       scanGraphData(read.csv(infile$datapath, stringsAsFactors = FALSE)) # else, set to default of "sample 1, sample 2, ..., etc"
     }
   })
   
   amplog.data <- reactive({
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     ampGraphData(read.xlsx(infile$datapath, sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
   })
     
   # ----------------------------------------- Scan Interaction Functions ---------------------------------------- #
   output$sampleControl <- renderUI({
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scans.data()), selected = names(scans.data())[1])
   })
   
   current_scan_data <- reactive({
     if (any(input$sampleSelect %in% names(scans.data()))) {
       # return the dataframe that corresponds with input$sampleSelect
       scans.data()[[input$sampleSelect]]   # should be something like graph.data$`std 1` which will return corresponding data frame
     }
   })
   
   current_average_data <- reactive({
     if (is.null(current_scan_data())) {
       return(NULL)
     }
     averageScans(current_scan.data())
   })
   
   # Generates average scan file to be returned through download button
   output$averageScans <- reactive({
     filename = function() {
       paste("average-scans", '.csv', sep='')
     }
     content = function(file) {
       write.csv(current_average_data(), file)
     }
   })
   
   output$removeScans <- renderUI({
     curr.scan.data <- current_scan_data()
     scan.state <- select(curr.scan.data, starts_with("scan"))
     selectInput("scansToRemove", label = "Remove a Scan", 
                 choices = colnames(scan.state))
   })
   
   output$addScans <- renderUI({
     curr.scan.data <- current_scan_data()
     scan.state <- select(curr.scan.data, starts_with("scan"))
     curr.scan.names <- colnames(scan.state)[-scansDropped]
     if (length(curr.scan.names) == 0) {
       curr.scan.names <- c("None")
     }
     selectInput("scansToAdd", label = "Add a Scan",
                 choices = curr.scan.names)
   })

  # ---------------------------------- Amplog Interactions Functions ----------------------------- #

   
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   

    
   output$scanPlot <- renderPlotly({
       if (!is.null(input$scans)) { # If the scans file was provided, then plot will be generated
         # Plot dataset 
         
         scanp <- plot_ly(current_scan_data(), y = ~scan1, x = ~sample.diameters, name = 'scan1', type = 'scatter', mode = 'lines') %>%
           add_trace(y = ~scan2, name = 'scan2') %>%
           add_trace(y = ~scan3, name = 'scan3') %>%
           add_trace(y = ~scan4, name= 'scan4')
         return(scanp)
       } else {
         return(NULL)
       }
   })

   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     print("entered renderPlot for ampPlot")
     if (!is.null(input$amplog)) {
     }
   })
   
   
   
  
})