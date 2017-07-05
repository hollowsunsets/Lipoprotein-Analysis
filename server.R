library(shiny)
library(shinyjs)
library(dplyr)
library(xlsx)
library(ggplot2)


options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
# source("graph-alteration.R")
# source("graph-analysis.R")

shinyServer(function(input, output) {
  scan.data <- 0
  curr.scan.state <- 0
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
       scan.data <<- scanGraphData(read.csv(infile$datapath, stringsAsFactors = FALSE), sparklink.data()) # if the sparklink file was provided,
     } else {
       scan.data <<- scanGraphData(read.csv(infile$datapath, stringsAsFactors = FALSE)) # else, set to default of "sample 1, sample 2, ..., etc"
     }
     return(scan.data)
   })
   
   amplog.data <- reactive({
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     show("amplog-interactions")
     ampGraphData(read.xlsx(infile$datapath, sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
   })
     
   # ----------------------------------------- Scan Interaction Functions ---------------------------------------- #
   output$sampleControl <- renderUI({
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scan.data), selected = names(scan.data[1]))
   })
   
   
   current_scan_data <- reactive({
     if (any(input$sampleSelect %in% names(scan.data))) {
       # return the dataframe that corresponds with input$sampleSelect
       return(scan.data[[input$sampleSelect]])   # should be something like graph.data$`std 1` which will return corresponding data frame
     } else {
       return(scan.data[[1]])
     }
   })

   current_average_data <- reactive({
     if (is.null(curr.scan.state)) {
       return(NULL)
     }
     averageScans(curr.scan.state)
   })
   
   
   observeEvent(input$scansData, {
     scan.data <<- scans.data()
     toggle("scan-interactions")
     toggle("scanPlot")
     toggle("scan-message")
   }, once = TRUE)
   
   observeEvent(input$amplogData, {
     toggle("amplog-interactions")
     toggle("ampPlot")
     toggle("amp-message")
   }, once = TRUE)
   
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
     curr.scan.state <<- current_scan_data()
     scan.state <- select(curr.scan.state, starts_with("scan"))
     selectInput("scansToRemove", label = "Remove a Scan", 
                 choices = colnames(scan.state))
   })
   
   output$addScans <- renderUI({
     curr.scan.state <<- current_scan_data()
     scan.state <- select(curr.scan.state, starts_with("scan"))
     curr.scan.names <- colnames(scan.state)[-scansDropped]
     if (length(curr.scan.names) == 0) {
       curr.scan.names <- c("None")
     }
     selectInput("scansToAdd", label = "Add a Scan",
                 choices = curr.scan.names)
   })
   
   observeEvent(input$optimizeSmooth, {
     
   })
   
   observeEvent(input$customSmooth, {
     prev.dataset <- curr.scan.state[1:4]
     
   })
   
  # ---------------------------------- Amplog Interactions Functions ----------------------------- #

   
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   

    
   output$scanPlot <- renderPlot({
       curr.scan.state <<- current_scan_data()
       if (!is.null(input$scansData)) { # If the scans file was provided, then plot will be generated
         scan.plot <- ggplot(data=curr.scan.state, aes(sample.diameters)) + 
                      geom_line(aes(color="scan 1",y=curr.scan.state[[1]])) + 
                      geom_line(aes(color="scan 2",y=curr.scan.state[[2]])) + 
                      geom_line(aes(color="scan 3",y=curr.scan.state[[3]])) +
                      geom_line(aes(color="scan 4",y=curr.scan.state[[4]]))
         return(scan.plot)
       } else {
         return(NULL)
       }
   })
   
   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     print("entered renderPlot for ampPlot")
     if (!is.null(input$amplog)) {
       amp.plot <- plot()
       return(amp.plot)
     } else {
       return(NULL)
     }
   })
})