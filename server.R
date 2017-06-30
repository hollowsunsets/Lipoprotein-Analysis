library(shiny)
library(shinyjs)
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
   
   # Retrieves and reads in and processes scans dataset.
   # To-Do: Add check to ensure it is an accepted format.
   scans.data <- reactive({
     infile <- input$scansData
     if (is.null(infile)) {
       return(NULL)
     }
     if (!is.null(input$sparklinkData)) {
       scanGraphData(read.csv(infile$datapath, stringsAsFactors = FALSE), sparklink.data) # if the sparklink file was provided, 
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
     
   output$sampleControl <- renderUI({
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scans.data()), selected = names(scans.data())[1])
   })
   
   current_scan_data <- reactive({
     if (any(input$sampleSelect %in% names(scans.data()))) {
       
     }
   })
   
   output$removeScans <- renderUI({
     scan.state <- current_scan_data() %>% select(startsWith("scan"))
     selectInput("scansToRemove", label = "Remove a Scan", 
                 choices = colnames(scan.state),
                 selected = "None")
   })
   
   output$addScans <- renderUI({
     scan.state <- current_scan_data() %>% select(startsWith("scan"))
     curr.scan.names <- colnames(scan.state) 
     selectInput("scansToAdd", label = "Add a Scan",
                 choices = curr.scan.names[-scansDropped],
                 selected = "None")
   })

   

  # ------------------------------------------------------------------------------------------------------ #
   # Generates the graph that visualizes the scan data 
   

    
   output$scanPlot <- renderPlotly({
       if (!is.null(input$scans)) { # If the scans file was provided, then plot will be generated
         # Plot dataset 
         
         p2 <- plot_ly(current_scan_data()), y = ~`Counts..3`, x = ~`Diameter..1`, name = 'scan1', type = 'scatter', mode = 'lines') %>%
           add_trace(y = ~`Counts..4`, name = 'scan2') %>%
           add_trace(y = ~`Counts..5`, name = 'scan3') %>%
           add_trace(y = ~`Counts..6`, name= 'scan4')
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