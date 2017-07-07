 # -------------------------------------------------- Dependencies -------------------------------------------------- #
library(shiny)
library(shinyjs)
library(dplyr)
library(xlsx)
library(ggplot2)


options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
# source("graph-analysis.R")

shinyServer(function(input, output, session) {
  # ----------------------------------------------- Global Variables --------------------------------------------------#
  scan.data <- NULL
  scan.timestamps <- NULL
  amp.data <- NULL
  curr.scan.state <- NULL 
  curr.amp.state <- NULL
  # -------------------------------------- General Data Processing and Retrieval ----------------------------------- #

    # Retrieves and reads in the given data files 
   sparklink.data <- reactive({
     infile <- input$sparklinkData
     if (is.null(infile)) {
       return(NULL)
     }
     sparklink.raw.data <- read.csv(infile$datapath, stringsAsFactors = FALSE)
     return(sparklink.raw.data)
   })
   
   # Retrieves and reads in and processes scans dataset.
   # To-Do: Add check to ensure it is an accepted format. 
   scans.data <- reactive({
     infile <- input$scansData
     if (is.null(infile)) {
       return(NULL)
     }
     raw.file <- read.csv(infile$datapath, stringsAsFactors = FALSE, na.strings = c("", NA)) 
     # Note: na.strings = c("", NA) is necessary to read timestamps in correctly
     if (!is.null(input$sparklinkData)) {
       scan.data <<- scanGraphData(raw.file, sparklink.data()) # if the sparklink file was provided,
       scan.timestamps <<- scanTimeStamps(raw.file, sparklink.data())
     } else {
       scan.data <<- scanGraphData(raw.file) # else, set to default of "sample 1, sample 2, ..., etc"
       scan.timestamps <<- scanTimeStamps(raw.file, sparklink.data())
     }
     return(scan.data)
   })
   
   amplog.data <- reactive({
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     amp.data <<- ampGraphData(read.xlsx(infile$datapath, sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE))
   })
     
   # ----------------------------------------- Scan Interaction Functions ---------------------------------------- #
   # NOTE: Ordering of function definitions matters - Shiny renders as it reads in, and the various components are defined
   # such that dependencies will exist before they are needed (nothing will be called when it doesn't exist yet)
  
   # Defines behavior for file control button, which hides and shows the 
   # file controls when clicked.  
    observeEvent(input$toggleFileControls, {
     toggle("file-selection-controls")
   })
   
    # Defines behavior for the sample selection dropdown menu, which 
    # allows users to select which sample they wish to see visualized. 
   output$sampleControl <- renderUI({
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scan.data), selected = names(scan.data[1]))
   })
   
   # Updates the names stored in the sample selection dropdown menu
   # if a Sparklink file has been passed in. 
   observe({
     if (!is.null(input$sparklinkData)) {
       sparklink.data <- sparklink.data()
       sample.names <- sparklink.data %>% select(Sample.Name)
       updateSelectInput(session, "sampleSelect", choices = sample.names[,1])
     }
   })
   
   current_scan_data <- reactive({
     if (any(input$sampleSelect %in% names(scan.data))) {
       # Return the dataframe that corresponds with input$sampleSelect 
       # (i.e, graph.data$`std1`, which is the scan data for the std1 sample)
       if (input$customSmooth > 0.01) {
         return(applyLoessSmooth(scan.data[[input$sampleSelect]], as.numeric(input$customSmooth))) 
       } else {
         return(scan.data[[input$sampleSelect]])
       }
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
     amp.data <<- amplog.data()
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
   
   output$smoothControl <- renderUI({
     tagList(
        p("Enter a number (n > 0.0) to represent the span percent by which you would like to smooth the graph."),
        p("i.e: 0.10 = 10% smoothing span"),
        textInput("customSmooth", "Enter Smoothing Span", "0.05"),
        p("Click to optimize the smoothing to minimize the SSE (sum of squared errors)."),
        actionButton("optimizeSmooth", "Optimize Smooth")
     )
   })
   
   observeEvent(input$optimizeSmooth, {
     
   })

  
   observeEvent(input$customSmooth, {
     curr.scan.state <<- current_scan_data()
     if (input$customSmooth > 0.01) {
       loess.graph.data <- applyLoessSmooth(curr.scan.state, as.numeric(input$customSmooth))
       curr.scan.state <<- loess.graph.data
     }
   })
  
   
   
  # ---------------------------------- Amplog Interactions Functions ----------------------------- #
   # Add any desired modifications to amplog data here
   current_amp_data <- reactive({
     return(amp.data)
   })
   
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   
   # To-Do: Figure out how to make it so this can graph a dynamic number of scans, rather than being hard-coded for 4 scans
   output$scanPlot <- renderPlot({
     curr.scan.state <<- current_scan_data()
     if (!is.null(input$scansData)) { # If the scans file was provided, then plot will be generated
       scan.plot <- ggplot(data=curr.scan.state, aes(sample.diameters)) + 
         geom_line(aes(color="scan 1",y=curr.scan.state[[1]])) + 
         geom_line(aes(color="scan 2",y=curr.scan.state[[2]])) + 
         geom_line(aes(color="scan 3",y=curr.scan.state[[3]])) +
         geom_line(aes(color="scan 4",y=curr.scan.state[[4]])) + 
         xlab("Diameters (nm)") +
         ylab("Concentration (dN#/cm^2)") +
         ggtitle(paste0(input$sampleSelect)) + 
            theme(plot.title = element_text(hjust = 0.5)) # Centers graph title
       return(scan.plot)
     } else {
       return(NULL)
     }
   })
   
   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     curr.amp.state <<- current_amp_data()
     if (!is.null(input$amplog)) {
       
       return(amp.plot)
     } else {
       return(NULL)
     }
   })
   

})