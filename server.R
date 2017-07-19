 # -------------------------------------------------- Dependencies -------------------------------------------------- #
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(lubridate)


options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
# source("graph-analysis.R")

shinyServer(function(input, output, session) {
  # ----------------------------------------------- Global Variables --------------------------------------------------#
  scan.timestamps <- NULL
  timeControlsEnabled <- FALSE
  # -------------------------------------- General Data Processing and Retrieval ----------------------------------- #

    # Retrieves and reads in the given data files 
   sparklink.data <- reactive({
     print("Currently reading in raw Sparklink file")
     infile <- input$sparklinkData
     if (is.null(infile)) {
       return(NULL)
     }
     sparklink.raw.data <- read.csv(infile$datapath, stringsAsFactors = FALSE, header = FALSE)
     return(sparklink.raw.data)
   })
   
   # Retrieves and reads in and processes scans dataset.
   # To-Do: Add check to ensure it is an accepted format. 
   scans.data <- reactive({
     print("Currently reading in raw scans file")
     scan.data <- NULL
     infile <- input$scansData
     if (!is.null(infile)) {
       raw.file <- read.csv(infile$datapath, stringsAsFactors = FALSE, na.strings = c("", NA)) 
       # Note: na.strings = c("", NA) is necessary to read timestamps in correctly
       if (!is.null(input$sparklinkData)) {
         scan.data <- scanGraphData(raw.file, sparklink.data()) # if the sparklink file was provided,
         scan.timestamps <<- scanTimeStamps(raw.file, sparklink.data())
       } else {
         scan.data <- scanGraphData(raw.file) # else, set to default of "sample 1, sample 2, ..., etc"
         scan.timestamps <<- scanTimeStamps(raw.file)
       }
     }
     return(scan.data)
   })
   
   amplog.data <- reactive({
     print("Currently reading in raw amperage file")
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     # Note: Shiny keeps temporary files w/o extensions. read_excel needs an .xlsx extension to run. 
     # Following lines are a workaround to use readxl with Shiny by making a copy of the input file with the extension.
     # Watch for resolution for #85 on Github for tidyverse/readxl
     file.copy(infile$datapath, paste(infile$datapath, ".xlsx", sep="")) 
     raw.amp.file <- readxl::read_excel(paste(infile$datapath, ".xlsx", sep=""), col_names = FALSE)
     trimmed.amp.file <- raw.amp.file[complete.cases(raw.amp.file),]
     print("Amp file was recognized as not null. Processing.")
     return(ampGraphData(trimmed.amp.file))
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
     print(colnames(scans.data()))
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scans.data()), selected = names(scans.data())[1])
   })
   
   # Updates the names stored in the sample selection dropdown menu
   # if a Sparklink file has been passed in. 
   observe({
     if (!is.null(input$sparklinkData)) {
       sparklink.data <- sparklink.data()
       sample.names <- sparklink.data[,3]
       print(sample.names)
       updateSelectInput(session, "sampleSelect", choices = sample.names)
     }
   })
   
   # Should only update when the sample being visualized is changed.
   current_sample_data <- reactive({
     current.sample.set <- scans.data()
     if (any(input$sampleSelect %in% names(current.sample.set))) {
       return(current.sample.set[[input$sampleSelect]])
     } else {
       return(current.sample.set[[1]])
     }
   })
   
   current_scan_data <- reactive({
     selected.sample.data <- current_sample_data()
     altered.sample.data <- selected.sample.data
       # Return the dataframe that corresponds with input$sampleSelect 
       # (i.e, graph.data$`std1`, which is the scan data for the std1 sample)
       if (!(is.null(selected.sample.data))) {
         if (!(is.null(input$customSmooth)) && input$customSmooth > 0.01) {
            altered.sample.data <- applyLoessSmooth(altered.sample.data, as.numeric(input$customSmooth))
         }
         if (input$scansToAdd != "None" && !is.null(input$scansToAdd)) {
            altered.sample.data <- addBackScan(altered.sample.data, input$scansToAdd, selected.sample.data)
         }
         if (input$scansToRemove != "None"  && !is.null(input$scansToRemove)) {
            altered.sample.data <<- dropScan(altered.sample.data, input$scansToRemove)
         }
       }
     return(altered.sample.data)
   })

   current_average_data <- reactive({
     if (is.null(curr.scan.state)) {
       return(NULL)
     }
     averageScans(curr.scan.state)
   })
   
   observeEvent(input$scansData, {
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
     selected.sample.data <- current_sample_data()
     if (!is.null(selected.sample.data)) {
       scan.names <- colnames(select(selected.sample.data, starts_with("scan")))
       scan.names[length(scan.names) + 1] <- "None"
       selectInput("scansToRemove", label = "Remove a Scan", 
                   choices = scan.names, selected = "None")
     }
   })
   
   output$addScans <- renderUI({
       selectInput("scansToAdd", label = "Add a Scan",
                   choices = scansDropped, selected = "None")
   })
   
   
   output$smoothControl <- renderUI({
     tagList(
        p("Enter a number (n > 0.0) to represent the span percent by which you would like to smooth the graph."),
        p("i.e: 0.10 = 10% smoothing span"),
        textInput("customSmooth", "Enter Smoothing Span", "0.05")
     )
   })
   
   
   
  # ---------------------------------- Amplog Interactions Functions ----------------------------- #
   output$timeControl <- renderUI({
     amp.range <- amplog.data()
     print("Amp range data is done being accessed")
     start.time <- amp.range$X0[1]
     print("Start time for the slider:")
     print(start.time)
     end.time <- tail(amp.range$X0, n = 1)
     print("End time for the slider:")
     print(end.time)
     print("Time control input should be rendered now")
     print("Default end time on the slider:")
     print(start.time + (12 * 60))
     sliderInput("range", "Time range:",
                 min = start.time, 
                 max = end.time, 
                 value = c(start.time, start.time + (12 * 60)))
   })
   
   
   # Add any desired modifications to amplog data here
   current_amp_data <- reactive({
     print("Currently processing state for amp data")
     current.amp.set <- amplog.data()
     altered.amp.data <- current.amp.set
     print(timeControlsEnabled)
     print(is.null(input$timeControl))
     print(is.null(input$timeControls))
     if (timeControlsEnabled) {
       print("Time controls are enabled. Amp graph will be set to user input.")
       print(input$timeControl)
       altered.amp.data <- intervalAmperageData(altered.amp.data,
                                                input$timeControl[[1]],
                                                input$timeControl[[2]])
       print("Amp graph data is done being altered.")
       
       return(altered.amp.data)
     }
     if (!(is.null(input$sampleSelect)) && !(is.null(scan.timestamps))) {
       print("Sample data is present. Amp data will be set to corresponding scan timestamps.")
       current.sample.timestamps <- scan.timestamps %>% filter(sample.name == input$sampleSelect)
       altered.amp.data <- intervalAmperageData(altered.amp.data, 
                                                current.sample.timestamps$start.time,
                                                current.sample.timestamps$end.time)
       print("Amp graph data is done being altered.")
       
       return(altered.amp.data)
     }
     # default modification
     print("Amp graph will be set to default setting.")
     print("Start time for the data:")
     print(altered.amp.data$X0[1])
     print("End time for the data:")
     print(altered.amp.data$X0[1] + (12 * 60))
     altered.amp.data <- intervalAmperageData(altered.amp.data, altered.amp.data$X0[1], 
                                              altered.amp.data$X0[1] + (12 * 60))
     print("Amp graph data is done being altered.")
     return(altered.amp.data)
   })
   
   
   observeEvent(input$toggleTimeControls, {
     print("Time controls are toggled")
     timeControlsEnabled <<- !timeControlsEnabled
     toggle("amp-time-controls")
     current.amp.data.reload <- current_amp_data()
     print("Amp log data was reloaded.")
   })
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   
   output$scanPlot <- renderPlot({
     selected.scan.data <- current_scan_data()
     if (!is.null(input$scansData) && !is.null(selected.scan.data)) { # If the scans file was provided, then plot will be generated
      scan.plot.data <- melt(selected.scan.data, id.vars = "sample.diameters", variable.name = 'series') 
      scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
         geom_line(aes(colour = series)) +
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
     print("Graph for amperage data is being created.")
     print("Data for graph is being retrieved.")
     selected.amp.data <- current_amp_data()
     print("Graph data is done being retrieved.")
     print(is.null(selected.amp.data))
     print(is.null(input$amplogData))
     print(!is.null(input$amplogData) && !is.null(selected.amp.data))
     if (!is.null(input$amplogData) && !is.null(selected.amp.data)) {
       print("All components for the graph are present.")
       amp.plot <- ggplot(data = selected.amp.data) +
                       geom_line(aes(x = X0, y= X2, group = 1)) + 
                       xlab("Time (PST, Standard Time)") +
                       ylab("Amperage (amp)") +
                       ggtitle(paste0("Amperage Data")) + 
                       theme(plot.title = element_text(hjust = 0.5))
       print("Graph is done being created. Should be rendering now.")
       return(amp.plot)
     } else {
       print("Graph returned NULL for some reason.")
       return(NULL)
     }
   })
   

})