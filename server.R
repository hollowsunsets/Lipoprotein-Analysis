 # -------------------------------------------------- Dependencies -------------------------------------------------- #
library(shiny)
library(shinyjs)
library(dplyr)
library(xlsx)
library(ggplot2)
library(reshape2)

options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
# source("graph-analysis.R")

shinyServer(function(input, output, session) {
  # ----------------------------------------------- Global Variables --------------------------------------------------#
  scan.timestamps <- NULL

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
     print("General scan data is recognized. ")
     scan.data <- NULL
     infile <- input$scansData
     print(infile)
     print(input$scansData)
     if (!is.null(infile)) {
       print(!is.null(infile))
       print("Raw file is now being read in, now that the file path has a file to process.")
       raw.file <- read.csv(infile$datapath, stringsAsFactors = FALSE, na.strings = c("", NA)) 
       print(raw.file)
       # Note: na.strings = c("", NA) is necessary to read timestamps in correctly
       if (!is.null(input$sparklinkData)) {
         print("Sparklink file was provided. Labels will be appended to the returned file.")
         scan.data <- scanGraphData(raw.file, sparklink.data()) # if the sparklink file was provided,
         print(scan.data)
         scan.timestamps <<- scanTimeStamps(raw.file, sparklink.data())
       } else {
         print("No sparklink file provided. Default labels will be generated accordingly.")
         scan.data <- scanGraphData(raw.file) # else, set to default of "sample 1, sample 2, ..., etc"
         print(scan.data)
         scan.timestamps <<- scanTimeStamps(raw.file)
       }
     }
     return(scan.data)
   })
   
   amplog.data <- reactive({
     infile <- input$amplogData
     if (is.null(infile)) {
       return(NULL)
     }
     return(ampGraphData(read.xlsx(infile$datapath, sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors = FALSE)))
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
     print("Sample control is being rendered.")
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scans.data()), selected = names(scans.data())[1])
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
   
   # Should only update when the sample being visualized is changed.
   current_sample_data <- reactive({
     print("The desired sample from the overall scans dataset is being selected.")
     current.sample.set <- scans.data()
     print("Let's hope that current.sample.set is not null.")
     print(current.sample.set)
     if (any(input$sampleSelect %in% names(scan.data))) {
       print("The corresponding sample from the selected sample (or the default) will be chosen.")
       return(current.sample.set[[input$sampleSelect]])
     } else {
       print("Default dataset (the first in the set) is being returned for some reason. Names do not match, you better check why.")
       print("Here's the non-matching inputs:")
       print(input$sampleSelect)
       print(names(scan.data))
       print(current.sample.set[[1]])
       return(current.sample.set[[1]])
     }
   })
   
   current_scan_data <- reactive({
     selected.sample.data <- current_sample_data()
     altered.sample.data <- selected.sample.data
     print("Current scan data is being reloaded")
       # Return the dataframe that corresponds with input$sampleSelect 
       # (i.e, graph.data$`std1`, which is the scan data for the std1 sample)
     print(selected.sample.data)
     print("Selected sample data better be null right now if nothing else is happening.")
       if (!(is.null(selected.sample.data))) {
         print("Selected sample data is not null. Hopefully this is reactive and updates.")
         print("Right now, curr.scan.graph.state is not null if this if statement is correct.")
         print(input$customSmooth)
         if (!(is.null(input$customSmooth)) && input$customSmooth > 0.01) {
            altered.sample.data <- applyLoessSmooth(altered.sample.data, as.numeric(input$customSmooth))
         }
         
         print(input$scansToAdd)
         if (input$scansToAdd != "None" && !is.null(input$scansToAdd)) {
            altered.sample.data <- addBackScan(altered.sample.data, input$scansToAdd, selected.sample.data)
         }
         print(input$scansToRemove)
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
   # Add any desired modifications to amplog data here
   current_amp_data <- reactive({
     current.amp.set <- amplog.data()
     altered.amp.data <- current.amp.set
     if (userinput) {
       # apply modification
       return(altered.amp.data)
     }
     if (sampletimestamp) {
       return(altered.amp.data)
     }
     # default modification
     return(altered.amp.data)
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
     selected.amp.data <- current_amp_data()
     if (!is.null(input$amplog) && !is.null(selected.amp.data)) {
       
       return(amp.plot)
     } else {
       return(NULL)
     }
   })
   

})