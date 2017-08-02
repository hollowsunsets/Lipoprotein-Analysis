 # -------------------------------------------------- Dependencies -------------------------------------------------- #
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(lubridate)
library(pracma)

options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
source("graph-analysis.R")

shinyServer(function(input, output, session) {
  # ----------------------------------------------- Global Variables --------------------------------------------------#
  scan.timestamps <- NULL
  timeControlsEnabled <- FALSE
  altered.sample.data <- NULL # maintains state for data and regressed graph
  scan.flags <- NULL
  
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
       scan.flags <<- integer(length(scan.data))
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
   
    # Defines behavior for amperage graph button, which hides and shows the 
    # amperage graph with the entire time range when clicked.
    observeEvent(input$displayFullAmpGraph, {
      toggle("fullAmpPlot")
    })
    
    # Defines behavior for the scan flag button, which flags or unflags the 
    # current sample for removal. 
    output$toggleScanFlag <- renderUI({
      actionButton("flagChange", label = label())
    })
  
    label <- reactive({
      if (!is.null(input$flagChange)) {
        if (scan.flags[[which(names(graph.data) == input$sampleSelect)]] == 0) {
          label <- "Flag This Sample"
        } else {
          label <- "Unflag This Sample"
        }
      }
    })
    
    observeEvent(input$flagChange, {
      if (scan.flags[[which(names(graph.data) == input$sampleSelect)]] == 0) {
        scan.flags[[which(names(graph.data) == input$sampleSelect)]] <<- 1
      } else {
        scan.flags[[which(names(graph.data) == input$sampleSelect)]] <<- 0
      }
    })
    # Defines behavior for the sample selection dropdown menu, which 
    # allows users to select which sample they wish to see visualized. 
   output$sampleControl <- renderUI({
     selectInput("sampleSelect", label = "Select a Sample",
                 choices = names(scans.data()), selected = names(scans.data())[1])
   })
   
   # Updates the names stored in the sample selection dropdown menu
   # if a Sparklink file has been passed in. 
   observe({
     if (!is.null(input$sparklinkData)) {
       sparklink.data <- sparklink.data()
       sample.names <- sparklink.data[,3]
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
       # Return the dataframe that corresponds with input$sampleSelect 
       # (i.e, graph.data$`std1`, which is the scan data for the std1 sample)
       if (!(is.null(altered.sample.data))) {
         if (!(is.null(input$customSmooth)) && input$customSmooth > 0.01) {
            altered.sample.data <<- applyLoessSmooth(altered.sample.data, as.numeric(input$customSmooth))
         }
         if (input$scansToAdd != "None" && !is.null(input$scansToAdd)) {
           altered.sample.data <<- addBackScan(altered.sample.data, input$scansToAdd, selected.sample.data)
           scan.names <- colnames(select(altered.sample.data, starts_with("scan")))
           scan.names[length(scan.names) + 1] <- "None"
           updateSelectInput(session, "scansToAdd", choices = scansDropped, selected = "None")
           updateSelectInput(session, "scansToRemove", choices = scan.names, selected = "None")
         }
         if (input$scansToRemove != "None"  && !is.null(input$scansToRemove)) {
           altered.sample.data <<- dropScan(altered.sample.data, input$scansToRemove)
           scan.names <- colnames(select(altered.sample.data, starts_with("scan")))
           scan.names[length(scan.names) + 1] <- "None"
           updateSelectInput(session, "scansToAdd", choices = scansDropped, selected = "None")
           updateSelectInput(session, "scansToRemove", choices = scan.names, selected = "None")
         }                    
       }
     return(altered.sample.data)
   })

   observeEvent(input$sampleSelect, {
     altered.sample.data <<- current_sample_data()
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
   # File name of average scans file should be {original file name}_average_scans.csv
   output$averageScans <- downloadHandler(
     filename = function() { paste(gsub("\\..*","",input$scansData), "_average_scans", '.csv', sep='') }, 
     content = function(file) {
        if (!is.null(input$sparklinkData) && !is.null(scan.flags)) {
          write.csv(getAverageScans(scans.data(), sparklink.data(), scan.flags), file, row.names = FALSE)
        } else if (!is.null(input$sparklinkData)) {
            write.csv(getAverageScans(scans.data(), sparklink.data()), file, row.names = FALSE)
        } else if (!is.null(scan.flags)) {
            write.csv(getAverageScans(scans.data(), scan.flags = scan.flags), file, row.names = FALSE) 
        } else {
         write.csv(getAverageScans(scans.data()), file, row.names = FALSE)
        }
       }
     )
   
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
        textInput("customSmooth", "Enter Graph Smoothing Span", "0.05")
     )
   })
   
   output$scanSimilarity <- renderUI({
     checkboxInput("showDissimilarScan", label = "Show Scan Dissimilarity",
                   value = TRUE)
   })
   
   
  # ---------------------------------- Amplog Interactions Functions ----------------------------- #
   output$timeControl <- renderUI({
     amp.range <- amplog.data()
     start.time <- amp.range$X0[1]
     end.time <- tail(amp.range$X0, n = 1)
     sliderInput("range", "Time range:",
                 min = start.time, 
                 max = end.time, 
                 value = c(start.time, start.time + (12 * 60)))
   })
   
   
   # Add any desired modifications to amplog data here
   current_amp_data <- reactive({
     current.amp.set <- amplog.data()
     altered.amp.data <- current.amp.set
     if (timeControlsEnabled) {
       altered.amp.data <- intervalAmperageData(altered.amp.data,
                                                input$timeControl[[1]],
                                                input$timeControl[[2]])

       return(altered.amp.data)
     }
     if (!(is.null(input$sampleSelect)) && !(is.null(scan.timestamps))) {
       current.sample.timestamps <- scan.timestamps %>% filter(sample.name == input$sampleSelect)
       altered.amp.data <- intervalAmperageData(altered.amp.data, 
                                                current.sample.timestamps$start.time,
                                                current.sample.timestamps$end.time)

       return(altered.amp.data)
     }
     # default modification
     altered.amp.data <- intervalAmperageData(altered.amp.data, altered.amp.data$X0[1], 
                                              altered.amp.data$X0[1] + (12 * 60))
     return(altered.amp.data)
   })
   
   
   observeEvent(input$toggleTimeControls, {
     timeControlsEnabled <<- !timeControlsEnabled
     toggle("amp-time-controls")
     current.amp.data.reload <- current_amp_data()
   })
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   
   output$scanPlot <- renderPlot({
     selected.scan.data <- current_scan_data()
     if (!is.null(input$scansData) && !is.null(selected.scan.data)) { 
      if (input$showDissimilarScan) {
        # dissimilar.scans <- findDissimilarScan(selected.scan.data)
        # 
        # similar.plot.data <- selected.scan.data[, !names(selected.scan.data) %in% dissimilar.scans]
        # 
        # dissimilar.plot.data <- selected.scan.data[dissimilar.scans]
        similar.plot.data <- selected.scan.data
        scan.plot.data <- melt(similar.plot.data, id.vars = "sample.diameters", variable.name = 'scans')
        
        scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
          geom_line(aes(colour = scans)) +
          xlab("Diameters (nm)") +
          ylab("Concentration (dN#/cm^2)") +
          ggtitle(paste0(input$sampleSelect)) + 
            theme(plot.title = element_text(hjust = 0.5)) # Centers graph title
        
      } else {
        scan.plot.data <- melt(selected.scan.data, id.vars = "sample.diameters", variable.name = 'scans')
        
        scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
           geom_line(aes(colour = scans)) +
           xlab("Diameters (nm)") +
           ylab("Concentration (dN#/cm^2)") +
           ggtitle(paste0(input$sampleSelect)) + 
              theme(plot.title = element_text(hjust = 0.5)) # Centers graph title
      }
       return(scan.plot)
     } else {
       return(NULL)
     }
   })
   
   # Generates the graph that visualizes the amplog data
   output$ampPlot <- renderPlot({
     selected.amp.data <- current_amp_data()
     if (!is.null(input$amplogData) && !is.null(selected.amp.data)) {
       amp.plot.subtitle <- paste0("From ", format(selected.amp.data$X0[[1]], usetz=TRUE, tz="Etc/GMT+8"),
                              " to ", format(tail(selected.amp.data$X0, n = 1), usetz=TRUE, tz="Etc/GMT+8"))
       amp.plot <- ggplot(data = selected.amp.data) +
                       geom_line(aes(x = X0, y= X2, group = 1)) + 
                       xlab("Time (PST, Standard Time)") +
                       ylab("Amperage (amp)") +
                       ggtitle(bquote(atop("Selected Amperage Data", 
                                          atop(.(amp.plot.subtitle))))) +
                       theme(plot.title = element_text(hjust = 0.5))
       return(amp.plot)
     } else {
       return(NULL)
     }
   })
   
   output$fullAmpPlot <- renderPlot({
     full.amp.data <- amplog.data()
     if (!is.null(input$amplogData) && !is.null(full.amp.data)) {
       full.amp.plot <- ggplot(data = full.amp.data) + 
                          geom_line(aes(x = X0, y = X2, group = 1)) +
                           xlab("Time (PST, Standard Time)") +
                           ylab("Amperage (amp)") +
                           ggtitle(paste0("Entire Amperage Dataset")) + 
                           theme(plot.title = element_text(hjust = 0.5))
       return(full.amp.plot)
     } else {
       return(NULL)
     }
   })

})