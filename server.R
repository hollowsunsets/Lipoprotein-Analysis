 # -------------------------------------------------- Dependencies -------------------------------------------------- #
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)
library(lubridate)
library(pracma)
library(zoo)


options(shiny.maxRequestSize=30*1024^2) # allows larger file sizes (up to 30MB)

source("file-setup.R")
source("graph-alteration.R")
source("graph-analysis.R")

shinyServer(function(input, output, session) {
  # ----------------------------------------------- Global Variables --------------------------------------------------#
  scan.timestamps <- NULL
  altered.sample.data <- NULL # maintains state for data and regressed graph
  sample.flags <- NULL # tracks the samples that are bad/good
  dissimilar.scans <- NULL
  
  # -------------------------------------- General Data Processing and Retrieval ----------------------------------- #

    # Retrieves and reads in the given data files 
   sparklink.data <- reactive({
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
       dissimilar.scans <<- vector(mode = "list", length = length(scan.data))
       names(dissimilar.scans) <<- names(scan.data)
       sample.flags <<- rep("normal", length(scan.data))
       names(sample.flags) <<- names(scan.data)
     }
     return(scan.data)
   })
   
   amplog.data <- reactive({
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
    output$toggleSampleFlag <- renderUI({
      actionButton("flagChange", label = label())
    })
  
    label <- reactive({
      if (!is.null(input$flagChange)) {
        if (sample.flags[input$sampleSelect] == "normal") {
          label <- "Reject This Entire Sample"
        } else {
          label <- "Undo Entire Sample Rejection"
        }
      }
    })
    
    observeEvent(input$flagChange, {

      if (sample.flags[input$sampleSelect] == "normal") {
        sample.flags[input$sampleSelect] <<- "rejected"
      } else {
        
        sample.flags[input$sampleSelect] <<- "normal"
      }
    })
    
    
    observeEvent(input$bookmarkState, {
      session$doBookmark()
    })
    
    enableBookmarking(store = "url")
    
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
       current.sample.data <- current.sample.set[[input$sampleSelect]]
     } else {
       current.sample.data <- current.sample.set[[1]]
     }
     return(current.sample.data)
   })
   
   
   # Tracks the state of the current graph
   current_scan_data <- reactive({
     selected.sample.data <- current_sample_data()
       # Return the dataframe that corresponds with input$sampleSelect 
       # (i.e, graph.data$`std1`, which is the scan data for the std1 sample)
       if (!(is.null(altered.sample.data))) {
         if (!(is.null(input$customSmooth)) && input$customSmooth > 0.01) {
            altered.sample.data <<- applyLoessSmooth(altered.sample.data, as.numeric(input$customSmooth))
         }
       }
     return(altered.sample.data)
   })
   
   
   
   current_dissimilar_scans <- reactive({
     current.sample.data <- current_scan_data()
     if(!is.null(current.sample.data)) {
       print(dissimilar.scans)
       # Sets temporary variable that contains the state of current scans marked as dissimilar
       current.dissimilar.scans <- dissimilar.scans[[input$sampleSelect]]
       
       # Updates the scan names in the dropdown menu for flagging scans and updates the stored flagged scans
       # This essentially regenerates the contents of the dropdown menu - it's too annoying to have to track the ordering otherwise
       if (input$scansToFlag != "None" && !is.null(input$scansToFlag) &&  !(input$scansToFlag %in% current.dissimilar.scans)) {
    
         # The scan that the user picked is added to the current dissimilar scans
         current.dissimilar.scans <- append(current.dissimilar.scans, input$scansToFlag)
         
         # Retrieves the names of the scans that are currently being graphed.
         current.scan.names <- colnames(select(current.sample.data, starts_with("scan")))
         
         # Retrieves the names of the scans that have not been previously marked as dissimilar
         flagged.names <- current.scan.names[(current.scan.names %in% current.dissimilar.scans)]
         
         # Adds "None" to the end of the list of names
         flagged.names <- append(flagged.names, "None")
         
         unflagged.names <- current.scan.names[!(current.scan.names %in% current.dissimilar.scans)]
         unflagged.names <- append(unflagged.names, "None")
         
         updateSelectInput(session, "scansToFlag", choices = unflagged.names, selected = "None")
         updateSelectInput(session, "scansToUnflag", choices = flagged.names, selected = "None")
         
         # The tracked dissimilar scans is updated 
         print(dissimilar.scans[[input$sampleSelect]])
         dissimilar.scans[[input$sampleSelect]] <<- current.dissimilar.scans
         print(dissimilar.scans[[input$sampleSelect]])
       } 
       
       if (input$scansToUnflag != "None" && !is.null(input$scansToUnflag) && (input$scansToUnflag %in% current.dissimilar.scans)) {
         current.dissimilar.scans <- current.dissimilar.scans[-which(current.dissimilar.scans == input$scansToUnflag)]
         
         current.scan.names <- colnames(select(current.sample.data, starts_with("scan")))
         flagged.names <- current.scan.names[(current.scan.names %in% current.dissimilar.scans)]
         flagged.names <- append(flagged.names, "None")
         
         unflagged.names <- current.scan.names[!(current.scan.names %in% current.dissimilar.scans)]
         unflagged.names <- append(unflagged.names, "None")

         updateSelectInput(session, "scansToFlag", choices = unflagged.names, selected = "None")
         updateSelectInput(session, "scansToUnflag", choices = flagged.names, selected = "None")
         print(dissimilar.scans[[input$sampleSelect]])
         dissimilar.scans[[input$sampleSelect]] <<- current.dissimilar.scans
         print(dissimilar.scans[[input$sampleSelect]])
         

       }
       print(current.dissimilar.scans)
       print(dissimilar.scans)
     }
     return(dissimilar.scans[[input$sampleSelect]])
   })
   
   observeEvent(input$sampleSelect,
                {
     altered.sample.data <<- current_sample_data()
     current.dissimilar.scans <- current_dissimilar_scans()
     print(current.dissimilar.scans)
     if (!is.null(current.dissimilar.scans)) {
        dissimilar.scans[[input$sampleSelect]] <<- current.dissimilar.scans
     }

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
        if (!is.null(input$sparklinkData) && !is.null(dissimilar.scans) && !is.null(sample.flags)) {
          return(write.csv(getAverageScans(scans.data(), sparklink.data(), dissimilar.scans = dissimilar.scans, sample.flags= sample.flags), file, row.names = FALSE))
        } else if (!is.null(input$sparklinkData) && !is.null(dissimilar.scans)) {
          return(write.csv(getAverageScans(scans.data(), sparklink.data(), dissimilar.scans = dissimilar.scans), file, row.names = FALSE))
        } else if (!is.null(input$sparklinkData) && !is.null(sample.flags)) {
          return(write.csv(getAverageScans(scans.data(), sparklink.data(), sample.flags = sample.flags), file, row.names = FALSE))
        } else if (!is.null(dissimilar.scans)) {
          return(write.csv(getAverageScans(scans.data(), dissimilar.scans = dissimilar.scans), file, row.names = FALSE))
        } else if (!is.null(sample.flags)) {
          return(write.csv(getAverageScans(scans.data(), sample.flags = sample.flags), file, row.names = FALSE))
        } else {
          return(write.csv(getAverageScans(scans.data()), file, row.names = FALSE))
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
   
   output$flagScans <- renderUI({
     selected.sample.data <- current_sample_data()
     scan.names <- colnames(select(selected.sample.data, starts_with("scan")))
     unflagged.scans <- scan.names[!(scan.names %in% dissimilar.scans[[input$sampleSelect]])]
     unflagged.scans <- append(unflagged.scans, "None")
     selectInput("scansToFlag", label = "Flag a Scan",
                 choices = unflagged.scans, selected = "None")
   })
   
   output$unflagScans <- renderUI({
     selected.sample.data <- current_sample_data()
     scan.names <- colnames(select(selected.sample.data, starts_with("scan")))
     flagged.scans <- scan.names[(scan.names %in% dissimilar.scans[[input$sampleSelect]])]
     flagged.scans <- append(flagged.scans, "None")
     selectInput("scansToUnflag", label = "Unflag a Scan",
                 choices = flagged.scans, selected = "None")
   })
   
   
   
   output$smoothControl <- renderUI({
     tagList(
        p("Enter a number (n > 0.0) to represent the span percent by which you would like to smooth the graph."),
        p("i.e: 0.10 = 10% smoothing span"),
        textInput("customSmooth", "Enter Graph Smoothing Span", "0.05")
     )
   })
   
   output$graphShowSimilarity <- renderUI({
     checkboxInput("showDissimilarScan", label = "Show Dissimilarity on Graph",
                   value = TRUE)
   })

   
   
  # ---------------------------------- Amplog Interactions Functions ----------------------------- #
   output$toggleTimeControls <- renderUI({
     checkboxInput("enableTimeControls", label = "Toggle & Enable Time Controls", 
                   value = FALSE)
   })
   
   output$timeControl <- renderUI({
     amp.range <- amplog.data()
     start.time <- amp.range$times[1]
     end.time <- tail(amp.range$times, n = 1)
     sliderInput("range", "Time range:",
                 min = start.time, 
                 max = end.time, 
                 value = c(start.time, start.time + (12 * 60)))
   })
   
   
   # Add any desired modifications to amplog data here
   current_amp_data <- reactive({
     current.amp.set <- amplog.data()
     altered.amp.data <- current.amp.set
     if (!is.null(input$timeControl) && input$enableTimeControls) {
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
     altered.amp.data <- intervalAmperageData(altered.amp.data, altered.amp.data$times[1], 
                                              altered.amp.data$times[1] + (12 * 60))
     return(altered.amp.data)
   })
   
   
    observeEvent(input$enableTimeControls, {
      toggle("amp-time-controls")
    })
   
  # ------------------------------------ Graph Rendering ----------------------------------------- #
   
   output$scanPlot <- renderPlot({
     selected.scan.data <- current_scan_data()
     if (!is.null(input$scansData) && !is.null(selected.scan.data)) { 
      if (input$showDissimilarScan) {
        
        current.scan.similarities <- current_dissimilar_scans()

        scan.plot.data <- melt(selected.scan.data, id.vars = "sample.diameters", variable.name = 'scans')
        scan.plot.data <- scan.plot.data %>% mutate("dissimilar" = scan.plot.data$scans %in% current.scan.similarities)
        
        scan.plot.subtitle <- "Dissimilar Scans:"
        for (i in 1:length(current.scan.similarities)) {
          scan.plot.subtitle <- paste(scan.plot.subtitle, current.scan.similarities[i])
        }
        
        scan.plot <- ggplot(data = scan.plot.data, aes(sample.diameters, value)) +
          geom_line(aes(colour = scans, linetype = dissimilar)) +
          xlab("Diameters (nm)") +
          ylab("Concentration (dN#/cm^2)") +
          ggtitle(bquote(atop(.(input$sampleSelect), 
                              atop(.(scan.plot.subtitle))))) + 
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
   
   # Generates the graph that visualizes the user-selected (or default) amplog data
   output$ampPlot <- renderPlot({
     selected.amp.data <- current_amp_data()
     if (!is.null(input$amplogData) && !is.null(selected.amp.data)) {
       amp.plot.subtitle <- paste0("From ", format(selected.amp.data$times[[1]], usetz=TRUE, tz="Etc/GMT+8"),
                              " to ", format(tail(selected.amp.data$times, n = 1), usetz=TRUE, tz="Etc/GMT+8"))
       amp.plot <- ggplot(data = selected.amp.data) +
                       geom_line(aes(x = times, y= nanometers, group = 1)) + 
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
   
   # Generates the graph that visualizes the full range of amplog data
   output$fullAmpPlot <- renderPlot({
     full.amp.data <- amplog.data()
     if (!is.null(input$amplogData) && !is.null(full.amp.data)) {
       full.amp.plot <- ggplot(data = full.amp.data) + 
                          geom_line(aes(x = times, y = nanometers, group = 1)) +
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