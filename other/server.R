library(shiny)
source("other\\file-setup.R")
source("other\\graph-alteration.R")
source("other\\graph-analysis.R")

shinyServer(function(input, output) {
  
  # Retrieve files from Shiny file input widgets. 
  sparklink.file <- input$Sparklink$datapath
  scans.file <- input$Scans$datapath
  amplog.file <- input$Amplog$datapath
  
  
})