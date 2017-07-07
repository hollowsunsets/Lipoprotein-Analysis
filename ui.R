library(shiny)
library(shinyjs)
library(ggplot2)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
      div(id="file-selection-controls",
        h3("File Selection"),
        p("Please upload the data files that you wish to visualize using this application."),
        fileInput("sparklinkData", "Choose Sparklink File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
        fileInput("scansData", "Choose Scans File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
        fileInput("amplogData", "Choose Amplog File",
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv",".xlsx"))),
      actionButton("toggleFileControls", "Hide File Controls"),
      useShinyjs(),  
      hidden(
        div(id = "scan-interactions", 
            h3("Scan Graph Interactions"),
            p("Choose a subset of scan data that you wish to visualize."),
            uiOutput("sampleControl"),
            uiOutput("smoothControl"),
            uiOutput("removeScans"),
            uiOutput("addScans"),
            downloadButton("averageScans", label = "Get Average Scans"))
        ),
      hidden(
        div(id = "amplog-interactions",
            h3("Amplog Graph Interactions"),
            uiOutput("timeControl"),
            uiOutput("nmControl"))
      )
    ),

    mainPanel(
      useShinyjs(),
        div(id = "scan-message", "Please provide a scan dataset to view the scan visualization."),
        hidden(
          div(id = "scanPlot",
              plotOutput("scanPlot"))
          ),
        div(id = "amp-message", "Please provide an amplog dataset to view the amplog visualization."),
        hidden(
        div(id = "ampPlot", 
            plotOutput("ampPlot"))
        )
      )
    )
))
  
