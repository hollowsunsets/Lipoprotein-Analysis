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
      p("Click the button below to hide the file controls or make them reappear."),
      actionButton("toggleFileControls", "Toggle File Controls"),
      useShinyjs(),  
      hidden(
        div(id = "scan-interactions", 
            h3("Scan Graph Interactions"),
            uiOutput("sampleControl"),
            uiOutput("smoothControl"),
            p(""),
            uiOutput("toggleScanFlag"),
            p(""),
            uiOutput("removeScans"),
            uiOutput("addScans"),
            uiOutput("scanSimilarity"),
            downloadButton("averageScans", label = "Get Average Scans"))
        ),
      hidden(
        div(id = "amplog-interactions",
            h3("Amplog Graph Interactions"),
            p("Click the button below to display a graph that visualizes the entire amperage dataset."),
            actionButton("displayFullAmpGraph", "Display Amp Graph with Entire Dataset"),
            actionButton("toggleTimeControls", "Toggle Time Controls"),
            hidden(
              div(id = "amp-time-controls",
                p("If desired, select a start and end time for the amperage data to change the data displayed."),
                uiOutput("timeControl")
              )
            )
        )
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
          ),
        hidden(
          div(id = "fullAmpPlot",
              plotOutput("fullAmpPlot"))
        )
      )
    )
))
  
