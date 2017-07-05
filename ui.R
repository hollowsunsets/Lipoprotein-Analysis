library(shiny)
library(shinyjs)
library(ggplot2)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
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
                            ".csv",".xlsx")),
      useShinyjs(),  
      hidden(
        div(id = "scan-interactions", 
            h3("Scan Graph Interactions"),
            p("Choose a subset of scan data that you wish to visualize."),
            uiOutput("sampleControl"),
            p("Enter a number (n > 0.0) to represent the span percent by which you would like to smooth the graph."),
            p("i.e: 0.10 = 10% smoothing span"),
            textInput("customSmooth", "Enter Smoothing Span", "0.05"),
            p("Click to optimize the smoothing to minimize the SSE (sum of squared errors)."),
            actionButton("optimizeSmooth", "Optimize Smooth"),
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
        div(id = "scan-message", "Please provide a scan dataset to view the scan visualization."),
            plotOutput("scanPlot"),
        div(id = "amp-message", "Please provide an amplog dataset to view the amplog visualization."),
        div(id = "ampPlot", 
            plotOutput("ampPlot"))
      )
    )
))
  
