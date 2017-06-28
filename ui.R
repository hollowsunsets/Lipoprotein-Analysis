library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
      h3("File Selection"),
      fileInput("Sparklink", "Choose Sparklink File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("Scans", "Choose Scans File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("Amplog", "Choose Amplog File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx")),
      conditionalPanel(   # only appears if scan data has been provided 
        condition = "",
        checkboxInput(), # checkbox for removing a scan
        checkboxInput() 
      )),
    mainPanel(
      conditionalPanel(  # only appears if scan data has been provided
        condition = "",
        plotOutput("scanPlot")
      ),
      conditionalPanel(  # only appears if amplog data has been provided
        condition = "",
        plotOutpu("nmPlot")
      )
    )
  )
))