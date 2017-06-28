library(shiny)

shinyUI(fluidPage(
  
  # Application title
  
  titlePanel("Samples"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      htmlOutput("selectUI"),
                  fileInput("Sparklink", "Choose Sparklink File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                  ),
                  fileInput("Scans", "Choose Scans File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                  ),
                  fileInput("Amplog", "Choose Amplog File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx")
                  ),
                  textInput("param", "Parameter", "0.05") # what is "Parameter"? rename to make more user friendly
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Visualization',
                 textOutput("text1"),
                 textOutput("text2"),
                 plotOutput("distPlot")
        )#,
       # tabPanel('Readings',
        #         dataTableOutput("svpTable"),
         #        textInput(inputId = 'dropScan_in',label='Enter scan number to be dropped',value='',placeholder='ex: 1'),
          #       actionButton("drop", "Drop Scan(s)"),
           #      checkboxInput("dropSample", "Drop Sample", FALSE),
            #     actionButton("writecsv", "Generate Averaged CSV")
      #  )
      )
    )
  )
))