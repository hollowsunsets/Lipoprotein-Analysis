library(shiny)
library(dplyr)
library(xlsx)
library(ggplot2)

source("file-setup.R")
source("graph-alteration.R")
source("graph-analysis.R")
source("multiplot.R")

shinyServer(function(input, output) {
    
    
    output$selectUI <- renderUI({ 
      sparklink<-Sparklink_read()
      sample = list(sparklink$Sample.Name)
      sample_name<-1:length(sample[[1]])
      names(sample_name)<-sample[[1]]
      
      selectizeInput(inputId = "sampleId", "Choose a sample",choices=sample_name)
    })
    
    output$distPlot <- renderPlot({
      result<-compute_graphs()
      
      test<-amp_scans()
      #sample_df<-list(QC1,span1,df_1.25_1,df_2.5_1)
      p1<-ggplot(data=result[[(as.numeric(input$sampleId))]][1:5],aes(diameter)) + geom_line(aes(color="scan 1",y=scan1)) + geom_line(aes(color="scan 2",y=scan2))+geom_line(aes(color="scan 3",y=scan3))+geom_line(aes(color="scan 4",y=scan4))+ggtitle(names(input$sample_name)[as.numeric(input$sampleId)])
      
      p2<-ggplot(data=test[[as.numeric(input$sampleId)]],aes(x=X1, y=X3, group=1))+geom_line()
      layout <- matrix(c(1, 1, 1, 1, 2), nrow = 5, byrow = TRUE)
      multiplot(p1,p2,cols=1,layout = layout)
    })
    

    
    output$text1 <- renderText({ 
      out<-scan_conform7_10()
      paste("In the range of diameter 7-10, the peaks might not conform at the scan: ",out)
    })
    
    output$text2 <- renderText({ 
      out<-scan_conform10_14()
      paste("In the range of diameter 10-14, the peaks might not conform at the scan: ",out)
    })
    

    output$svpTable <- renderDataTable({
      result<-renderScan()
      result
    })
    
    dropScans <- observeEvent(input$drop, {
      if (input$dropScan_in!=""){
        dropScan_df<-renderScan()
        scan_index<-as.numeric(input$dropScan_in)
        dropScan_mat<-data.matrix(dropScan_df[2:(nrow(dropScan_df)-1),c(-1,-(scan_index+1))])
        recalc_row<-rowMeans(dropScan_mat)
        merged[1,(as.numeric(input$sampleId)+1)]<<-0
        merged[4:(nrow(merged)-1),(as.numeric(input$sampleId)+1)]<<-recalc_row
      }
      
      if (input$dropSample == TRUE){
        merged[,(as.numeric(input$sampleId)+1)]<<-NULL
      }
    })
    
    generateCSV <- observeEvent(input$writecsv, {
      write.csv(merged,"Fyttic_input.csv")
    })
  
  
})