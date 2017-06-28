#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(ggplot2)
library(xlsx)
library(chron) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
      textInput("param", "Parameter", "0.05")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Visualization',
                 textOutput("text1"),
                 textOutput("text2"),
                 plotOutput("distPlot")
        ),
        tabPanel('Readings',
                 dataTableOutput("svpTable"),
                 textInput(inputId = 'dropScan_in',label='Enter scan number to be dropped',value='',placeholder='ex: 1'),
                 actionButton("drop", "Drop Scan(s)"),
                 checkboxInput("dropSample", "Drop Sample", FALSE),
                 actionButton("writecsv", "Generate Averaged CSV")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  cur_files<-c('','','')
  currentSL<-""
  currentSL_ts<-""
  currentSvp_ts<-""
  cur_svpd<-""
  cur_svp<-""
  cur_amp<-""
  dia_start_index<-0
  sample_start_index<-0
  datetime<-0
  
  Sparklink_read_df <- NULL
  Sparklink_df_ts <- NULL
  sparklink_ts <- NULL
  merged<-data.frame()
  source("multiplot.R")
  Sparklink_read <- reactive({
    infile <- input$Sparklink
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=currentSL){
      currentSL=infile$datapath
      cur_files[1]=infile$datapath
      df<-read.csv(infile$datapath)[ ,c('Sample.Name', 'Sample.Vial')]
    }
    df
  })
  
  Sparklink_TS <- reactive({
    infile <- input$Sparklink
    if (is.null(infile)) {
      
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=currentSL_ts){
      currentSL_ts<-infile$datapath
      Sparklink_df_ts<-read.csv(infile$datapath,stringsAsFactors=FALSE)[ ,c('Executed')]
    }
    Sparklink_df_ts
  })
  
  svp_diameter_read <- reactive({
    infile <- input$Scans
    if (is.null(infile)) {
      
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=cur_svpd){
      cur_svpd<-infile$datapath
      cur_files[2]=infile$datapath
      svp_diameter_df<-read.csv(infile$datapath,skip = dia_start_index)[,2]
    }
    svp_diameter_df
  })
  
  svp_ts_read <- reactive({
    infile <- input$Scans
    if (is.null(infile)) {
      
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=currentSvp_ts){
      currentSvp_ts<-infile$datapath
      svp_ts_df<-read.csv(infile$datapath,skip=(datetime),header=F,stringsAsFactors=FALSE)[,c(F,T)][1:2,]
    }
    svp_ts_df
  })
  
  svp_read <- reactive({
    infile <- input$Scans
    if (is.null(infile)) {
      
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=cur_svp){
      cur_svp<-infile$datapath
      svp_df<-read.csv(infile$datapath,skip = sample_start_index)[,c(T,F)]
    }
    svp_df
  })
  
  svp_index_read <- reactive({
    infile <- input$Scans
    if (is.null(infile)) {
      
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=cur_svp){
      cur_svp<-infile$datapath
      svp_df<-read.csv(infile$datapath)
    }
    svp_df8pq
  })
  
  
  
  amplog_read <- reactive({
    infile <- input$Amplog
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if (infile$datapath!=cur_amp){
      cur_amp<-infile$datapath
      cur_files[3]=infile$datapath
      amp_df<-read.xlsx(infile$datapath,sheetIndex = 1,as.data.frame = T,header = F,stringsAsFactors=FALSE)[c(1,3)]
    }
    amp_df
  })
  
  
  
  output$selectUI <- renderUI({ 
    sparklink<-Sparklink_read()
    sample = list(sparklink$Sample.Name)
    sample_name<-1:length(sample[[1]])
    names(sample_name)<-sample[[1]]
    
    selectizeInput(inputId = "sampleId", "Choose a sample",choices=sample_name)
  })
  
  avg_scans <- reactive({
    if ((input$Amplog$datapath!=cur_files[3]) & (input$Sparklink$datapath!=cur_files[1]) & (input$Scans$datapath!=cur_files[2])){
      
      sparklink<-Sparklink_read()
      sparklink$Sample.Name = (as.character(sparklink$Sample.Name))
      sparklink$Sample.Vial = (as.character(sparklink$Sample.Vial))
      merge<-t(sparklink[,2])
      
      # svp import,datatime handling
      aim<-svp_index_read()
      # odd_indexes<-seq(3,ncol(aim),2)
      sample_start_index<<-which(aim[,1]=='Raw Data - Time(s)')
      dia_start_index<<-which(aim[,1]=='Raw Data - Time(s)')
      datetime<<-which(aim[,1]=='Date')
      
      diameters<-svp_diameter_read()
      svp_ts<-svp_ts_read()
      svp_ts<-as.POSIXct(paste(svp_ts[1,], svp_ts[2,], sep=" "), format="%m/%d/%Y %H:%M:%S")
      
      
      # timestamp handling for sparklink and svp
      sparklink_ts<-Sparklink_TS()
      sparklink_ts<-as.POSIXct(strptime(sparklink_ts, "%d-%m-%Y %H:%M:%S"))
      # print(svp_ts)
      svp<-svp_read()
      
      svp_ts<-times(format(svp_ts, "%H:%M:%S"))
      sparklink_ts<-times(format(sparklink_ts, "%H:%M:%S"))
      
      
      svp$Raw.Data...Time.s.<-NULL
      index=0
      for (i in 1:length(svp_ts)){
        if (between((sparklink_ts[1]-svp_ts[i])*86400,710,730)){
          index=i
          break
        }
      }
      #convert scans using the formula
      print(svp[,index:(index+(length(sparklink_ts)*6)-1)])
      print(index)
      print(sparklink_ts)
      svp<-cbind(diameters,svp[,index:(index+(length(sparklink_ts)*6)-1)])
      print(svp)
      
      svp[,2:ncol(svp)]<-svp[,2:ncol(svp)]*((25.02*exp(-0.2382*svp$diameters))+(950.9*exp(-1.017*svp$diameters))+1)
      print(svp)
      
      
      #deleting first 2 scans from the converted data
      index<-c()
      count=1
      for (i in 2:ncol(svp)){
        if (count == 1 | count == 2){
          #cat(count," ",i+1,"\n")
          index<-c(index,i)
        }
        if (count == 6){
          count<-0
        }
        count<-count+1
      }
      #index
      
      svp[index] <- list(NULL)
      
      #taking average across 4 scans for each sample
      test<-svp[2:ncol(svp)]
      n <- 1:ncol(test)
      ind <- data.frame(matrix(c(n, rep(NA, 4 - ncol(test)%%4)), byrow=F, nrow=4))
      nonna <- sapply(ind, function(x) all(!is.na(x)))
      ind <- ind[, nonna]
      scan_avg<-as.data.frame(do.call(cbind, lapply(ind, function(i)rowMeans(test[, i]))),stringsAsFactors=FALSE)
      scan_avg[sapply(scan_avg, is.factor)] <- lapply(scan_avg[sapply(scan_avg, is.factor)], as.character)
      #merge averaged svp and sparklink data
      
      colnames(scan_avg)<-sparklink[,1]
      colnames(merge)<-sparklink[,1]
      merged<<-as.data.frame(rbind(merge,sprintf("inj%d", 1:ncol(merged))),stringsAsFactors=FALSE)
      merged<<-rbind(merged,scan_avg)
      merged<<-cbind(" " =c("Sample Name","Diameter",diameters),merged)
      ones<-rep(1,ncol(merged))
      ones[1]<-"Flag"
      merged<<-rbind(ones,merged)
      svp
    }
    svp
  })
  
  amp_scans <- reactive({  
    if ((input$Amplog$datapath!=cur_files[3]) & (input$Sparklink$datapath!=cur_files[1]) & (input$Scans$datapath!=cur_files[2])){
      #read the amplog data
      amplog<-amplog_read()
      amplog_ts<-as.POSIXct(strptime(amplog$X1, "%Y-%m-%d %H:%M:%S"))
      sparklink_ts<-Sparklink_TS()
      sparklink_ts<-as.POSIXct(strptime(sparklink_ts, "%d-%m-%Y %H:%M:%S"))
      amplog_ts <- times(format(amplog_ts, "%H:%M:%S"))
      sparklink_ts_t<-times(format(sparklink_ts, "%H:%M:%S"))
      index_amp=0
      for (i in 1:length(amplog_ts)){
        if (as.character(sparklink_ts_t[1]-amplog_ts[i])<'00:12:01'){
          index_amp=i
          break
        }
      }
      
      #test<-split(amplog[index_amp:nrow(amplog),], cut(strptime(amplog$X1[index_amp:nrow(amplog)], "%Y-%m-%d %H:%M:%S"),breaks="12 mins"))
      test<-split(amplog[index_amp:nrow(amplog),], cut(as.POSIXlt(amplog$X1[index_amp:nrow(amplog)], format="%Y-%m-%d %H:%M:%S"),breaks="12 mins"))
      test
    }
    test
  })
  
  compute_graphs <- reactive({
    if ((input$Amplog$datapath!=cur_files[3]) & (input$Sparklink$datapath!=cur_files[1]) & (input$Scans$datapath!=cur_files[2])){
      svp<-avg_scans()
      result <- vector("list", (ncol(svp)-1)/4)
      svp_index=1
      for (i in 1:length(result)){
        result[[i]]<-data.frame(
          diameter = svp$diameters[1:1150],
          scan1 = predict(loess(svp[,svp_index+1]~diameters, svp, span = as.numeric(input$param))),
          scan2 = predict(loess(svp[,svp_index+2]~diameters, svp, span = as.numeric(input$param))),
          scan3 = predict(loess(svp[,svp_index+3]~diameters, svp, span = as.numeric(input$param))),
          scan4 = predict(loess(svp[,svp_index+4]~diameters, svp, span = as.numeric(input$param)))
        )
        svp_index<-svp_index+4
      }
      result
    }
    result
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
  
  scan_conform7_10<- reactive({
    result <- compute_graphs()
    loess7_10 <- result[[as.numeric(input$sampleId)]][(result[[as.numeric(input$sampleId)]]$diameter>7) & (result[[as.numeric(input$sampleId)]]$diameter<10),]
    
    loess7_10$scan3 <- loess7_10$scan3 * 1.07
    loess7_10$scan4 <- loess7_10$scan4 * 1.07
    max_index = which.max(c(max(loess7_10$scan1),max(loess7_10$scan2),max(loess7_10$scan3),max(loess7_10$scan4)))
    df_svp_max_index <- as.data.frame(apply(loess7_10,2,which.max))
    colnames(df_svp_max_index) <- c("index")
    svp_max_index <- df_svp_max_index$index[max_index+1]
    max_window <- loess7_10[between(loess7_10$diameter,(loess7_10[svp_max_index,max_index]-0.25),(loess7_10[svp_max_index,max_index]+0.25)),]
    window_sum <- c(sum(max_window$scan1),sum(max_window$scan2),sum(max_window$scan3),sum(max_window$scan4))
    out <- which(!between(window_sum,(mean(window_sum)-sd(window_sum)),(mean(window_sum)+1.1*sd(window_sum))))
    return(out)
  })
  
  scan_conform10_14<-reactive({
    result<-compute_graphs()
    loess10_14<-result[[as.numeric(input$sampleId)]][(result[[as.numeric(input$sampleId)]]$diameter>10) & (result[[as.numeric(input$sampleId)]]$diameter<14),]
    loess10_14$scan3<-loess10_14$scan3*1.07
    loess10_14$scan4<-loess10_14$scan4*1.07
    n=nrow(loess10_14)
    flag<-0
    for (i in 1:ncol(loess10_14)){
      if (rowMeans(loess10_14[i,2:5])<rowMeans(loess10_14[i+1,2:5])){
        flag=i
      }
    }
    
    max_index=which.max(c(max(loess10_14$scan1[flag:n]),max(loess10_14$scan2[flag:n]),max(loess10_14$scan3[flag:n]),max(loess10_14$scan4[flag:n])))
    
    df_svp_max_index<-as.data.frame(apply(loess10_14,2,which.max))
    colnames(df_svp_max_index)<-c("index")
    svp_max_index<-df_svp_max_index$index[max_index+1]
    max_window<-loess10_14[between(loess10_14$diameter,(loess10_14[svp_max_index,max_index]-0.25),(loess10_14[svp_max_index,max_index]+0.25)),]
    window_sum<-c(sum(max_window$scan1),sum(max_window$scan2),sum(max_window$scan3),sum(max_window$scan4))
    
    out<-which(!between(window_sum,(mean(window_sum)-sd(window_sum)),(mean(window_sum)+1.1*sd(window_sum))))
    out
  })
  
  output$text1 <- renderText({ 
    out<-scan_conform7_10()
    paste("In the range of diameter 7-10, the peaks might not conform at the scan: ",out)
  })
  
  output$text2 <- renderText({ 
    out<-scan_conform10_14()
    paste("In the range of diameter 10-14, the peaks might not conform at the scan: ",out)
  })
  
  flagScans<- reactive({
    out7_10<-scan_conform7_10()
    out10_14<-scan_conform10_14()
    scan_flag<-c(1,1,1,1)
    scan_flag[out7_10]<-0
    scan_flag[out10_14]<-0
    scan_flag
  })
  
  
  renderScan <- reactive ({
    result<-avg_scans()
    ones<-rep(1,ncol(result))
    ones[1]<-"Flag"
    result<-rbind(ones,result)
    scan_flag<-flagScans()
    n=as.numeric(input$sampleId)*4+1
    result[1,c(n-3,n-2,n-1,n)]<-scan_flag
    result[,c(1,n-3,n-2,n-1,n)]
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
}


# Run the application 
shinyApp(ui = ui, server = server)

