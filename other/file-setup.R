library(dplyr) # dependency for wrangling data 
library(xlsx) # dependency for reading in .xlsx files
install.packages("microbenchmark")

# test data
sparklink.file <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE)
scans.file <- read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE)
amplog.file <- read.xlsx("data\\ampdDataset2.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors=FALSE)

# Returns 
scanGraphData <- function(raw.sparklink.file, raw.scans.file) {
  
  # Getting relevant data from Sparklink file
  scan.graph.data <- raw.sparklink.file %>% select(Sample.Name, Sample.Vial, Executed)
  
  # Removing rows before relevant scan data begins from scans file 
  scans.test <- scans.file[which(scans.file$Sample.File == "Raw Data - Time(s)"):nrow(scans.file),]

  # Change column names to the first row (which contains names of relevant data)
  colnames(scans.test) <- scans.test[1,]
  scans.test <- scans.test[-1,]
  
  
  scans.med <- select(scans.test, starts_with("Count"))
  
  return(scan.graph.data)                                      
}

nmGraphData <- function(amplog.file) {
  nm.graph.data <- amplog.file %>% select(X1, X3) 
}
