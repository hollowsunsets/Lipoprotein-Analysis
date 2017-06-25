library(dplyr) # dependency for wrangling data 
library(xlsx) # dependency for reading in .xlsx files
install.packages("microbenchmark")

# test data
sparklink.file <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE)
scans.file <- read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE)
amplog.file <- read.xlsx("data\\ampdDataset2.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors=FALSE)

# With the given Sparklink and scan files, returns a data frame that has 
# scan data, 
scanGraphData <- function(raw.sparklink.file, raw.scans.file) {
  
  # Getting relevant data from Sparklink file
  scan.graph.data <- raw.sparklink.file %>% select(Sample.Name, Sample.Vial, Executed)
  
  # Removing rows before relevant scan data begins from scans file 
  raw.scans.df <- scans.file[which(scans.file$Sample.File == "Raw Data - Time(s)"):nrow(scans.file),]

  # Change column names to the first row (which contains names of relevant data)
  colnames(raw.scans.df) <- raw.scans.df[1,]
  raw.scans.df <- raw.scans.df[-1,]
  
  # Filter out data that is not relevant to scans (gets all count data)
  filtered.scans <- raw.scans.df %>% select(starts_with("Count")) 
  
  # Remove first two scans 
  filtered.scans <- nthDelete(filtered.scans, 4, 1)
  filtered.scans <- nthDelete(filtered.scans, 4, 2)
  
  # Apply formula to scan data 
  test2 <- scans.med
  svp[,2:ncol(svp)]<-svp[,2:ncol(svp)]*((25.02*exp(-0.2382*svp$diameters))+(950.9*exp(-1.017*svp$diameters))+1)



  return(scan.graph.data)                                      
}


nmGraphData <- function(amplog.file) {
  nm.graph.data <- amplog.file %>% select(X1, X3) 
}

# Deletes every nth column in the given data frame.
nthDelete <- function(data.frame, n, i) {
  data.frame[,-(seq(i, to=ncol(data.frame), by=n))]
}

