library(dplyr) # dependency for wrangling data 
library(xlsx) # dependency for reading in .xlsx files
library(microbenchmark) # dependency for testing performance

# test data
raw.sparklink.file <- read.csv("data\\SparkRunlistDataset2.csv", stringsAsFactors = FALSE)
raw.scans.file <- read.csv("data\\AIMDataset2.csv", stringsAsFactors = FALSE)
amplog.file <- read.xlsx("data\\ampdDataset2.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors=FALSE)

# With the given Sparklink and scan files, returns a data frame that has 
# scan data, 
scanGraphData <- function(raw.sparklink.file, raw.scans.file) {
  # Removing rows before relevant scan data begins from scans file 
  raw.scans.df <- scans.file[which(scans.file$Sample.File == "Raw Data - Time(s)"):nrow(scans.file),]

  # Change column names to the first row (which contains names of relevant data)
  colnames(raw.scans.df) <- raw.scans.df[1,]
  raw.scans.df <- raw.scans.df[-1,]
  
  # Filter out data that is not relevant to scans (gets all count data and diameters)
  filtered.scans <- raw.scans.df %>% select(starts_with("Count")) 
  diameters <- raw.scans.df %>% select(`Diameter #1`)
  
  # Remove first two scans of six scans 
  filtered.scans <- nthDelete(filtered.scans, 6, 1)
  filtered.scans <- nthDelete(filtered.scans, 5, 1)
  
  # Converts data frame to numeric matrix to allow binary operations (limitation of R)
  filtered.scans <- (as.data.frame(lapply(filtered.scans, as.numeric)))
  diameters <- (as.data.frame(lapply(diameters, as.numeric)))
  
    # Apply formula to scan data 
  filtered.scans[,1:ncol(filtered.scans)] <- filtered.scans[,1:ncol(filtered.scans)] *
                                             ((25.02 * exp(-0.2382 * diameters$Diameter..1)) + # formula starts here
                                             (950.9 * exp(-1.017 * diameters$Diameter..1)) + 1)
  scan.graph.data <- cbind(diameters, filtered.scans)
  
  return(scan.graph.data)                                      
}


nmGraphData <- function(amplog.file) {
  nm.graph.data <- amplog.file %>% select(X1, X3) 
}

# Deletes every nth column in the given data frame, beginning from the 
# given starting index, i. 
nthDelete <- function(data.frame, n, i) {
  data.frame[,-(seq(i, to=ncol(data.frame), by=n))]
}

