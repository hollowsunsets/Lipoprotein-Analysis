# --------------------- Dependencies ---------------------------

library(dplyr) # dependency for wrangling data 
library(xlsx) # dependency for reading in .xlsx files

raw.amplog.file <- read.xlsx("data\\170522_new_data_format_for_JC_amplog.xlsx", sheetIndex = 1, as.data.frame = T, header = F, stringsAsFactors=FALSE)


# ---------------------- Functions -------------------------------

# With the given scan files, returns a data frame that has 
# contains the relevant scan data, prepared to be graphed.
scanGraphData <- function(raw.scans.file) {
  # Remove all rows before the rows that contain diameter and count data
  diameter.row.index <- grep("^Raw", raw.scans.file[,1])
  raw.scans.df <- raw.scans.file[diameter.row.index:nrow(raw.scans.file),]

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
  
  # Combine diameters and the filtered, processed scans into one data frame
  scan.graph.data <- cbind(diameters, filtered.scans)
  
  return(scan.graph.data)                                      
}


# Returns a data frame to be graphed from the given amplog file.
nmGraphData <- function(raw.amplog.file) {
  nm.graph.data <- raw.amplog.file %>% select(X1, X3) 
  return(nm.graph.data)
}

# Returns a data frame that contains labels for the scan data from the given Sparklink file.
getSampleLabels <- function(raw.sparklink.file) {
  graph.labels <- raw.sparklink.file %>% select(Sample.Name, Sample.Vial)
  return (graph.labels)
}

# Deletes every nth column in the given data frame, beginning from the 
# given starting index, i. 
nthDelete <- function(data.frame, n, i) {
  data.frame[,-(seq(i, to=ncol(data.frame), by=n))]
}

