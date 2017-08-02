# --------------------- Dependencies ---------------------------
# NOTE: Comment out or move to server.R after done testing
 library(dplyr) # dependency for general data wrangling
 library(readxl) # dependency for reading in .xlsx files
 library(lubridate) # dependency for manipulating timestamps

# --------------------- Test Variables --------------------------
# raw.scans.file <- read.csv("data\\170522_new_data_format_for_JC_DMA.csv", na.strings = c("", "NA"), stringsAsFactors=FALSE)
# raw.scans.file <- read.csv("data\\AIMDataset2.csv", na.strings = c("", "NA"), stringsAsFactors=FALSE)
# raw.sparklink.file <- read.csv("data\\170622_Study114_Runlist.csv", stringsAsFactors = FALSE, header = FALSE)
graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA))) 
# sparklink.timestamps <- scanTimeStamps(raw.scans.file, raw.sparklink.file)
# Note: na.strings = c("", "NA") is necessary for time stamps to be retrieved properly
# amp.graph.data <- ampGraphData(read_excel("data\\170712_Study115_Batch3_Amplog.xlsx", col_names = FALSE))
# test.amprange <- intervalAmperageData(amp.graph.data, amp.graph.data$X0[1], amp.graph.data$X0[1] + (12 * 60))
 


# ---------------------- Functions -------------------------------


# With the given scan files, returns a data frame that has 
# contains the relevant scan data, prepared to be graphed.
scanGraphData <- function(raw.scans.file, raw.sparklink.file = NULL) {
  # Remove all rows before the rows that contain diameter and count data
  diameter.row.index <- grep("^Raw", raw.scans.file[,1])                            
  raw.scans.df <- raw.scans.file[diameter.row.index:nrow(raw.scans.file),]

  # Change column names to the first row (which contains identifiers of samples)
  colnames(raw.scans.df) <- raw.scans.df[1,]
  raw.scans.df <- raw.scans.df[-1,]
  
  # Filter out data that is not relevant to scans (gets all count data and diameters)
  filtered.scans <- raw.scans.df %>% select(starts_with("Count")) 
  diameters <- raw.scans.df %>% select(`Diameter #1`)
  
  # Remove first two scans of six scans 
  filtered.scans <- nthDelete(filtered.scans, 6, 1)
  filtered.scans <- nthDelete(filtered.scans, 5, 1)
  
  # Converts data frames to numeric matrix to allow binary operations (limitation of R)
  filtered.scans <- (as.data.frame(lapply(filtered.scans, as.numeric))) 
  diameters <- (as.data.frame(lapply(diameters, as.numeric)))
  
  # Apply formula to scan data 
  filtered.scans[,1:ncol(filtered.scans)] <- filtered.scans[,1:ncol(filtered.scans)] *
                                             ((25.02 * exp(-0.2382 * diameters$Diameter..1)) + # formula starts here
                                             (950.9 * exp(-1.017 * diameters$Diameter..1)) + 1)
  
  # Adds the diameter data to the filtered scan data 
  filtered.scans <- cbind(diameters, filtered.scans)
  
  # Size of vector (data structure holding the sample data) is set to number of samples
  scan.graph.data <- vector("list", (ncol(filtered.scans)/4)) 
  
  # Index is pointer for traversing through filtered data 
  scan.index <- 1
  
  # Split the filtered scans dataframe into smaller dataframes - each corresponding to a sample, 
  # with corresponding scan data and diameters
  # i.e: sample1 --- example of dataframes that populate the vector
  #     scan1             -0.466  -0.431 -0.431 -0.419 -0.419 ...
  #     scan2             -0.491  -0.491 -0.491 -0.479 -0.479
  #     scan3             -0.304  -0.304 -0.304 -0.293 -0.293
  #     scan4             -0.335  -0.335 -0.335 -0.324 -0.324
  #     sample.diameters   5.97    5.97   5.97   5.98   5.98
  for (i in 1:length(scan.graph.data)) {
    sample.data <- data.frame(    
      scan1 = filtered.scans[,scan.index + 1], 
      scan2 = filtered.scans[,scan.index + 2],
      scan3 = filtered.scans[,scan.index + 3],
      scan4 = filtered.scans[,scan.index + 4]
    )
    # Adds sample dataframe to the vector that will contain all sample data frames
    sample.data <- sample.data %>% mutate("sample.diameters" = filtered.scans$`Diameter..1`)
    scan.graph.data[[i]] <- sample.data 
    # Updates index so the next set of scans can be retrieved
    scan.index <- scan.index + 4  
  }

  # Set the names of the sample data frames
  # If sparklink file was provided, get names from sparklink file. Else, set to default.
  if (!(is.null(raw.sparklink.file))) {
    sample.names <- as.data.frame(raw.sparklink.file[,3], stringsAsFactors = FALSE)
    sample.size.difference <- length(scan.graph.data) - nrow(sample.names)
    if (nrow(sample.names) < length(scan.graph.data)) {
      sample.names <- rbind(sample.names, 
                            c(paste0("unlabeled sample ", 
                                     nrow(sample.names):
                                     nrow(sample.names) + sample.size.difference)))
    }
    names(scan.graph.data) <- sample.names[,1]
  } else {
    names(scan.graph.data) <- c(paste0("sample ", 1:length(scan.graph.data)))
  }
  
  return(scan.graph.data)                                      
}

# Returns a dataframe containing the corresponding starting and end time stamps 
# for each sample (the time during which each sample was run) from the given raw scans file. 
scanTimeStamps <- function(raw.scans.file, raw.sparklink.file = NULL) {
  
  # Removes all rows before and after the time stamps and the sample labels. 
  timestamp.start.index <- grep("^Sample #", raw.scans.file[,1])
  timestamp.end.index <- grep("^Start", raw.scans.file[,1])
  timestamp.df <- raw.scans.file[timestamp.start.index:timestamp.end.index,]
  
  # Changes the column names of the time stamp dataframe to the first row, then
  # deletes the first row.
  colnames(timestamp.df) <- timestamp.df[1,]
  timestamp.df <- timestamp.df[-1,]

  # Selects only the relevant data (in this context only the columns that have information in them)
  # All blank columns were set to NA when read in, and are now deleted.
  timestamp.df <- timestamp.df[colSums(is.na(timestamp.df)) != nrow(timestamp.df)]
  
  # Remove the first column of the dataframe containing labels
  timestamp.df <- timestamp.df[,-1]
  
  # Removes the first two scans of the six scans from the dataset
  timestamp.df <- nthDelete(timestamp.df, 6, 1)
  timestamp.df <- nthDelete(timestamp.df, 5, 1)

  # Creates a dataframe from the start time and date for each scan
  final.timestamps <- data.frame(start.time = parse_date_time(paste(timestamp.df[1,], 
                                                                    timestamp.df[2,]),
                                                              c("%d/%m/%Y %H:%M:%S",
                                                                "%m/%d/%y %H:%M:%S")))
  
  # Creates columns of a rounded down start time and a rounded up end time to represent a flexible
  # starting and end time to be graphed for each sample
  number.of.samples <- floor((nrow(final.timestamps)/4)) * 4 
  sample.times <- data.frame(start.time = final.timestamps[(seq(1, to=number.of.samples, by=4)),],
                             end.time = final.timestamps[(seq(4, to=number.of.samples, by=4)),])
  sample.times$start.time <- floor_date(sample.times$start.time, "minute")
  sample.times$end.time <- ceiling_date(sample.times$end.time, "minute")
  
  # Data is labeled the same as the scan data itself to facilitate ease of access 
  if (!(is.null(raw.sparklink.file))) {
    # Retrieves the sample names from the sparklink file, assuming a specific format.
    sample.names <- as.data.frame(raw.sparklink.file[,3], stringsAsFactors = FALSE)
    
    # Gets the difference in sample columns, in the case that the amount of samples and the number of 
    # sample labels are consistent (which has been the case previously)
    sample.size.difference <- nrow(sample.times) - nrow(sample.names)
    
    # Adds "unlabeled sample" + the unlabeled index to ensure that every column has a label
    if (nrow(sample.names) < nrow(sample.times)) {
      sample.names <- rbind(sample.names, 
                            c(paste0("unlabeled sample ", 
                                    nrow(sample.names):
                                    nrow(sample.names) + sample.size.difference)))
    }
    
    # Adds a column that contains the sample labels to the sample times
    sample.times$sample.name <- sample.names[,1]
    
  } else {
    # If no sparklink file was provided, then the sample names will be set to "sample 1, sample 2..." etc
    sample.times$sample.name <- c(paste0("sample ", 1:(nrow(sample.times))))
  }
  
  return(sample.times)
}

# Function for setting the column names of the graph data so the 
# data processing doesn't have to be run through again. 
# Assumed format is Sparklink labels format.
setGraphLabels <- function(graph.labels, graph.data) {
  names(graph.data) <- graph.labels[,1] # Is this even used? It's just one line and it's already run in the file processing itself
}

# Returns a data frame to be graphed from the given amplog file.
# Assumes a particular format for the amplog file. 
ampGraphData <- function(raw.amplog.file) {
  amp.graph.data <- raw.amplog.file %>% select(X0, X2)
  return(amp.graph.data)
}

# Returns a data frame that contains the time stamps and amperage data from the given amperage dataframe 
# within the given start time and end time, +/- 3 minutes.
intervalAmperageData <- function(amp.graph.data, start.time, end.time) {
  # Calculates the time interval which will represent the range of amperage data that will be graphed.
  # Time interval is set to +/- 3 minutes for improved graph readability and to catch times that are not an exact match.
  # i.e, cases where start time is 12:15:30, amperage time is 12:15:29
  selected.interval <- as.interval(start.time - (3 * 60), end.time + (3 * 60))
  
  # Selects the times from the data that are contained within the previously calculated time interval.
  selected.amp.times <- amp.graph.data[,1] %>% filter(amp.graph.data$X0 %within% selected.interval)
  
  # Joins the corresponding amperage data to the selected time stamps.
  selected.amp.data <- inner_join(selected.amp.times, amp.graph.data, by = "X0")
  return(selected.amp.data)
}



# Deletes every nth column in the given data frame, beginning from the 
# given starting index, i. 
nthDelete <- function(data.frame, n, i) {
  data.frame[,-(seq(i, to=ncol(data.frame), by=n))]
}
