# Assumes a format of the following: 
# sample 1: scan1, scan2, scan3, scan4, sample.diameters
# Returns a collection of scans that are measured to be significantly different
# from each other (if any), based on the trapezoidal approximation of the graph area.\

# Test: Sample 10, 12, 16 17, 18, 42
# Possible thing to consider: Printing the similarity metric if the scan is found to be bad, 
# so Carissa and Jake can know that one is especially important
current.graph.data <- graph.data[[42]]
findDissimilarScan <- function(current.graph.data) {
  current.graph.data <- current.graph.data[complete.cases(current.graph.data),]
  
  # Steps:
  ## Compute the area beneath the graphs for each dataset.
  ## Compute the similarity 
  
  scan.area1 <- trapz(current.graph.data$sample.diameters, current.graph.data$scan1)
  scan.area2 <- trapz(current.graph.data$sample.diameters, current.graph.data$scan2)
  scan.area3 <- trapz(current.graph.data$sample.diameters, current.graph.data$scan3)
  scan.area4 <- trapz(current.graph.data$sample.diameters, current.graph.data$scan4)
  
  scan.area.mean <- (scan.area1 + scan.area2 + scan.area3 + scan.area4)/4
  
  similarity.metric <- min(scan.area4, scan.area.mean)/ max(scan.area4, scan.area.mean)
  
  
  
  min(scan.area1, scan.area2)/ max(scan.area1, scan.area2)
  min(scan.area1, scan.area4)/ max(scan.area1, scan.area4)
  return("None")
}

# Computes the similarity between two numbers, and returns a number representing a metric
# that indicates the level of similarity, with 100 being a perfect match, and numbers closer
# to 0 being extremely dissimilar.
findSimilarity <- function() {
  
}

# Assumed data input format is the returned format from scanGraphData(). 
# Generates the averaged dataset which contains the averaged values from the 4 visualized scans for each sample.
# Returns in the format of a dataframe.
getAverageScans <- function(graph.data, sparklink.file = NULL) {
  # Add the overall sample labels to the graph, which should have been generated already for the graph data
  avg.scans <-  rbind(c("Sample Label", names(graph.data)))

  # Remove any columns that contain NA values that may have resulted from given files having irregular sizes (i.e, 46 labels for 47 scans)
  avg.scans <- avg.scans[ , colSums(is.na(avg.scans)) == 0]

  # If a Sparklink file was provided, add sample name, diameters and the corresponding
  # inj labels.
  if (!is.null(sparklink.file)) {
    sample.names <- rbind(c("Sample Name", sparklink.file[,4]))
    sample.size.difference <- (length(graph.data) + 1) - length(sample.names)
    if (length(sample.names) < length(graph.data) + 1) {
      sample.names <- cbind(sample.names, 
                            c(paste0("unnamed sample ", 
                                     length(sample.names):
                                     length(sample.names))))
    }
    avg.scans <- avg.scans %>% 
      rbind(sample.names)
    avg.scans <- avg.scans %>%
      rbind(c("Diameter", paste0("inj", 1:(ncol(avg.scans) - 1))))
  } else {
    # Otherwise, only add the diameters and inj labels.
    avg.scans <- avg.scans %>%
      rbind(c("Diameter", paste0("inj", 1:(length(avg.scans) - 1))))
  }
  
  avg.scans <- as.data.frame(avg.scans, stringsAsFactors = FALSE)
    if (!is.null(graph.data)) {
      # Retrieves the diameters from the first set of sample data. The diameters should be the same for every dataset.
      avg.sample.data <- as.character(graph.data[[1]]$sample.diameters)
      graph.index <- 1
      # For loop bound is set to the size of avg.scans so unlabeled samples are not included
      for (i in 1:(ncol(avg.scans) - 1)) {
        current.sample <- graph.data[[graph.index]][,1:4]
        current.sample.means <- rowMeans(current.sample)
        avg.sample.data <- avg.sample.data %>%
          cbind(as.character(current.sample.means))
        graph.index <- graph.index + 1
      }
      avg.sample.data <- as.data.frame(avg.sample.data, stringsAsFactors = FALSE)
      names(avg.sample.data) <- names(avg.scans)
      avg.scans <- bind_rows(avg.scans, avg.sample.data)
    }
  avg.scans <- avg.scans[complete.cases(avg.scans),]
  return(avg.scans)
}
