current.graph.data <- graph.data[[42]]


graph.data <- scanGraphData(read.csv("data\\170622_Study114_AIM.csv", stringsAsFactors = FALSE, na.strings = c("", NA)))
# Assumes a format of the following: 
# sample 1: scan1, scan2, scan3, scan4, sample.diameters
# Returns a collection of scans that are measured to be significantly different
# from each other (if any), based on the trapezoidal approximation of the graph area.\
findDissimilarScan <- function(current.graph.data) {
  badScans <- c()
  
  # Removes all NA values from the data frame
  current.graph.data <- current.graph.data[complete.cases(current.graph.data),]
  
  # Calculates the area under the curves of the scan datasets with a trapezoidal approximation
  scan.areas <- lapply(names(current.graph.data[1:4]), 
                      function(x) { trapz(current.graph.data$sample.diameters, 
                                          as.numeric(unlist(current.graph.data[x]))) })
  # Calculates the mean of the 4 scan areas
  scan.area.mean <- mean(as.numeric(scan.areas), na.rm = FALSE)
  
  # Calculates the similarity of the scan areas to the scan mean 
  similarity.metrics <- lapply(seq_along(scan.areas), function(x) { findSimilarity(scan.areas[[x]], scan.area.mean) })
  
  # Converts the similarity metrics list to a data frame, with the metrics in one column (for ease of access)
  similarity.metrics <- do.call(rbind, lapply(similarity.metrics, data.frame, stringsAsFactors=FALSE))
  
  # Adds names to the similarity metrics dataframe so they can be accessed
  similarity.metrics$scan.names <- names(current.graph.data[1:4])
  
  # Records the bad scans, if there are any.
  for (i in 1:nrow(similarity.metrics)) {
    if (similarity.metrics$X..i..[i] < 0.90) {
      badScans <- c(similarity.metrics$scan.names[i], badScans)
    }
  }
  
  # If there are no bad scans, return a list containing "None" (assuming that the functions using this expect a list)
  # Otherwise, return the list of bad scans.
  if (is.null(badScans)) {
    return(c("None"))
  } else {
    return(badScans)
  }
}




# Computes the similarity between two numbers, and returns a number representing a metric
# that indicates the level of similarity, with 100 being a perfect match, and numbers closer
# to 0 being extremely dissimilar.
findSimilarity <- function(first.number, second.number) {
  return(min(first.number, second.number)/max(first.number, second.number))
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
